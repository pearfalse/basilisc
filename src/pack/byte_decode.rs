#![allow(clippy::unusual_byte_groupings)]
//! Encoding conversion in I/O objects.

use std::{
	io,
	ops::ControlFlow,
	mem::ManuallyDrop, num::NonZeroUsize,
};

use nonzero_ext::nonzero;

use crate::{
	latin1::CharExt,
	support::IoObject,
};

use super::ErrorKind;

/// Handles converting to/from UTF-8 and RISC OS Latin-1 in byte streams.
///
/// Some UTF-8 characters will map to RISC OS Latin-1 characters in the encode process, but
/// something needs to read the multiple bytes of said characters to know what to map in. This is
/// that something.
#[derive(Debug)]
pub(super) struct ByteDecoder<'a> {
	buf: *mut [u8], // heap storage
	drain: *const [u8], // pull off chars, same storage as `buf`
	src: IoObject<'a>,
	last_read_pos: u64,
}

impl<'a> ByteDecoder<'a> {
	pub const DEFAULT_CAPACITY: NonZeroUsize = nonzero!(1usize<<16);

	/// Constructs a new decoder from an I/O object with the default buffer capacity.
	#[inline]
	pub fn new(src: IoObject<'a>) -> Self {
		Self::with_capacity(src, Self::DEFAULT_CAPACITY)
	}

	/// Constructs a new decoder from an I/O object with a specific buffer capacity.
	///
	/// The buffer will always be at least 4 bytes in size.
	pub fn with_capacity(src: IoObject<'a>, capacity: NonZeroUsize) -> Self {
		// - ManuallyDrop ensures that the original vec doesn't get dropped immediately
		// - using vec![] ensures the buffer is initialised
		// - immediately converting to boxed slice ensures the buffer stays address-stable

		// buffer capacity must be at least 4
		let capacity = capacity.max(nonzero!(4usize));

		let mut buf = ManuallyDrop::new(vec![0u8; capacity.get()].into_boxed_slice());
		Self {
			buf: std::ptr::slice_from_raw_parts_mut(
				buf.as_mut_ptr(), capacity.get()),
			drain: std::ptr::slice_from_raw_parts(buf.as_ptr(), 0),
			src,
			last_read_pos: 0,
		}
	}

	/// Reads the next encoding-mapped byte from the source.
	pub fn read_next(&mut self) -> Result<Option<u8>, ErrorKind> {
		let ch: char = loop {
			if let Some(c) = self.try_next_char()? { break c; } // found a char

			match self.fill_buf()? {
				ControlFlow::Break(()) if self.drain_len() == 0
					=> return Ok(None), // file is done
				ControlFlow::Break(()) => return Err(self.utf8_error()),
				ControlFlow::Continue(()) => continue,
			}
		};

		ch.as_risc_os_latin1()
			.map(Option::Some)
			.ok_or_else(|| ErrorKind::NoLatin1Char(ch))
	}


	fn utf8_error(&self) -> ErrorKind {
    	ErrorKind::InvalidUtf8 { start_pos: self.last_read_pos }
	}

	fn try_next_char(&mut self) -> Result<Option<char>, ErrorKind> {
		let slice = unsafe { &*self.drain };
		let lead = match slice.first().copied() {
			Some(c) => c,
			None => return Ok(None),
		};

		let char8_len = match lead {
			0x00..=0x7f => 1u8, // ASCII
			0b110_00000..=0b110_11111 => 2,
			0b1110_0000..=0b1110_1111 => 3,
			// most Latin-1 conversions from the SMPs will fail, but we need the char for the error
			0b11110_000..=0b11110_111 => 4,
			_ => return Err(self.utf8_error())
		};

		let char8_slice = if char8_len as usize <= slice.len() {
			&slice[..(char8_len as usize)]
		} else {
			// more read data needed
			return Ok(None);
		};

		self.drain = &slice[(char8_len as usize)..] as *const [u8];
		std::str::from_utf8(char8_slice)
			.map_err(|_| ErrorKind::InvalidUtf8 { start_pos: self.last_read_pos })
			.map(|s| {
				self.last_read_pos += char8_len as u64;
				Some(s.chars().next().unwrap())
			})
	}

	fn fill_buf(&mut self) -> io::Result<ControlFlow<()>> {
		use std::ptr;

		let rem_len = self.drain_len();

		let empty_buf = unsafe {
			if rem_len > 0 {
				// move undrained elements to the front and offset `entire_buf`
				ptr::copy(
					self.drain as *const u8,
					self.buf as *mut u8,
					rem_len);
				let space_len = (*self.buf).len() - rem_len;
				ptr::slice_from_raw_parts_mut(
					(self.buf as *mut u8).add(rem_len),
					space_len)
			} else {
				self.buf
			}
		};


		// incorporate length of forwarded elements in this level
		let read_size = {
			let buf = unsafe {
				// SAFETY: values are always init'd
				&mut *empty_buf
			};
			let read_size = self.src.read(buf)?;
			debug_assert!(read_size <= buf.len());
			read_size
		};

		self.drain = std::ptr::slice_from_raw_parts(self.buf as *const u8, read_size + rem_len);

		Ok(match read_size {
			0 => ControlFlow::Break(()),
			_ => ControlFlow::Continue(()),
		})
	}

	#[inline(always)]
	fn drain_len(&self) -> usize {
		unsafe {
			// SAFETY: self.drain is always a valid slice
			(*self.drain).len()
		}
	}
}

impl<'a> Drop for ByteDecoder<'a> {
	fn drop(&mut self) {
		unsafe {
			// SAFETY: `buf` was always a boxed slice, and we're not touching `self.drain` anymore
			std::mem::drop(Box::from_raw(self.buf))
		}
	}
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn empty() {
		check(b"", b"", None);
	}

	#[test]
	fn simple() {
		check(b"one", b"one", None); // ASCII
	}

	#[test]
	fn non_ascii() {
		check("fianc\u{00e9}e".as_bytes(), b"fianc\xe9e", None); // non-ASCII
	}

	#[test]
	fn multi_iter_no_utf8_split() {
		// split into multiple iterations
		check(b"MULTIread", b"MULTIread", Some(3));
	}

	#[test]
	fn stupidly_small_buffer() {
		for size in 1..=3 {
			check("fun\u{00a9}tion \u{1fbc0}".as_bytes(), b"fun\xa9tion \x84", Some(size));
		}
	}

	#[test]
	fn multi_iter_utf8_split() {
		// 3 bytes a piece
		check("\u{2401}\u{2402}\u{2403}\u{2404}".as_bytes(), b"\x01\x02\x03\x04", Some(4));
	}

	#[test]
	fn catch_trailing_failures() {
		check_err(b"ABC\xe0", ErrorKind::InvalidUtf8 { start_pos: 3 }, None);
	}

	fn check(input: &[u8], output: &[u8], buf_size: Option<usize>) {
		use std::ops::Deref;
		let out_buf = check_impl(input, output.len(), buf_size);

		assert_hex::assert_eq_hex!(Ok(output), out_buf.as_ref().map(Deref::deref));
	}

	fn check_err(input: &[u8], output: ErrorKind, buf_size: Option<usize>) {
		let out_err = check_impl(input, 0, buf_size);
		assert_eq!(Err(output), out_err)
	}

	fn check_impl(input: &[u8], output_cap: usize, buf_size: Option<usize>)
	-> Result<Vec<u8>, ErrorKind> {
		let mut out_buf = Vec::with_capacity(output_cap);
		let mut input = io::Cursor::new(input);
		let mut sut = ByteDecoder::with_capacity(&mut input,
			NonZeroUsize::new(buf_size.unwrap_or(output_cap))
				.unwrap_or(ByteDecoder::DEFAULT_CAPACITY)
			);

		let result = loop {
			match sut.read_next() {
				Ok(Some(ch)) => { out_buf.push(ch); },
				Ok(None) => break Ok(()),
				Err(e) => break Err(e),
			};
		};

		result.map(|()| out_buf)
	}
}
