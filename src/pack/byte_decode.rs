use core::fmt;
use std::{
	io,
	ops::ControlFlow,
	mem::{MaybeUninit, ManuallyDrop},
};

use crate::{
	latin1::CharExt,
	support::IoObject,
};

pub(super) struct ByteDecoder<'a> {
	buf: MaybeUninit<*mut [u8]>, // heap storage
	drain: *const [u8], // pull off chars
	src: IoObject<'a>,
	last_read_pos: u64,
}

#[derive(Debug, thiserror::Error)]
pub(super) enum Error {
	#[error("invalid UTF-8 character at file position {start_pos}")]
	InvalidUtf8 { start_pos: u64 },
	#[error("character '{}' has no RISC OS Latin-1 equivalent", (.0).escape_unicode())]
	NoLatin1Char(char),
	#[error("I/O error: {0:?}")]
	IoError(#[from] io::Error),
}

impl PartialEq for Error {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::InvalidUtf8 { start_pos: a }, Self::InvalidUtf8 { start_pos: b })
				if a == b => true,
			(Self::NoLatin1Char(a), Self::NoLatin1Char(b))
				if a == b => true,
			_ => false,
		}
	}
}

type Utf8Match = ControlFlow<[u8; 4]>;

impl<'a> ByteDecoder<'a> {
	pub const DEFAULT_CAPACITY: usize = 1<<16;

	pub fn with_capacity(src: IoObject<'a>, capacity: usize) -> Self {
		let capacity = if capacity == 0 { Self::DEFAULT_CAPACITY } else { capacity };

		// - ManuallyDrop ensures that the original vec doesn't get dropped immediately
		// - using vec![] ensures the buffer is initialised
		// - immediately converting to boxed slice ensures the buffer stays address-stable
		let mut buf = ManuallyDrop::new(vec![0u8; capacity].into_boxed_slice());
		Self {
			buf: MaybeUninit::new(std::ptr::slice_from_raw_parts_mut(buf.as_mut_ptr(), capacity)),
			drain: std::ptr::slice_from_raw_parts(buf.as_ptr(), 0),
			src,
			last_read_pos: 0,
		}
	}

	pub fn read_next(&mut self) -> Result<Option<u8>, Error> {
		let ch: char = loop {
			match self.try_next_char()? {
				Some(c) => break c, // found a char
				None => {}, // multi-byte char splits at end of buffer, or buffer ends
			};

			match self.fill_buf()? {
				ControlFlow::Break(()) if self.drain_len() == 0
					=> return Ok(None), // file is done
				ControlFlow::Break(()) => return Err(self.utf8_error()),
				ControlFlow::Continue(()) => continue,
			}
		};

		ch.as_risc_os_latin1()
			.map(Option::Some)
			.ok_or_else(|| Error::NoLatin1Char(ch))
	}


	fn utf8_error(&self) -> Error {
    	Error::InvalidUtf8 { start_pos: self.last_read_pos }
	}

	fn try_next_char(&mut self) -> Result<Option<char>, Error> {
		let slice = unsafe { &*self.drain };
		let lead = match slice.first().copied() {
			Some(c) => c,
			None => return Ok(None),
		};

		let char8_len = match lead {
			0x00..=0x7f => 1u8, // ASCII
			0b110_00000..=0b110_11111 => 2,
			0b1110_0000..=0b1110_1111 => 3,
			// all Latin-1 conversions from the SMPs will fail, but we need the char for the error
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
			.map_err(|_| Error::InvalidUtf8 { start_pos: self.last_read_pos })
			.map(|s| {
				self.last_read_pos += char8_len as u64;
				Some(s.chars().next().unwrap())
			})
	}

	fn fill_buf(&mut self) -> io::Result<ControlFlow<()>> {
		use std::ptr;

		let entire_buf = unsafe {
			// SAFETY: always init'd, no one else keeps a mut ref around for this
			self.buf.assume_init_read()
		};
		let rem_len = self.drain_len();

		let empty_buf = unsafe {
			if rem_len > 0 {
				// move undrained elements to the front and offset `entire_buf`
				ptr::copy(
					self.drain as *const u8,
					entire_buf as *mut u8,
					rem_len);
				let space_len = (*entire_buf).len() - rem_len;
				ptr::slice_from_raw_parts_mut(
					(entire_buf as *mut u8).add(rem_len),
					space_len)
			} else {
				entire_buf
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

		self.drain = std::ptr::slice_from_raw_parts(entire_buf as *const u8, read_size + rem_len);

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
			// SAFETY: `buf` is a boxed slice, put behind MaybeUninit to prevent mut aliasing
			std::mem::drop(Box::from_raw(self.buf.assume_init_read()))
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
	fn multi_iter_utf8_split() {
		// 3 bytes a piece
		check("\u{2401}\u{2402}\u{2403}\u{2404}".as_bytes(), b"\x01\x02\x03\x04", Some(4));
	}

	#[test]
	fn catch_trailing_failures() {
		check_err(b"ABC\xe0", Error::InvalidUtf8 { start_pos: 3 }, None);
	}

	fn check(input: &[u8], output: &[u8], capacity: Option<usize>) {
		use std::ops::Deref;
		let out_buf = check_impl(input, output.len(), capacity);

		assert_hex::assert_eq_hex!(Ok(output), out_buf.as_ref().map(Deref::deref));
	}

	fn check_err(input: &[u8], output: Error, capacity: Option<usize>) {
		let out_err = check_impl(input, 0, capacity);
		assert_eq!(Err(output), out_err)
	}

	fn check_impl(input: &[u8], output_cap: usize, capacity: Option<usize>)
	-> Result<Vec<u8>, Error> {
		let mut out_buf = Vec::with_capacity(output_cap);
		let mut input = io::Cursor::new(input);
		let mut sut = ByteDecoder::with_capacity(&mut input, capacity.unwrap_or(output_cap));

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
