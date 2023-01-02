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
}

#[derive(Debug, thiserror::Error)]
pub(super) enum Error {
	#[error("invalid UTF-8 character at file position {start_pos}")]
	InvalidUtf8 { start_pos: u64 },
	#[error("character '{0}' has no RISC OS Latin-1 equivalent")]
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
		// - ManuallyDrop ensures that the original vec doesn't get dropped immediately
		// - using vec![] ensures the buffer is initialised
		// - immediately converting to boxed slice ensures the buffer stays address-stable
		let mut buf = ManuallyDrop::new(vec![0u8; capacity].into_boxed_slice());
		Self {
			buf: MaybeUninit::new(std::ptr::slice_from_raw_parts_mut(buf.as_mut_ptr(), capacity)),
			drain: std::ptr::slice_from_raw_parts(buf.as_ptr(), 0),
			src,
		}
	}

	pub fn read_next(&mut self) -> Result<Option<u8>, Error> {
		let ch: char = loop {
			match self.try_next_char() {
				Some(c) => break c, // found a char
				None => {},
			};

			match self.fill_buf()? {
				ControlFlow::Break(()) => return Ok(None), // file is done
				ControlFlow::Continue(()) => continue,
			}
		};

		ch.as_risc_os_latin1()
			.map(Option::Some)
			.ok_or_else(|| Error::NoLatin1Char(ch))
	}


	fn try_next_char(&mut self) -> Option<char> {
		None
	}

	fn fill_buf(&mut self) -> io::Result<ControlFlow<()>> {
		debug_assert!(unsafe {&*self.drain}.is_empty());

		let buf = unsafe {
			// SAFETY: always init'd, no one else keeps a mut ref around for this
			self.buf.assume_init_read()
		};


		let read_size = {
			let buf = unsafe {
				// SAFETY: values are always init'd
				&mut *buf
			};
			let read_size = self.src.read(buf)?;
			debug_assert!(read_size <= buf.len());
			read_size
		};

		self.drain = std::ptr::slice_from_raw_parts(buf as *const u8, read_size);

		Ok(match read_size {
			0 => ControlFlow::Break(()),
			_ => ControlFlow::Continue(()),
		})
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
		check(b"fianc\xc3\xa9e", b"fianc\xe9e", None); // non-ASCII
	}

	fn check(input: &[u8], output: &[u8], capacity: Option<usize>) {
		let mut out_buf = Vec::with_capacity(output.len());
		let mut input = io::Cursor::new(input);
		let mut sut = ByteDecoder::with_capacity(&mut input, capacity.unwrap_or(output.len()));

		let result = loop {
			match sut.read_next() {
				Ok(Some(ch)) => { out_buf.push(ch); },
				Ok(None) => break Ok(()),
				Err(e) => break Err(e),
			};
		};

		assert_hex::assert_eq_hex!(Ok(output), result.map(|()| &*out_buf));
	}
}
