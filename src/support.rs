//! Miscellaneous support code.

use std::{fmt::{self, Debug}, io, convert::Infallible, fmt::Write};

mod per_line_bits;
pub(crate) use per_line_bits::*;

mod arrayvec_ext;
pub(crate) use arrayvec_ext::ArrayVecExt;

/// A thin trait to express [`IoObject`].
pub trait Readable: io::Read + Debug {}
impl<T: io::Read + Debug> Readable for T {}

/// A convience alias for I/O dyn traits used throughout `basc`.
pub type IoObject<'a> = &'a mut (dyn Readable + 'a);

/// A trait for easily fetching data from an I/O source, one byte at a time.
///
/// It is automatically implemented on [`dyn io::Read`](io::Read);
pub trait NextByte {
	type Error;
	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error>;
}

impl<'a> NextByte for dyn io::Read + 'a {
	type Error = io::Error;

	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error> {
		next_byte_io_read(self)
	}
}

impl<'a, 'b> NextByte for &'a mut (dyn io::Read + 'b) {
	type Error = io::Error;

	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error> {
		next_byte_io_read(*self)
	}
}

fn next_byte_io_read<I: io::Read + ?Sized>(src: &mut I) -> Result<Option<u8>, io::Error> {
	let mut tgt = [0u8];
	match io::Read::read(src, &mut tgt) {
		Ok(0) => Ok(None),
		Ok(_) => Ok(Some(tgt[0])),
		Err(e) => Err(e),
	}
}

impl<'a> NextByte for std::slice::Iter<'a, u8> {
	type Error = Infallible;

	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error> {
		Ok(self.next().copied())
	}
}


pub(crate) struct HexArray<'a>(pub &'a [u8]);

impl<'a> fmt::Debug for HexArray<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_char('[')?;
		let mut show_comma = false;
		for &b in self.0 {
			if show_comma {
				f.write_str(", ")?;
			}
			show_comma = true;
			write!(f, "{:02x}", b)?;
		}
		f.write_char(']')
	}
}

impl<'a> fmt::Display for HexArray<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		<Self as fmt::Debug>::fmt(self, f)
	}
}
