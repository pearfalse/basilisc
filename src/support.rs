use std::{io, convert::Infallible};

mod per_line_bits;
pub(crate) use per_line_bits::*;

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
		next_byte_io_read(self)
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

// include meta-src files that we want
#[path = "../meta-src/keyword.rs"] mod keyword;
#[path = "../meta-src/token_iter.rs"] mod token_iter;
#[path = "../meta-src/subarray.rs"] mod subarray;

pub(crate) use keyword::{RawKeyword, Keyword, IntoIter as KeywordIntoIter};
pub(crate) use token_iter::TokenIter;
pub(crate) use subarray::SubArray;
