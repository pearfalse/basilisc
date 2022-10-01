use std::io;

mod per_line_bits;
pub(crate) use per_line_bits::*;

pub trait NextByte {
	type Error;
	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error>;
}

impl<I: io::Read> NextByte for I {
	type Error = io::Error;

	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error> {
		let mut tgt = [0u8];
		match io::Read::read(self, &mut tgt) {
			Ok(0) => Ok(None),
			Ok(_) => Ok(Some(tgt[0])),
			Err(e) => Err(e),
		}
	}
}

// include meta-src files that we want
#[path = "../meta-src/keyword.rs"] mod keyword;
#[path = "../meta-src/token_iter.rs"] mod token_iter;
#[path = "../meta-src/subarray.rs"] mod subarray;

pub(crate) use keyword::{RawKeyword, Keyword};
pub(crate) use token_iter::TokenIter;
pub(crate) use subarray::SubArray;
