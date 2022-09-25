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
#[path ="../meta-src"]
mod meta_src {
	mod keyword;
	pub(crate) use keyword::Keyword;
	
	mod token_iter;
	pub(crate) use token_iter::TokenIter;
}
pub(crate) use meta_src::*;
