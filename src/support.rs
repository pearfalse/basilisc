use core::convert::Infallible;
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

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct InMemoryBytes<I: Iterator<Item = u8>>(pub I);

impl<I: Iterator<Item = u8>> NextByte for InMemoryBytes<I> {
	type Error = Infallible;

	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error> {
		Ok(self.0.next())
	}
}
