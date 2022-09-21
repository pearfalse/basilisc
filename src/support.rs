use core::convert::Infallible;
use std::io;

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


#[derive(Debug)]
pub(crate) struct PerLineBits {
	store: Box<[u8; Self::BYTE_COUNT]>,
}

impl PerLineBits {
	const BYTE_COUNT: usize = 0xff00 >> 3;

	pub fn new() -> Self {
		Self {
			store: Box::new([0u8; Self::BYTE_COUNT])
		}
	}

	pub fn try_get(&self, index: u16) -> Option<bool> {
		let (byte_idx, bit_mask) = Self::decompose(index);
		self.store.get(byte_idx as usize).map(|r| *r & bit_mask != 0)
	}

	pub fn try_get_mut<'a>(&'a mut self, index: u16) -> Option<BitRefMut<'a>> {
		let (byte_idx, bit_mask) = Self::decompose(index);
		self.store.get_mut(byte_idx as usize).map(|rm| BitRefMut {
			r#ref: rm,
			bit_mask,
		})
	}


	pub fn get(&self, index: u16) -> bool {
		self.try_get(index).expect("index out of range")
	}

	pub fn get_mut<'a>(&'a mut self, index: u16) -> BitRefMut<'a> {
		self.try_get_mut(index).expect("index out of range")
	}


	fn decompose(index: u16) -> (u16, u8) {
		debug_assert!(Self::BYTE_COUNT < u16::MAX as usize);

		((index >> 3), 1 << (index & 0b111) as u8)
	}
}


#[derive(Debug)]
pub(crate) struct BitRefMut<'a> {
	r#ref: &'a mut u8,
	bit_mask: u8,
}

impl<'a> BitRefMut<'a> {
	#[inline]
	pub fn bit(&self) -> bool {
		(*self.r#ref & self.bit_mask) != 0
	}

	#[inline]
	pub fn set(&mut self) {
		*self.r#ref |= self.bit_mask;
	}

	#[inline]
	pub fn clear(&mut self) {
		*self.r#ref &= !self.bit_mask;
	}

	#[inline]
	pub fn set_to(&mut self, value: bool) {
		if value { self.set() } else { self.clear() }
	}
}

#[cfg(test)]
mod test_per_line_bits {
	use super::PerLineBits;

	#[test]
	fn get() {
		let mut sut = PerLineBits::new();
		assert_eq!(Some(false), sut.try_get(0));
		sut.store[0] = 1;
		assert_eq!(Some(true), sut.try_get(0));

		assert_eq!(Some(false), sut.try_get(7));
		sut.store[0] = 0x80;
		assert_eq!(Some(true), sut.try_get(7));

		assert_eq!(false, sut.get(9));
		sut.store[1] = 2;
		assert_eq!(true, sut.get(9));

		assert!(sut.try_get(0xff00).is_none());
		assert!(sut.try_get(u16::MAX).is_none());
	}

	#[test]
	fn get_mut() {
		let mut sut = PerLineBits::new();

		assert_eq!(0, sut.store[0]);
		let mut r#ref = sut.try_get_mut(3).unwrap();
		assert!(! r#ref.bit());
		r#ref.set();
		assert_eq!(1u8<<3, sut.store[0]);

		sut.store[0x20] = 0b1111_1111;
		sut.get_mut(0x104).clear();
		assert_eq!(0b1110_1111, sut.store[0x20]);

		sut.store[2] = 0b0000_0010;
		sut.get_mut(16).set_to(true);
		sut.get_mut(17).set_to(false);
		assert_eq!(0b0000_0001, sut.store[2]);

		assert!(sut.try_get_mut(0xff00).is_none());
		assert!(sut.try_get_mut(u16::MAX).is_none());
	}
}
