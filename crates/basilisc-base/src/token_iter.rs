use core::num::NonZeroU8;
use std::{iter::FusedIterator, fmt};

/// An interator optimised for yielding a BBC BASIC token.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct TokenIter {
	a: Option<NonZeroU8>,
	b: Option<NonZeroU8>,
}

impl TokenIter {
	/// Creates a new `TokenIter` for a direct (1-byte) token.
	pub const fn new_direct(value: NonZeroU8) -> Self {
		Self {
			a: Some(value),
			b: None,
		}
	}

	/// Creates a new `TokenIter` for an indirect (2-byte) token.
	pub const fn new_indirect(prefix: NonZeroU8, value: NonZeroU8) -> Self {
		Self {
			a: Some(prefix),
			b: Some(value),
		}
	}

	/// Returns the number of bytes yet to be yielded in the iterator.
	pub fn len(&self) -> u8 {
		match (self.a, self.b) {
			(Some(_), Some(_)) => 2,
			(Some(_), None) => 1,
			_ => 0,
		}
	}

	/// Peeks the next byte that this iterator will yield.
	pub fn peek_first(&self) -> u8 {
		self.a.map(NonZeroU8::get).unwrap_or(0)
	}
}

impl fmt::Debug for TokenIter {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("TokenIter[")?;
		match (self.a, self.b) {
			(Some(a), Some(b)) => write!(f, "{:02x}, {:02x}", a, b),
			(Some(a), None) => write!(f, "{:02x}", a),
			(None, _) => Ok(()),
		}?;
		f.write_str("]")
	}
}

impl Iterator for TokenIter {
	type Item = NonZeroU8;

	fn next(&mut self) -> Option<Self::Item> {
		core::mem::replace(&mut self.a, self.b.take())
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		let len = <Self as ExactSizeIterator>::len(self);
		(len, Some(len))
	}
}

impl FusedIterator for TokenIter { }

impl ExactSizeIterator for TokenIter {
	fn len(&self) -> usize {
		Self::len(self) as usize
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn direct() {
		let mut direct = TokenIter::new_direct(NonZeroU8::new(0xab).unwrap());
		assert_eq!(NonZeroU8::new(0xab), direct.next());
		assert_eq!(None, direct.next());
	}

	#[test]
	fn indirect_c6() { test_indirect(0xc6) }

	#[test]
	fn indirect_c7() { test_indirect(0xc7) }

	#[test]
	fn indirect_c8() { test_indirect(0xc8) }

	fn test_indirect(prefix: u8) {
		let prefix = NonZeroU8::new(prefix).unwrap();
		let mut sut = TokenIter::new_indirect(prefix, NonZeroU8::new(0x9d).unwrap());
		assert_eq!(Some(prefix), sut.next());
		assert_eq!(NonZeroU8::new(0x9d), sut.next());
		assert_eq!(None, sut.next());
	}
}
