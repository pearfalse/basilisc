use core::num::NonZeroU8;
use std::{iter::FusedIterator, fmt};

/// An interator optimised for yielding a BBC BASIC token.
#[derive(Clone, PartialEq, Eq, Default)]
pub(crate) struct TokenIter {
	a: Option<NonZeroU8>,
	b: Option<NonZeroU8>,
}

impl TokenIter {
	/// Creates a new `TokenIter` for a direct (1-byte) token.
	pub(crate) const fn new_direct(value: NonZeroU8) -> Self {
		Self {
			a: Some(value),
			b: None,
		}
	}

	/// Creates a new `TokenIter` for an indirect (2-byte) token.
	pub(crate) const fn new_indirect(prefix: NonZeroU8, value: NonZeroU8) -> Self {
		Self {
			a: Some(prefix),
			b: Some(value),
		}
	}

	pub(crate) fn to_byte_slice<'stor>(&'_ self, storage: &'stor mut [u8; 2]) -> &'stor mut [u8] {
		match (self.a, self.b) {
			(Some(a), Some(b)) => {
				storage[0] = a.get();
				storage[1] = b.get();
				&mut storage[..]
			},
			(Some(a), None) => {
				storage[0] = a.get();
				&mut storage[..1]
			},
			(None, _) => &mut []
		}
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

/// A wrapper for `TokenIter` whose [`Display`][1] impl will output Rust code to construct the
/// inner value. Designed for build scripts only.
///
/// [1]: https://doc.rust-lang.org/stable/std/fmt/trait.Display.html
pub(crate) struct Codegen {
	iter: TokenIter,
	add_unsafe_blocks: bool,
}

impl Codegen {
	fn get_num(&self, field: Option<NonZeroU8>) -> Result<NZu8Codegen, fmt::Error> {
		field.map(|n| NZu8Codegen {
			value: n,
			add_unsafe_block: self.add_unsafe_blocks
		}).ok_or(fmt::Error)
	}

	#[allow(dead_code)] // not really dead code, it's used in build script
	pub(crate) fn from(iter: TokenIter, add_unsafe_blocks: bool) -> Self {
		Self { iter, add_unsafe_blocks }
	}
}

impl fmt::Display for Codegen {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

		let is_indirect = self.iter.b.is_some();
		write!(f, "TokenIter::{}(", if is_indirect {
			stringify!(new_indirect)
		} else {
			stringify!(new_direct)
		})?;

		if is_indirect {
			write!(f, "{}, {}",
				self.get_num(self.iter.a)?, self.get_num(self.iter.b)?)?;
		} else {
			write!(f, "{}", self.get_num(self.iter.a)?)?;
		}

		write!(f, ")")
	}
}

/// A support struct for [`Codegen`], handling a statically verified call to
/// [`new_unchecked`][1].
///
/// [1]: https://doc.rust-lang.org/stable/core/num/struct.NonZeroU8.html#method.new_unchecked
struct NZu8Codegen {
	value: NonZeroU8,
	add_unsafe_block: bool,
}

impl fmt::Display for NZu8Codegen {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if self.add_unsafe_block {
			f.write_str("unsafe { /* SAFETY: value generated and checked at build time */ ")?;
		}
		write!(f, "NonZeroU8::new_unchecked({})", self.value)?;
		if self.add_unsafe_block {
			f.write_str(" }")?;
		}

		Ok(())
	}
}

impl Iterator for TokenIter {
	type Item = NonZeroU8;

	fn next(&mut self) -> Option<Self::Item> {
		core::mem::replace(&mut self.a, self.b.take())
	}
}

impl FusedIterator for TokenIter { }

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
