use core::num::NonZeroU8;
use std::fmt;

/// An interator optimised for yielding a BBC BASIC token.
#[derive(Debug, Clone)]
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
}

/// A wrapper for `TokenIter` whose [`Display`][1] impl will output Rust code to construct the
/// inner value. Designed for build scripts only.
/// [1]: https://doc.rust-lang.org/stable/std/fmt/trait.Display.html
pub(crate) struct Codegen(TokenIter);

impl From<TokenIter> for Codegen {
	fn from(src: TokenIter) -> Self {
		Self(src)
	}
}

impl fmt::Display for Codegen {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fn get_num(field: Option<NonZeroU8>) -> Result<NZu8Codegen, fmt::Error> {
			field.map(NZu8Codegen).ok_or(fmt::Error)
		}

		let is_indirect = self.0.b.is_some();
		write!(f, "TokenIter::{}(", if is_indirect {
			stringify!(new_indirect)
		} else {
			stringify!(new_direct)
		})?;

		if is_indirect {
			write!(f, "{}, {}",
				get_num(self.0.a)?, get_num(self.0.b)?)?;
		} else {
			write!(f, "{}", get_num(self.0.a)?)?;
		}

		write!(f, ")")
	}
}

/// A support struct for [`Codegen`], handling a statically verified call to
/// [`new_unchecked`][1].
///
/// [1]: https://doc.rust-lang.org/stable/core/num/struct.NonZeroU8.html#method.new_unchecked
struct NZu8Codegen(NonZeroU8);

impl fmt::Display for NZu8Codegen {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "unsafe {{ \
/* SAFETY: value generated and checked at build time */ \
NonZeroU8::new_unchecked({}) }}",
			self.0)
	}
}

impl Iterator for TokenIter {
	type Item = NonZeroU8;

	fn next(&mut self) -> Option<Self::Item> {
		core::mem::replace(&mut self.a, self.b.take())
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
