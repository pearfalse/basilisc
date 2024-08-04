//! Cooked keywords are high-level representations of BASIC keyword data, used by the build script
//! to generate raw keyword data. See [`Keyword`] for more.

use std::num::NonZeroU8;

use ascii::{AsciiStr, AsAsciiStr as _};

use crate::token_iter::TokenIter;
use crate::keyword::{self, Prefix, RawKeyword, TokenPosition};


/// An error in keyword construction, if any constraints are not met.
///
/// The actual error cause is only relevant to humans, not to other code.
#[derive(Debug)]
pub(crate) struct KeywordCtorError(&'static str);


/// Represents a BASIC keyword at a high level. `build.rs` uses this to hold its own representation
/// of all BASIC keywords `basilisc` needs to know about, and uses it to construct the
/// [`RawKeyword`](keyword::RawKeyword) arrays that `basilisc` will refer to at runtime.
///
/// A keyword's data is:
///
/// - A representable form in 2–9 ASCII characters.
/// - A minimum abbreviation (optional).
/// - The byte it tokenises to.
/// - Which position it sits in (some property-style keywords tokenise to different bytes depending
///   on if they are used as lvalues or not).
/// - Whether or not the token is greedy (some keywords will be tokenised on match without needing
///   a trailing space or punctuation mark to denote its end).
///
/// For the sake of the lookup algorithm in the final crate, this struct defines its own ordering
/// rules as:
///
/// - Lexical ordering of full keyword;
/// - Token position;
/// - Abbreviation length.
///
/// While this means that keywords cannot be stably sorted, no two keywords should ever be equal to
/// each other in the final data.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Keyword {
	byte: NonZeroU8,
	keyword: &'static AsciiStr,
	min_abbrev: Option<NonZeroU8>,
	position: TokenPosition,
	greedy: bool,
	prefix: Prefix,
}

impl Keyword {
	/// This function returns `Err([KeywordCtorError])` if any of these conditions are not met:
	///
	/// - The keyword must contain printable ASCII bytes only[^1].
	/// - The keyword must be between 1 and 9 characters in length.
	/// - If `min_abbrev` is populated, it must be between 1 and 7.
	///
	/// [^1]: `basilisc`'s tokeniser has more stringent requirements than this; this is just a small
	/// step above what is required for memory safety.
	pub fn try_new(
		byte: u8,
		keyword: &'static str,
		min_abbrev: Option<NonZeroU8>,
		position: TokenPosition,
		greedy: bool,
		prefix: Prefix,
	) -> Result<Self, KeywordCtorError> {
		// byte must be high
		if ! (0x7f..=0xff).contains(&byte) {
			return Err(KeywordCtorError("token is not valid"));
		}
		let byte = NonZeroU8::new(byte).unwrap();

		// length must be non-zero, but also small enough to be meaningful
		if ! (2..=keyword::MAX_LEN as usize).contains(&keyword.len()) {
			return Err(KeywordCtorError(match keyword.len() {
				0 => "string is empty",
				_ => "string is too long"
			}));
		}

		// min_abbrev must be correct
		if min_abbrev.map(|abbr| abbr.get() as usize >= keyword.len().min(7)) == Some(true) {
			return Err(KeywordCtorError("abbreviation length is too long"));
		}

		// all bytes must be 0x21..=0x7e
		if ! keyword.as_bytes().iter().all(|b| (0x21..=0x7e).contains(b)) {
			return Err(KeywordCtorError("String contains invalid bytes"));
		}

		let keyword = unsafe { keyword.as_ascii_str_unchecked() };
		Ok(Self {
			byte, keyword, position, min_abbrev, greedy, prefix
		})
	}

	/// Returns the keyword as the packed array format that [RawKeyword] uses
	/// internally.
	pub fn as_array(&self) -> [u8; keyword::STORE_SIZE as usize] {
		const S: usize = keyword::STORE_SIZE as usize;
		let mut store = [0u8; S];
		store[0..self.keyword.len()].copy_from_slice(self.keyword.as_bytes());

		// S - 1 := length | prefix bits
		store[S - 1] = self.keyword.len() as u8 | self.prefix as u8;

		// S - 2 := unprefixed byte
		store[S - 2] = self.byte.get();

		// S - 3 := flags
		let flags = &mut store[S - 3];
		if let Some(abbr) = self.min_abbrev {
			debug_assert!(abbr.get() <= 7);
			*flags |= abbr.get();
		}

		*flags |= {
			use keyword::{TokenPosition::*, flags};
			(match self.position {
				Any => 0,
				Left => flags::LVALUE_ONLY,
				Right => flags::RVALUE_ONLY,
			}) | (match self.greedy {
				true => flags::GREEDY,
				false => 0,
			})
		};

		store
	}

	/// Gets the token byte for this keyword.
	#[inline(always)]
    pub(crate) fn byte(&self) -> NonZeroU8 {
        self.byte
    }

    /// Gets an ASCII string slice for this keyword's representation.
    #[inline(always)]
    pub(crate) fn keyword(&self) -> &AsciiStr {
        self.keyword
    }

    /// Gets the minimum abbreviation length for this keyword.
    #[inline(always)]
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn min_abbrev(&self) -> Option<NonZeroU8> {
        self.min_abbrev
    }

    /// Gets the position dependency for this keyword.
    #[inline(always)]
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn position(&self) -> TokenPosition {
        self.position
    }
}

impl PartialOrd for Keyword {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(<Keyword as Ord>::cmp(self, other))
	}
}

impl Ord for Keyword {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		fn abbr(src: &Keyword) -> u8 {
			src.min_abbrev.map(NonZeroU8::get).unwrap_or(0)
		}

		// compare keyword, greedy, pos, abbr (not byte, that's output data)
		self.keyword().cmp(other.keyword())
			.then_with(|| self.greedy.cmp(&other.greedy)) // TODO: necessary?
			.then_with(|| self.position.cmp(&other.position))
			.then_with(|| abbr(self).cmp(&abbr(other)))
	}
}

impl From<Keyword> for RawKeyword {
	fn from(kw: Keyword) -> Self {
		unsafe {
			// SAFETY: invariants are handled by `Keyword::as_array`
			// array already contains prefix bits, so OR in nothing additional
			Self::new_unchecked(kw.as_array())
		}
	}
}


#[cfg(test)]
mod tests {
	use super::*;
	use crate::keyword::TokenPosition;

	#[test]
	fn byte() {
		for b in [0x98, 0xca, 0xff, 0x7f, 0xbdu8] {
			let k = Keyword::try_new(b, "WORD", None, TokenPosition::Any, false, Prefix::Direct)
				.unwrap();
			assert_eq!(b, k.byte().get());
		}
	}

	#[test]
	fn as_ascii_str() {
		let k = Keyword::try_new(255, "ASCIIstr", None, keyword::TokenPosition::Any, false, Prefix::Direct)
			.unwrap();
		assert_eq!(AsciiStr::from_ascii(b"ASCIIstr").unwrap(), k.keyword());
	}

	#[test]
	fn as_array() {
		let k = Keyword::try_new(255, "ABCDEFGHI", None, keyword::TokenPosition::Any, false, Prefix::Direct)
			.unwrap();
		assert_eq!([65,66,67,68,69,70,71,72,73,0,255,9], k.as_array());
	}

	#[test]
	fn flags() {
		const KEYWORD: &'static str = "KEYWORD";
		for pos in [
			TokenPosition::Any,
			TokenPosition::Left,
			TokenPosition::Right,
		] {
			for abbrev_len in 0u8..(KEYWORD.len() as u8 - 1) {
				let abbrev_len = NonZeroU8::new(abbrev_len);
				let kw = Keyword::try_new(255, KEYWORD, abbrev_len, pos, false, Prefix::Direct)
					.unwrap();
				assert_eq!(pos, kw.position());
				assert_eq!(abbrev_len, kw.min_abbrev());
			}
		}
	}

	#[test]
	fn fail_too_long() {
		let _ = Keyword::try_new(155, "ASCIIstr??", None, TokenPosition::Any, false, Prefix::Direct)
			.unwrap_err();
	}

	#[test]
	fn fail_not_ascii() {
		let _ = Keyword::try_new(155, "1234\u{4e94}", None, TokenPosition::Any, false, Prefix::Direct)
			.unwrap_err();
	}

	#[test]
	#[allow(non_snake_case)]
	fn fail_C0() {
		let _ = Keyword::try_new(155, "keywo\rd", None, TokenPosition::Any, false, Prefix::Direct)
			.unwrap_err();
	}
}

