use std::num::NonZeroU8;

use ascii::{AsciiStr, AsAsciiStr as _};

use crate::keyword;

#[derive(Debug)]
pub(crate) struct KeywordCtorError(&'static str);

#[allow(non_snake_case)]
pub(crate) mod Match {
	#[allow(non_upper_case_globals)]
	pub const Nongreedy: bool = false;
	#[allow(non_upper_case_globals)]
	pub const Greedy: bool = true;
}


#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Keyword {
	byte: NonZeroU8,
	keyword: &'static AsciiStr,
	min_abbrev: Option<NonZeroU8>,
	position: keyword::TokenPosition,
	greedy: bool,
}

impl Keyword {
	/// This function returns `Err(KeywordCtorError)` if any of these conditions are not met.
	pub fn try_new(
		byte: u8,
		keyword: &'static str,
		min_abbrev: Option<NonZeroU8>,
		position: keyword::TokenPosition,
		greedy: bool,
	) -> Result<Self, KeywordCtorError> {
		// byte must be high
		if ! (0x7f..=0xff).contains(&byte) {
			return Err(KeywordCtorError("token is not valid"));
		}
		let byte = NonZeroU8::new(byte).unwrap();

		// length must be non-zero, but also small enough to be meaningful
		if ! (1..=keyword::MAX_LEN as usize).contains(&keyword.len()) {
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
			byte, keyword, position, min_abbrev, greedy
		})
	}

	pub fn as_array(&self) -> [u8; keyword::STORE_SIZE as usize] {
		const S: usize = keyword::STORE_SIZE as usize;
		let mut store = [0u8; S];
		store[0..self.keyword.len()].copy_from_slice(self.keyword.as_bytes());
		store[S - 1] = self.keyword.len() as u8;
		// S - 2 is iteration statel keep at 0
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
			}) | match self.greedy {
				true => flags::GREEDY,
				false => 0,
			}
		};

		store
	}

	#[inline(always)]
    pub(crate) fn byte(&self) -> NonZeroU8 {
        self.byte
    }

    #[inline(always)]
    pub(crate) fn keyword(&self) -> &AsciiStr {
        self.keyword
    }

    #[inline(always)]
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn min_abbrev(&self) -> Option<NonZeroU8> {
        self.min_abbrev
    }

    #[inline(always)]
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn position(&self) -> keyword::TokenPosition {
        self.position
    }
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::keyword::TokenPosition;

	#[test]
	fn byte() {
		for b in [0x98, 0xca, 0xff, 0x7f, 0xbdu8] {
			let k = Keyword::try_new(b, "WORD", None, TokenPosition::Any, false)
				.unwrap();
			assert_eq!(b, k.byte().get());
		}
	}

	#[test]
	fn as_ascii_str() {
		let k = Keyword::try_new(255, "ASCIIstr", None, keyword::TokenPosition::Any, false)
			.unwrap();
		assert_eq!(AsciiStr::from_ascii(b"ASCIIstr").unwrap(), k.keyword());
	}

	#[test]
	fn as_array() {
		let k = Keyword::try_new(255, "ABCDEFGHI", None, keyword::TokenPosition::Any, false)
			.unwrap();
		assert_eq!([65,66,67,68,69,70,71,72,73,0,0,9], k.as_array());
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
				let kw = Keyword::try_new(255, KEYWORD, abbrev_len, pos, false)
					.unwrap();
				assert_eq!(pos, kw.position());
				assert_eq!(abbrev_len, kw.min_abbrev());
			}
		}
	}

	#[test]
	fn fail_too_long() {
		let _ = Keyword::try_new(155, "ASCIIstr??", None, TokenPosition::Any, false)
			.unwrap_err();
	}

	#[test]
	fn fail_not_ascii() {
		let _ = Keyword::try_new(155, "1234\u{4e94}", None, TokenPosition::Any, false)
			.unwrap_err();
	}

	#[test]
	#[allow(non_snake_case)]
	fn fail_C0() {
		let _ = Keyword::try_new(155, "keywo\rd", None, TokenPosition::Any, false)
			.unwrap_err();
	}
}

