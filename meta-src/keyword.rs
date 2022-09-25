#![allow(dead_code)] // neither build.rs nor bin crate will ever use 100% of methods here

use std::borrow::Borrow;

use ascii::AsciiStr;

const MAX_LEN: usize = 10;
pub(crate) type RawKeyword = [u8; MAX_LEN];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Keyword<S: Borrow<RawKeyword>>(S);

impl Keyword<RawKeyword> {
	pub(crate) fn new(src: &str) -> Self {
		let src = src.as_bytes();
		let slice = Some(src).filter(|&src| src.iter().all(|&b| b >= 0x20))
			.expect("string contains ASCII control codes");

		let _ = AsciiStr::from_ascii(slice)
			.expect("string is not ASCII");

		Self({
			let mut a = [0u8; MAX_LEN];
			a[..slice.len()].copy_from_slice(slice);
			a
		})
	}
}

impl<'a> Keyword<&'a RawKeyword> {
	pub(crate) fn as_bytes<'s>(&'s self) -> &'a [u8] where 'a: 's {
		let mut slice = self.0.as_slice();

		while let Some((0, x)) = slice.split_last() {
			slice = x;
		}
		slice
	}

	pub(crate) fn as_ascii_str(&self) -> &AsciiStr {
		unsafe {
			// SAFETY: we don't allow construction of non-ASCII byte slices
			AsciiStr::from_ascii_unchecked(self.as_bytes())
		}
	}
}

impl Keyword<RawKeyword> {
	pub(crate) fn as_bytes(&self) -> &[u8] {
		let mut slice = self.0.as_slice();

		while let Some((0, x)) = slice.split_last() {
			slice = x;
		}
		slice
	}

	pub(crate) fn as_ascii_str(&self) -> &AsciiStr {
		unsafe {
			// SAFETY: we don't allow construction of non-ASCII byte slices
			AsciiStr::from_ascii_unchecked(self.as_bytes())
		}
	}
}

impl<S: Borrow<RawKeyword>> Keyword<S> {
	pub(crate) const unsafe fn new_unchecked(src: S) -> Self {
		Self(src)
	}

	pub(crate) fn try_new(src: S) -> Option<Self> {
		if src.borrow()[0] != 0 {
			Some(Self(src))
		} else {
			None
		}
	}

	pub(crate) fn as_array(&self) -> RawKeyword { *self.0.borrow() }
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn as_ascii_str() {
		let k = Keyword::new("ASCIIstr");
		assert_eq!(AsciiStr::from_ascii(b"ASCIIstr"), Ok(k.as_ascii_str()));
	}

	#[test]
	fn as_array() {
		let k = Keyword::new("ABCDEFGH");
		assert_eq!([65,66,67,68,69,70,71,72,0,0], k.as_array());
	}

	#[test]
	#[should_panic]
	fn fail_too_long() {
		let _ = Keyword::new("ASCIIstring");
	}

	#[test]
	#[should_panic]
	fn fail_not_ascii() {
		let _ = Keyword::new("1234\u{4e94}");
	}

	#[test]
	#[should_panic]
	#[allow(non_snake_case)]
	fn fail_C0() {
		let _ = Keyword::new("keywo\rd");
	}
}