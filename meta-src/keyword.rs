#![allow(dead_code)] // neither build.rs nor bin crate will ever use 100% of methods here

use std::borrow::Borrow;

use ascii::AsciiStr;

const MAX_LEN: usize = 12;

/// The raw storage for a keyword.
pub(crate) type RawKeyword = [u8; MAX_LEN];

/// A BASIC keyword, accessed through some backing store.
///
/// This type provides interfaces to BASIC keywords that are referenced from some fixed-size
/// backing store, which `Keyword` may own or borrow.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Keyword<S: Borrow<RawKeyword>>(S);

impl Keyword<RawKeyword> {
	/// Creates a new `Keyword` from a Rust string. Only intended for use from build scripts.
	///
	/// # Panics
	///
	/// This function panics if the string contains ASCII control codes, non-ASCII characters,
	/// or does not fit into a [`RawKeyword`](struct.RawKeyword.html).
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
	/// Returns populated bytes of a keyword.
	///
	/// There are two methods with this name, this one being specialised for borrowing `Keyword`
	/// instances.
	pub(crate) fn as_bytes<'s>(&'s self) -> &'a [u8] where 'a: 's {
		let mut slice = self.0.as_slice();

		while let Some((0, x)) = slice.split_last() {
			slice = x;
		}
		slice
	}

	/// Returns the keyword as an ASCII string slice.
	///
	/// There are two methods with this name, this one being specialised for borrowing `Keyword`
	/// instances.
	pub(crate) fn as_ascii_str(&self) -> &AsciiStr {
		unsafe {
			// SAFETY: we don't allow construction of non-ASCII byte slices
			AsciiStr::from_ascii_unchecked(self.as_bytes())
		}
	}
}

impl Keyword<RawKeyword> {
	/// Returns populated bytes of a keyword.
	///
	/// There are two methods with this name, this one being specialised for `Keyword`
	/// instances that own their raw data.
	pub(crate) fn as_bytes(&self) -> &[u8] {
		let mut slice = self.0.as_slice();

		while let Some((0, x)) = slice.split_last() {
			slice = x;
		}
		slice
	}

	/// Returns the keyword as an ASCII string slice.
	///
	/// There are two methods with this name, this one being specialised for `Keyword`
	/// instances that own their raw data.
	pub(crate) fn as_ascii_str(&self) -> &AsciiStr {
		unsafe {
			// SAFETY: we don't allow construction of non-ASCII byte slices
			AsciiStr::from_ascii_unchecked(self.as_bytes())
		}
	}
}

impl<S: Borrow<RawKeyword>> Keyword<S> {
	/// Constructs a `Keyword` without verifying the inner contents.
	///
	/// # Safety
	///
	/// The underlying data must consist of ASCII bytes only. There are additional restrictions
	/// to avoid logic errors (see [`try_new`](#try_new)), but this is the only restriction for
	/// memory safety.
	pub(crate) const unsafe fn new_unchecked(src: S) -> Self {
		Self(src)
	}

	/// Attempts contruction of a `Keyword` with the raw data given.
	///
	/// The underlying data is expected to consist of printable, non-space ASCII bytes only. The
	/// exception to this rule is `NUL` bytes, which may appear anywhere (although `NUL` bytes
	/// before printing ASCII characters likely indicates a logic error).
	pub(crate) fn try_new(src: S) -> Option<Self> {
		let slice: &[u8] = src.borrow().as_slice();

		if slice[0] == 0 || slice.iter().any(|&b| b >= 0x7f || (b <= 0x20 && b != 0)) {
			None
		} else {
			Some(Self(src))
		}
	}

	/// Returns a copy of the raw keyword store.
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
		let k = Keyword::new("ABCDEFGHIJ");
		assert_eq!([65,66,67,68,69,70,71,72,73,74,0,0], k.as_array());
	}
	
	#[test]
	fn try_new() {
		assert!(Keyword::try_new([40,40,40,0,0,0,0,0,0,0,0,0]).is_some());
		assert!(Keyword::try_new(RawKeyword::default()).is_none());
	}

	#[test]
	#[should_panic]
	fn fail_too_long() {
		let _ = Keyword::new("ASCIIstring??");
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