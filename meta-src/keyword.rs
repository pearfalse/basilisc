#![allow(dead_code)] // neither build.rs nor bin crate will ever use 100% of methods here

use std::{num::NonZeroU8, convert::TryFrom, mem};

use ascii::AsciiStr;

pub const MAX_LEN: u8 = 9;
pub const STORE_SIZE: u8 = 12;

/// The raw storage for a keyword.
pub(crate) type RawKeyword = [u8; STORE_SIZE as usize];

/// A BASIC keyword, accessed through some backing store.
///
/// This type provides interfaces to BASIC keywords that are referenced from some fixed-size
/// backing store, which `Keyword` may own or borrow.
#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(C)]
pub(crate) struct Keyword {
	/// Actual meaningful characters
	chars: [u8; MAX_LEN as usize],
	/// Unused padding byte (should be 0)
	_padding: u8,
	/// Iteration indexing state
	iter_state: u8,
	/// Length of string (number of meaningful bytes in `chars`)
	len: NonZeroU8,
}

#[allow(dead_code)]
#[doc(hidden)]
fn _assert_struct_size() {
	static_assertions::assert_eq_size!(RawKeyword, Keyword, Option<Keyword>);
}

impl Keyword {
	/// Creates a new `Keyword` from a RawKeyword. The array must consist of the following:
	///
	/// - `1..=MAX_LEN` printable ASCII characters at the start;
	/// - the string length in the last byte (index `MAX_LEN`).
	///
	/// This function returns `None` if any of these conditions are not met.
	pub(crate) fn try_new(mut src: RawKeyword) -> Result<Self, RawKeyword> {
		let _slice = NonZeroU8::new(src[STORE_SIZE as usize - 1])
			// length must be non-zero, but also small enough to be meaningful
			.filter(|l| l.get() <= MAX_LEN)
			// length is good, get the byte slice and let's have a look
			.map(|len| &src[..len.get() as usize])
			// all bytes must be 0x21..=0x7f
			.filter(|s| s.iter().all(|b| (0x20..=0x7f).contains(b)))
			.ok_or_else(|| src)?;

		// checks pass, we can create this now
		src[STORE_SIZE as usize - 2] = 0; // iteration state
		Ok(unsafe {
			// SAFETY: we have just validated all required soundness conditions
			Self::new_unchecked(src)
		})
	}

	/// Returns populated bytes of a keyword.
	///
	/// There are two methods with this name, this one being specialised for borrowing `Keyword`
	/// instances.
	pub(crate) fn as_bytes(&self) -> &[u8] {
		let slice: &[u8] = self.chars.as_slice();
		unsafe {
			// SAFETY: we don't safely allow construction of byte slices that don't fit
			core::slice::from_raw_parts(slice.as_ptr(), self.len.get() as usize)
		}
	}

	/// Returns the keyword as an ASCII string slice.
	///
	/// There are two methods with this name, this one being specialised for borrowing `Keyword`
	/// instances.
	pub(crate) fn as_ascii_str(&self) -> &AsciiStr {
		unsafe {
			// SAFETY: slice is guaranteed by constructor checks to be ASCII
			AsciiStr::from_ascii_unchecked(self.as_bytes())
		}
	}

	/// Constructs a `Keyword` without verifying the inner contents.
	///
	/// # Safety
	///
	/// All of the following conditions must be met:
	/// - The underlying data that spans the string must consist of non-NULL ASCII bytes only.
	/// - The final byte (index `STORE_SIZE - 1`) must be a non-zero value strictly less than or
	/// equal to `MAX_LEN`.
	/// - The byte at index `STORE_SIZE - 2` must be 0.
	pub(crate) const unsafe fn new_unchecked(src: RawKeyword) -> Self {
		let r = mem::transmute::<RawKeyword, Self>(src);
		r.assert_iter_state();
		r
	}

	/// Returns a copy of the raw keyword store.
	pub(crate) fn as_array(&self) -> RawKeyword {
		unsafe {
			// safety: RawKeyword has strictly looser invariants than Keyword
			mem::transmute(self.clone())
		}
	}

	/// Moves the keyword into an iterator over its bytes.
	pub(crate) fn into_iter(self) -> IntoIter {
		self.assert_iter_state();
		IntoIter(self)
	}

	/// Iterates over the contents of the keyword.
	///
	/// This method just clones `self`, and moves the clone into an iterator, hence
	/// the maybe unexpected return type.
	pub(crate) fn iter(&self) -> IntoIter {
		self.clone().into_iter()
	}

	#[inline(always)]
	const fn assert_iter_state(&self) {
		debug_assert!(self.iter_state == 0);
	}
}


impl PartialOrd for Keyword {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for Keyword {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		self.as_bytes().cmp(other.as_bytes())
	}
}


impl IntoIterator for Keyword {
	type IntoIter = IntoIter;
	type Item = u8;

	fn into_iter(self) -> Self::IntoIter {
		Self::into_iter(self)
	}
}

impl<'a> IntoIterator for &'a Keyword {
	type IntoIter = IntoIter;
	type Item = u8;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}


#[derive(Debug, Clone)]
pub(crate) struct IntoIter(Keyword);

impl IntoIter {
	pub(crate) const fn empty() -> Self {
		let mut empty_keyword = unsafe {
			// SAFETY: this mirrors an empty iteration
			Keyword::new_unchecked([
				33, 0, 0,  0, 0, 0,  0, 0, 0, // chars
				0, // padding
				0, // iteration state (kept blank as a hack for assertions in new_unchecked)
				1, // length (matches iteration state, must be non-zero)
			])
		};
		empty_keyword.iter_state = empty_keyword.len.get();
		Self(empty_keyword)
	}
}

impl Iterator for IntoIter {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		let next_byte = match self.0.iter_state {
			end @ 0 | end if end >= self.0.len.get() => None,
			good => Some(good),
		}.and_then(|idx| self.0.chars.get(idx as usize));

		if next_byte.is_some() {
			debug_assert!(self.0.iter_state < MAX_LEN);
			self.0.iter_state += 1;
		}

		next_byte.copied()
	}
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct KeywordFromStrError<'a>(pub &'a str);

impl<'a> TryFrom<&'a str> for Keyword {
	type Error = KeywordFromStrError<'a>;

	fn try_from(value: &'a str) -> Result<Self, Self::Error> {
		let error = move || KeywordFromStrError(value);

		let slice = value.as_bytes();
		let slice_len = match u8::try_from(slice.len()) {
			Ok(0) | Err(_) => return Err(error()),
			Ok(e) => e,
		};

		let mut arr = RawKeyword::default();
		arr[..slice.len()].copy_from_slice(slice);
		arr[STORE_SIZE as usize - 1] = slice_len;
		Keyword::try_new(arr).map_err(|_| error())
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn new(s: &str) -> Keyword {
		Keyword::try_from(s).unwrap()
	}

	#[test]
	fn as_ascii_str() {
		let k = new("ASCIIstr");
		assert_eq!(AsciiStr::from_ascii(b"ASCIIstr"), Ok(k.as_ascii_str()));
	}

	#[test]
	fn as_array() {
		let k = new("ABCDEFGHI");
		assert_eq!([65,66,67,68,69,70,71,72,73,0,0,9], k.as_array());
	}
	
	#[test]
	fn try_new() {
		assert!(Keyword::try_new([40,40,40,0,0,0,0,0,0,0,0,3]).is_ok());
		assert!(Keyword::try_new(RawKeyword::default()).is_err());
	}

	#[test]
	#[should_panic]
	fn fail_too_long() {
		let _ = new("ASCIIstr??");
	}

	#[test]
	#[should_panic]
	fn fail_not_ascii() {
		let _ = new("1234\u{4e94}");
	}

	#[test]
	#[should_panic]
	#[allow(non_snake_case)]
	fn fail_C0() {
		let _ = new("keywo\rd");
	}

	#[test]
	fn into_iter() {
		for case in ["ABCDEF", "1", "123456789"] {
			let output: arrayvec::ArrayVec<u8, {MAX_LEN as usize}>
				= new(case).into_iter().collect();
			assert_eq!(case.as_bytes(), &*output);
		}
	}
}