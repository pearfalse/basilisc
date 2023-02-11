#![allow(dead_code)] // neither build.rs nor bin crate will ever use 100% of methods here

use std::{fmt, num::NonZeroU8, mem};

use arrayvec::ArrayVec;
use ascii::{AsciiStr, AsAsciiStr, AsciiChar};

pub const MAX_LEN: u8 = 9;
pub const STORE_SIZE: u8 = 12;

/// A BASIC keyword, accessed through some backing store.
///
/// This type provides interfaces to BASIC keywords that are referenced from some fixed-size
/// backing store, which `Keyword` may own or borrow.
#[derive(Clone, Copy)]
#[repr(C)]
#[repr(align(4))]
pub(crate) struct RawKeyword {
	/// Actual meaningful characters
	chars: [u8; MAX_LEN as usize],
	/// Flags
	///
	/// - b7: only if as an lvalue
	/// - b6 to b4: unused, must be 0
	/// - b3 to b0: minimum abbreviation length (if 0, no abbrev)
	flags: u8,
	/// Unused
	_reserved: u8,
	/// Length of string (number of meaningful bytes in `chars`)
	len: NonZeroU8,
}

impl PartialEq for RawKeyword {
	fn eq(&self, other: &Self) -> bool {
		Self::eq_guts(self, other)
	}
}

impl Eq for RawKeyword {}

impl PartialOrd for RawKeyword {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(<Self as Ord>::cmp(self, other))
	}
}

impl Ord for RawKeyword {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		use std::cmp::Ordering;

		macro_rules! sort_this {
			($op:expr) => {
				match (&$op(self)).cmp(&$op(other)) {
					Ordering::Equal => {},
					other => return other,
				};
			};
		}

		sort_this!(RawKeyword::as_bytes);
		sort_this!(|k: &RawKeyword| k.flags & (flags::LVALUE_ONLY | flags::RVALUE_ONLY));

		// at this point, there should be no other meaningful changes!
		debug_assert!(self.min_abbrev() == other.min_abbrev());
		Ordering::Equal
	}
}

pub(crate) mod flags {
	pub const LVALUE_ONLY         : u8 = 1<<7;
	pub const RVALUE_ONLY         : u8 = 1<<6;
	pub const GREEDY              : u8 = 1<<5;
	pub const MIN_ABBREV_LEN_MASK : u8 = 0b1111;

	pub const RESERVED: u8 = !(LVALUE_ONLY | RVALUE_ONLY | GREEDY | MIN_ABBREV_LEN_MASK);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub(crate) enum TokenPosition {
	Any = 0,
	Left = 2,
	Right = 1,
}

#[allow(dead_code)]
#[doc(hidden)]
fn _assert_struct_size() {
	static_assertions::assert_eq_size!([u8; STORE_SIZE as usize], RawKeyword, Option<RawKeyword>);
}

impl fmt::Debug for RawKeyword {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "RawKeyword(\"{}\"", self.as_ascii_str())?;
		if let Some(abbr) = self.min_abbrev_len().map(NonZeroU8::get) {
			write!(f, ", abbr {}", abbr)?;
		}

		let mut flag_chars = ArrayVec::<AsciiChar, 2>::new();
		match self.position() {
			TokenPosition::Left  => flag_chars.push(AsciiChar::L),
			TokenPosition::Right => flag_chars.push(AsciiChar::R),
			_ => {},
		};
		flag_chars.push(if self.is_greedy() {AsciiChar::Exclamation} else {AsciiChar::Question});

		write!(f, " [{}])", <&AsciiStr>::from(&*flag_chars))
	}
}

impl RawKeyword {
	/// Returns populated bytes of a keyword.
	#[inline]
	pub fn as_bytes(&self) -> &[u8] {
		let start: *const u8 = self.chars.as_ptr();
		unsafe {
			// SAFETY: we don't safely allow construction of byte slices that don't fit
			core::slice::from_raw_parts(start, self.len.get() as usize)
		}
	}

	fn eq_guts(a: &Self, b: &Self) -> bool {
		a.len == b.len && a.flags == b.flags && a.as_bytes() == b.as_bytes()
	}

	#[inline]
	/// Returns the length of this keyword.
	pub(crate) fn len(&self) -> NonZeroU8 { self.len }

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

	/// Constructs a `RawKeyword` without verifying the inner contents.
	///
	/// # Safety
	///
	/// The following preconditions must match:
	///
	/// - The first `n` bytes of `src`, where `n` is the keyword length in bytes, must be printing
	///   ASCII characters, excluding space;
	/// - The `src[10]` must not set any bits in `flags::RESERVED`;
	/// - The minimum abbrev length in `src[10]` must be less than the string length or zero;
	/// - `src[11]` is reserved and must be zero.
	///
	/// Violating any of the above conditions may (but is not guaranteed to) violate memory safety.
	pub(crate) const unsafe fn new_unchecked(src: [u8; STORE_SIZE as usize]) -> Self {
		let raw_len = src[STORE_SIZE as usize - 1];
		debug_assert!(raw_len > 0 && raw_len <= MAX_LEN);
		mem::transmute(src)
	}

	/// Returns a copy of the raw keyword store.
	pub(crate) fn as_array(&self) -> RawKeyword {
		unsafe {
			// safety: RawKeyword has strictly looser invariants than Keyword
			mem::transmute(self.clone())
		}
	}

	/// Indicates whether this keyword only tokenises as an lvalue or rvalue.
	pub fn position(&self) -> TokenPosition {
		let lvalue = self.flags & (flags::LVALUE_ONLY) != 0;
		let rvalue = self.flags & (flags::RVALUE_ONLY) != 0;

		match (lvalue, rvalue) {
			(true, false) => TokenPosition::Left,
			(false, true) => TokenPosition::Right,
			(false, false) => TokenPosition::Any,
			(true, true) => unreachable!("both LVALUE_ONLY and RVALUE_ONLY set!"),
		}
	}

	#[inline]
	pub const fn min_abbrev_len(&self) -> Option<NonZeroU8> {
		NonZeroU8::new(self.flags & flags::MIN_ABBREV_LEN_MASK)
	}

	#[inline]
	pub fn min_abbrev(&self) -> Option<&AsciiStr> {
		self.min_abbrev_len().map(|c| unsafe {
			// SAFETY: upheld by external constraints to `RawKeyword::new_unchecked`
			let bytes = ::core::slice::from_raw_parts(self.chars.as_ptr(), c.get() as usize);
			bytes.as_ascii_str_unchecked()
		})
	}

	#[inline]
	pub fn min_abbrev_bytes(&self) -> Option<&[u8]> {
		self.min_abbrev().map(AsciiStr::as_bytes)
	}

	#[inline]
	pub fn is_greedy(&self) -> bool {
		self.flags & flags::GREEDY != 0
	}

	/// Iterates over the contents of the keyword.
	///
	/// This method just clones `self`, and moves the clone into an iterator, hence
	/// the maybe unexpected return type.
	pub(crate) fn iter<'a>(&'a self) -> Iter<'a> {
		Iter::new(self)
	}
}

impl<'a> IntoIterator for &'a RawKeyword {
	type IntoIter = Iter<'a>;
	type Item = u8;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}


#[derive(Debug, Clone)]
pub(crate) struct Iter<'a> {
	keyword: &'a RawKeyword,
	pos: u8,
}

static EMPTY: RawKeyword = unsafe {
	// SAFETY: this should match preconditions on RawKeyword::new_unchecked
	RawKeyword::new_unchecked([
		33, 0, 0,  0, 0, 0,  0, 0, 0, // chars
		0, // flags
		0, // reserved
		1, // length (must be non-zero)
	])
};

impl Iter<'static> {
	pub(crate) fn empty() -> Self {
		Self {
			keyword: &EMPTY,
			pos: 1,
		}
	}
}

impl<'a> Iter<'a> {
	fn new(keyword: &'a RawKeyword) -> Self {
		Self {
			keyword,
			pos: 0,
		}
	}
}

impl<'a> Iterator for Iter<'a> {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		let next_byte = match self.pos {
			end @ 0 | end if end >= self.keyword.len.get() => None,
			good => Some(good),
		}.and_then(|idx| self.keyword.chars.get(idx as usize));

		if next_byte.is_some() {
			debug_assert!(self.pos < MAX_LEN);
			self.pos += 1;
		}

		next_byte.copied()
	}
}


#[cfg(test)]
mod tests {
	use super::*;
	use crate::cooked_keyword::*;

	fn uncook(
		byte: u8, word: &'static str, min_abbrev: Option<NonZeroU8>,
		pos: TokenPosition, greedy: bool,
	) -> RawKeyword {
		let kw = Keyword::try_new(byte, word, min_abbrev, pos, greedy).unwrap();
		unsafe {
			RawKeyword::new_unchecked(kw.as_array())
		}
	}

	#[test]
	fn min_abbrev() {
		let kw = uncook(0x80, "LONG", NonZeroU8::new(2), TokenPosition::Any, false);

		assert_eq!(Some(AsciiStr::from_ascii(b"LO").unwrap()), kw.min_abbrev());
	}

	#[test]
	fn min_abbrev_real() {
		assert_eq!(Some(b"P".as_slice()),
			crate::token_data::TOKEN_MAP_DIRECT[0xf1]
			.unwrap().min_abbrev_bytes());
	}

	#[test]
	fn token_iter() {
		assert_eq!(b"PRINT", &*crate::token_data::TOKEN_MAP_DIRECT[0xf1]
			.unwrap().iter()
			.collect::<ArrayVec<u8, {MAX_LEN as usize}>>());
	}
}
