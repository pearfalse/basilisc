#![allow(dead_code)] // neither build.rs nor bin crate will ever use 100% of methods here

use std::{fmt, num::NonZeroU8, mem};

use ascii::AsciiStr;

pub const MAX_LEN: u8 = 9;
pub const STORE_SIZE: u8 = 12;

/// A BASIC keyword, accessed through some backing store.
///
/// This type provides interfaces to BASIC keywords that are referenced from some fixed-size
/// backing store, which `Keyword` may own or borrow.
#[derive(Clone, Copy)]
#[repr(C)]
pub(crate) struct RawKeyword {
	/// Actual meaningful characters
	chars: [u8; MAX_LEN as usize],
	/// Flags
	///
	/// - b7: only if as an lvalue
	/// - b6 to b4: unused, must be 0
	/// - b3 to b0: minimum abbreviation length (if 0, no abbrev)
	flags: u8,
	/// Iteration indexing state
	iter_state: u8,
	/// Length of string (number of meaningful bytes in `chars`)
	len: NonZeroU8,
}

impl PartialEq for RawKeyword {
	fn eq(&self, other: &Self) -> bool {
		self.assert_iter_state();
		other.assert_iter_state();
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
	pub const MIN_ABBREV_LEN_MASK : u8 = 0b1111;

	pub const RESERVED: u8 = !(LVALUE_ONLY | RVALUE_ONLY | MIN_ABBREV_LEN_MASK);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub(crate) enum TokenPosition {
	Any,
	Left,
	Right,
}

#[allow(dead_code)]
#[doc(hidden)]
fn _assert_struct_size() {
	static_assertions::assert_eq_size!([u8; STORE_SIZE as usize], RawKeyword, Option<RawKeyword>);
}

impl fmt::Debug for RawKeyword {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "RawKeyword(\"{}\")", self.as_ascii_str()) // TODO: add flags
	}
}

impl RawKeyword {
	/// Returns populated bytes of a keyword.
	#[inline]
	pub fn as_bytes(&self) -> &[u8] {
		let slice: &[u8] = self.chars.as_slice();
		unsafe {
			// SAFETY: we don't safely allow construction of byte slices that don't fit
			core::slice::from_raw_parts(slice.as_ptr(), self.len.get() as usize)
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
	/// All preconditions of values specified in [`Self::new`] must pass.
	pub(crate) const unsafe fn new_unchecked(src: [u8; STORE_SIZE as usize]) -> Self {
		debug_assert!(src[STORE_SIZE as usize - 1] != 0);
		let r: Self = mem::transmute(src);
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
	pub fn min_abbrev(&self) -> Option<NonZeroU8> {
		NonZeroU8::new(self.flags & flags::MIN_ABBREV_LEN_MASK)
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

impl IntoIterator for RawKeyword {
	type IntoIter = IntoIter;
	type Item = u8;

	fn into_iter(self) -> Self::IntoIter {
		Self::into_iter(self)
	}
}

impl<'a> IntoIterator for &'a RawKeyword {
	type IntoIter = IntoIter;
	type Item = u8;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}


#[derive(Debug, Clone)]
pub(crate) struct IntoIter(RawKeyword);

impl IntoIter {
	pub(crate) const fn empty() -> Self {
		let mut empty_keyword = unsafe {
			// SAFETY: this mirrors an empty iteration
			RawKeyword::new_unchecked([
				33, 0, 0,  0, 0, 0,  0, 0, 0, // chars
				0, // flags
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
