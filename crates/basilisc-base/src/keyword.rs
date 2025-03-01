//! (Raw) Keywords as used by `basilisc`, in its own serialised form.
#![allow(dead_code)] // neither build.rs nor bin crate will ever use 100% of methods here

use std::{fmt, num::NonZeroU8, mem};

use arrayvec::ArrayVec;
use ascii::{AsciiStr, AsAsciiStr, AsciiChar};

pub use crate::token_iter::TokenIter;

/// Length of the longest known keyword.
pub const MAX_LEN: u8 = 9;

/// The size of `RawKeyword` in bytes.
pub const STORE_SIZE: u8 = 12;

/// A BASIC keyword, serialised in its own compact format.
///
/// While it has no stability guarantees, `RawKeyword` aims to be compact, efficient, and well-
/// aligned for 32- and 64-bit CPUs. The \[size, alignment\] of this struct are currently \[12, 4\].
///
/// `Option<RawKeyword>` is statically guaranteed to have the same type size as its unwrapped
/// variant.
///
/// This struct defines its own ordering rules as:
///
/// - Lexical ordering of full keyword;
/// - Token position (any < rvalue-only < lvalue-only);
/// - Abbreviation length.
///
/// While this means that keywords cannot be stably sorted, no two keywords should ever be equal to
/// each other in the final data.
#[derive(Clone, Copy)]
#[repr(C, align(4))]
pub struct RawKeyword {
	/// Actual meaningful characters
	chars: [u8; MAX_LEN as usize],
	/// Flags (see the `flags` submodule)
	flags: u8,
	/// Token byte in its namespace.
	token_byte: NonZeroU8,
	/// Length of string (number of meaningful bytes in `chars`) and prefix
	len_and_prefix: NonZeroU8,
}

impl PartialEq for RawKeyword {
	fn eq(&self, other: &Self) -> bool {
		self.len_and_prefix == other.len_and_prefix
			&& self.flags == other.flags
			&& self.as_bytes() == other.as_bytes()
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

/// Namespace for flag bits.
pub mod flags {
	/// For keywords that may only match in lvalue position. Setting this alongside `RVALUE_ONLY`
	/// is a logic error.
	pub const LVALUE_ONLY         : u8 = 1<<7;

	/// For keywords that may only match in rvalue position. Setting this alongside `LVALUE_ONLY`
	/// is a logic error.
	pub const RVALUE_ONLY         : u8 = 1<<6;

	/// Marks a keyword as greedy (i.e. will match this token even if another potential keyword
	/// character immediately follows).
	pub const GREEDY              : u8 = 1<<5;

	/// Marks a keyword as directly preceding a line number reference (i.e. GOTO/GOSUB).
	pub const TRIGGERS_LINE_REF   : u8 = 1<<4;

	/// Marks the bytes reserved to hold the keyword minimum abbreviation length.
	pub const MIN_ABBREV_LEN_MASK : u8 = 0b1111;
}

/// Implementation detail for top bits in `RawKeyword::len_and_prefix`.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prefix {
	Direct = 0,
	C6 = 0x40,
	C7 = 0x80,
	C8 = 0xc0,
}

impl Prefix {
	const LOWER_BITS: u8 = 0b11_1111;

	pub fn byte(self) -> Option<NonZeroU8> {
		NonZeroU8::new(match self {
			Self::Direct => 0,
			Self::C6 => 0xc6,
			Self::C7 => 0xc7,
			Self::C8 => 0xc8,
		})
	}
}


/// Different positions a keyword has to be in to tokenise a particular way. Most tokens will use
/// [TokenPosition::Any].
///
/// The three values order themselves (smallest to largest) as: Any, Right, Left.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum TokenPosition {
	Any = 0,
	Left = 2,
	Right = 1,
}

static_assertions::assert_eq_size!([u8; STORE_SIZE as usize], RawKeyword, Option<RawKeyword>);

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
			core::slice::from_raw_parts(start, self.len().get() as usize)
		}
	}

	#[inline]
	/// Returns the length of this keyword.
	pub fn len(&self) -> NonZeroU8 {
		unsafe {
			// SAFETY: `len_and_prefix` is known to be non-zero in the lowest 6 bits
			NonZeroU8::new_unchecked(self.len_and_prefix.get() & Prefix::LOWER_BITS)
		}
	}

	/// Returns the keyword as an ASCII string slice.
	pub fn as_ascii_str(&self) -> &AsciiStr {
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
	/// - The keyword length in bytes (`n`) must be non-zero and set in `src[11]`, OR-ed with
	///   the u8 cast of the appropriate `Prefix` value.
	/// - The first `n` bytes of `src`, where `n` is the keyword length in bytes, must be printing
	///   ASCII characters, excluding space. The rest should be zero.
	/// - The minimum abbrev length in `src[9]` must be less than the string length (it may be zero).
	/// - The token byte in `src[10]` must be non-zero.
	pub const unsafe fn new_unchecked(src: [u8; STORE_SIZE as usize]) -> Self {
		let raw_len = src[STORE_SIZE as usize - 1] & Prefix::LOWER_BITS;
		debug_assert!(raw_len > 0 && raw_len <= MAX_LEN);
		unsafe {
			// SAFETY: caller promises to uphold documented safety invariants
			mem::transmute(src)
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

	/// Returns a `TokenIter` for this keyword's byte value.
	pub fn tokens(&self) -> TokenIter {
		let prefix: Prefix = unsafe {
			std::mem::transmute(self.len_and_prefix.get() & !Prefix::LOWER_BITS)
		};

		match prefix.byte() {
			None => TokenIter::new_direct(self.token_byte),
			Some(n) => TokenIter::new_indirect(n, self.token_byte)
		}
	}

	/// Returns the length of the keyword's minimum abbreviation.
	#[inline]
	pub const fn min_abbrev_len(&self) -> Option<NonZeroU8> {
		NonZeroU8::new(self.flags & flags::MIN_ABBREV_LEN_MASK)
	}

	/// Returns the keyword name, truncated to its minimum abbreviation (the trailing '.' is not
	/// included).
	#[inline]
	pub fn min_abbrev(&self) -> Option<&AsciiStr> {
		self.min_abbrev_len().map(|c| unsafe {
			// SAFETY: upheld by external constraints to `RawKeyword::new_unchecked`
			let bytes = ::core::slice::from_raw_parts(self.chars.as_ptr(), c.get() as usize);
			bytes.as_ascii_str_unchecked()
		})
	}

	/// Returns `true` if the token uses greedy matching.
	#[inline]
	pub fn is_greedy(&self) -> bool {
		self.flags & flags::GREEDY != 0
	}

	/// Returns `true` if the presence of this token indicates expectation of a line reference.
	#[inline]
	pub const fn triggers_line_ref(&self) -> bool {
		self.flags & flags::TRIGGERS_LINE_REF != 0
	}

	/// Iterates over the contents of the keyword.
	pub fn iter(&self) -> Iter<'_> {
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


/// An iterator over the characters of a keyword.
///
/// Logically, the yielded item is an [`AsciiChar`], but for maximum compatibility
/// with the crate's call sites, it yields them as [`u8`]s.
#[derive(Debug, Clone)]
pub struct Iter<'a> {
	keyword: &'a RawKeyword,
	pos: u8,
}

impl Iter<'static> {
	/// Returns a keyword iterator that yields no characters when iterated.
	pub fn empty() -> Self {
		static EMPTY: RawKeyword = unsafe {
			// SAFETY: this should match preconditions on RawKeyword::new_unchecked
			RawKeyword::new_unchecked([
				33, 0, 0,  0, 0, 0,  0, 0, 0, // chars
				0, // flags
				0xff, // byte value
				1, // length (must be non-zero) | prefix
			])
		};

		Self {
			keyword: &EMPTY,
			pos: 1,
		}
	}
}

impl<'a> Iter<'a> {
	/// Constructs a new iterator for a keyword.
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
			end @ 0 | end if end >= self.keyword.len().get() => None,
			good => Some(good),
		}.and_then(|idx| self.keyword.chars.get(idx as usize));

		if next_byte.is_some() {
			debug_assert!(self.pos < MAX_LEN);
			self.pos += 1;
		}

		next_byte.copied()
	}
}
