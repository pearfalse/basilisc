//! Handles &FFB-to-text conversion.

use core::convert::Infallible;
use std::{cell::Cell, fmt, io, mem};

use crate::{
	support::{ArrayVecExt, NextByte, PerLineBits},
	token_data,
	line_numbers,
	common::StringState,
};

use basilisc_base::keyword;

use arrayvec::ArrayVec;
use thiserror::Error;


/// Represents a line of BASIC in the original file.
///
/// Line contents are always heap-allocated.
#[derive(Debug, PartialEq, Eq)]
pub struct Line {
	pub line_number: u16,
	pub data: Box<[u8]>,
}

impl Line {
	/// Constructs a new line from its constituent parts.
	pub fn new(line_number: u16, data: Box<[u8]>) -> Self {
		Self { line_number, data }
	}
}


/// Contains all errors that `basilisc` could encounter when unpacking a file.
#[derive(Debug, Error)]
pub enum ErrorKind {
	/// The input file ended mid-line, or without seeing the designated EOF marker
	#[error("unexpected end of file")]
	UnexpectedEof,

	/// Did not encounter the expected line start byte
	#[error("expected 0Dh byte to start a new line, got {:02x} instead", .0)]
	UnexpectedByte(u8),

	/// I/O error occurred
	#[error("io error: {0}")]
	IoError(io::Error),

	/// Encountered invalid line number reference (65281 or higher)
	#[error("invalid line number reference (must be less than 62580)")]
	InvalidLineReference,
}

/// Indicates an error when unpacking a tokenised BASIC file.
#[derive(Debug, PartialEq)]
pub struct Error {
	line_number: u16,
	pub kind: ErrorKind,
}

impl Error {
	/// The (logical) line number at which the error occurred.
	pub fn line_number(&self) -> Option<u16> {
		if self.line_number < crate::line_numbers::LIMIT {
			Some(self.line_number)
		} else {
			None
		}
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Display::fmt(&self.kind, f)?;
		if self.line_number < crate::line_numbers::LIMIT {
			write!(f, " at line {}", self.line_number)
		} else {
			Ok(())
		}
	}
}

impl From<ErrorKind> for Error {
	/// Converts an `ErrorKind` into an `Error`, giving up on having line number information.
	fn from(kind: ErrorKind) -> Self {
		Self { line_number: u16::MAX, kind }
	}
}


impl From<io::Error> for ErrorKind {
	fn from(e: io::Error) -> Self {
		Self::IoError(e)
	}
}

impl From<line_numbers::DecodeError> for ErrorKind {
	fn from(_: line_numbers::DecodeError) -> Self {
		Self::InvalidLineReference
	}
}

impl From<Infallible> for ErrorKind {
	fn from(e: Infallible) -> Self {
		match e {}
	}
}

impl PartialEq for ErrorKind {
	fn eq(&self, other: &Self) -> bool {
		use ErrorKind::*;
		match (self, other) {
			(&UnexpectedEof, &UnexpectedEof) => true,
			(&InvalidLineReference, &InvalidLineReference) => true,
			(&UnexpectedByte(a), &UnexpectedByte(b)) => a == b,
			_ => false,
		}
	}
}

type Result<T> = std::result::Result<T, Error>;
type KindResult<T> = std::result::Result<T, ErrorKind>;
type TokenLookup = Option<u8>;
type ByteFlush = ArrayVec<u8, 5>; // decoded line number reference is the biggest
type LineNumberStage = ArrayVec<u8, 2>; // don't need to stage the third byte

/// State machine for parsing a line header.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum LineState {
	/// Not currently in a line
	#[default]
	HaveNothing,
	/// The `0D` to mark a new line (or half the EOF marker) has been parsed
	Have0D,
	/// We have half the line number (upper byte)
	HaveHalfLineNumber(u8),
	/// We have the full line number
	HaveFullLineNumber(u16),
	/// We have the line number and length, or are already parsing its content
	InLine {
		line_number: u16,
		remaining_bytes: u8,
	},
}


/// Handles the operation of decoding a BBC BASIC file.
#[derive(Debug)]
pub struct Parser<I: NextByte> {
	/// The inner byte I/O object
	inner: Peekable<I>,
	/// Buffer used for parsing, to minimise reallocations between lines
	buffer: Vec<u8>,
	/// The state of parsing metdata about the current line
	line_state: LineState,
	/// Handles line references after GOTO/GOSUB statements
	line_ref: Option<LineNumberStage>,
	/// Buffer to fill the 1–2 bytes of a BBC BASIC token
	token_lookup: TokenLookup,
	/// Buffer for potential lookups that didn't match anything, or for format hacks
	byte_flush: ByteFlush,
	/// Iterator for remaining characters of a previous lookup match
	cur_token: KeywordIter2<'static>,
	/// Have we reached the end of the BASIC file?
	have_reached_end: bool,
	/// Are we in a string literal? (if so, don't expand tokens)
	string_state: StringState,
	/// Which lines are referenced?
	referenced_lines: PerLineBits,
}

impl<I: NextByte> Parser<I>
where I: NextByte, ErrorKind: From<<I as NextByte>::Error> {
	/// Constructs a new parser based on an I/O object.
	pub fn new(inner: I) -> Self {
		Self {
			inner: Peekable::new(inner),
			buffer: Vec::with_capacity(256), // TODO how big can a line be?
			line_state: LineState::default(),
			line_ref: None,
			token_lookup: None,
			byte_flush: ArrayVec::new_const(),
			cur_token: KeywordIter2::default(),
			have_reached_end: false,
			string_state: StringState::default(),
			referenced_lines: PerLineBits::new(),
		}
	}

	/// Returns a bit set tracking which lines have been referenced in `GOTO`/`GOSUB` statements.
	pub(crate) fn referenced_lines(&self) -> &PerLineBits { &self.referenced_lines }

	/// Processes another line of the input stream.
	///
	/// A return value of `Ok(None)` indicates that the intended EOF marker has been found. Upon
	/// encountering this marker, further calls to this method will return `Ok(None)` without
	/// attempting to read more bytes from the inner stream.
	pub fn next_line(&mut self) -> Result<Option<Line>> {
		self.buffer.clear();
		self.line_state = LineState::HaveNothing;

		// terminated?
		if self.have_reached_end {
			return Ok(None);
		}

		let line_number = loop {
			// check for existing token unpacks first
			if let Some(byte) = self.cur_token.next() {
				self.buffer.push(byte);
				continue;
			}

			// check for existing bytes to yield as-is
			if let Some(b) = self.byte_flush.pop_front() {
				self.buffer.push(b);
				continue;
			}

			// handle EOL before attempting to read next byte
			if let LineState::InLine {
				line_number: ln,
				remaining_bytes: 0
			} = self.line_state {
				// account for abandoned, incomplete indirects first
				if let Some(to_push) = self.token_lookup.take() {
					self.byte_flush.push(to_push);
					continue;
				}
				if self.line_ref.is_some() {
					// unfinished line reference!
					return Err(Error { kind: ErrorKind::UnexpectedEof, line_number: ln });
				}
				break ln;
			}

			let self_line_number = Cell::new(u16::MAX);
			let wrap_error = |kind| Error { line_number: self_line_number.get(), kind };

			let nb = self.inner.next_byte().map_err(|ioe| Error {
				line_number: self_line_number.get(),
				kind: ioe.into(),
			})?;

			// what's the line state?
			let (_, remaining_bytes) = match self.line_state {
				LineState::HaveNothing => match nb {
					Some(0x0d) => {
						self.line_state = LineState::Have0D;
						continue;
					},
					Some(what) => return Err(wrap_error(ErrorKind::UnexpectedByte(what))),
					None => return Err(wrap_error(ErrorKind::UnexpectedEof)),
				},
				LineState::Have0D => match nb {
					Some(0xff) => {
						// this is an EOF marker
						self.have_reached_end = true;
						return Ok(None);
					},
					Some(lh) => {
						// line number, low byte
						self.line_state = LineState::HaveHalfLineNumber(lh);
						continue;
					},
					None => return Err(wrap_error(ErrorKind::UnexpectedEof)),
				},
				LineState::HaveHalfLineNumber(lh) => match nb {
					Some(ll) => {
						let lf = ((lh as u16) << 8) + (ll as u16);
						self.line_state = LineState::HaveFullLineNumber(lf);
						continue;
					},
					None => return Err(wrap_error(ErrorKind::UnexpectedEof)),
				},
				LineState::HaveFullLineNumber(lf) => {
					let len = match nb {
						// retrieved 0 -- technically supported, kinda weird
						Some(smol) if smol < 4 => {
							self.line_state = LineState::HaveNothing;
							continue;
						},
						// one was retrieved
						Some(rem) => rem,
						// EOF
						None => return Err(wrap_error(ErrorKind::UnexpectedEof)),
					};
					self.line_state = LineState::InLine {
						line_number: lf,
						// the BASIC line header includes itself in its length
						remaining_bytes: len - 4,
					};
					self_line_number.set(lf);
					continue;
				},
				LineState::InLine { line_number, ref mut remaining_bytes }
					=> (line_number, remaining_bytes),
			};
			debug_assert!(*remaining_bytes > 0);
			*remaining_bytes -= 1;

			// if here, we are in a line, with at least one more byte we can (and must) read
			let next_byte = nb.ok_or(ErrorKind::UnexpectedEof).map_err(wrap_error)?;

			// handle line number references
			if self.update_line_ref(next_byte).map_err(wrap_error)? {
				continue;
			}

			if next_byte == 0x8d {
				self.line_ref = Some(LineNumberStage::new());
				continue;
			}

			let to_push = match (self.string_state, next_byte == b'"') {
				(StringState::NotInString | StringState::MaybeClosed, true) => {
					self.string_state = StringState::InString;
					Some(next_byte)
				},
				(StringState::InString, true) => {
					self.string_state = StringState::MaybeClosed;
					Some(next_byte)
				},
				(StringState::MaybeClosed, false) => {
					self.string_state = StringState::NotInString;
					self.update_lookup_stage(next_byte)
				},
				(StringState::InString, false) => Some(next_byte),
				(StringState::NotInString, false) => self.update_lookup_stage(next_byte),
			};
			if let Some(to_push) = to_push {
				self.buffer.push(to_push);
				continue;
			}

			// no byte to push here, but there might be something in the buffer fields
			// so drop through the bottom of the loop
		}; // loop

		Ok(Some(Line::new(line_number, {
			let mut will_box = Vec::with_capacity(self.buffer.len());
			will_box.extend_from_slice(&self.buffer);
			will_box.into_boxed_slice()
		})))
	}

	fn update_lookup_stage(&mut self, next_byte: u8) -> Option<u8> {
		debug_assert!(self.cur_token.clone().next().is_none());

		let (decode_map, byte_if_fail) = match self.token_lookup.take() {
			Some(0xc6) => (&token_data::TOKEN_MAP_C6, Some(0xc6)),
			Some(0xc7) => (&token_data::TOKEN_MAP_C7, Some(0xc7)),
			Some(0xc8) => (&token_data::TOKEN_MAP_C8, Some(0xc8)),
			None if (0xc6..=0xc8).contains(&next_byte) => {
				// indirect will finish next round
				self.token_lookup = Some(next_byte);
				return None;
			},
			None => (&token_data::TOKEN_MAP_DIRECT, None),
			_ => return Some(next_byte),
		};

		// try lookup and conversion to token
		match decode_map.get_flat(next_byte as usize) {
			Some(s) => {
				let should_insert_space = if ! s.is_greedy() {
					// if next byte is also a token char, or token, insert a space
					// this undoes an optimisation by BASIC squashers that would destroy the
					// program syntax on a plaintext roundtrip
					// TODO: check if this is right
					self.inner.peek().map(|b| b >= 0x7f || b.is_ascii_alphabetic()) == Some(true)
				} else { false };
				self.cur_token = KeywordIter2::new(s.iter(), should_insert_space);
			},
			None => {
				// no match
				debug_assert!(self.byte_flush.is_empty());
				if let Some(b) = byte_if_fail {
					self.byte_flush.push(b);
				}
				self.byte_flush.push(next_byte);
			}
		};

		None // no ASCII byte to pass through
	}

	fn update_line_ref(&mut self, next_byte: u8) -> KindResult<bool> {
		let ref_stage = match self.line_ref {
			Some(ref mut stage) => stage,
			None => return Ok(false)
		};

		match **ref_stage {
			[a, b] => {
				let mut decoded = line_numbers::try_decode([a, b, next_byte])?;
				self.referenced_lines.get_mut(decoded).set();
				debug_assert!(self.byte_flush.is_empty());

				// stringify line reference
				let mut stage = ArrayVec::<u8, 5>::new();
				if decoded == 0 {
					stage.push(0);
				}
				while decoded > 0 {
					stage.push((decoded % 10) as u8 + b'0');
					decoded /= 10;
				}
				stage.reverse();
				self.byte_flush = stage;
				self.line_ref = None;
			},
			_ => ref_stage.push(next_byte),
		};

		Ok(true)
	}
}


/// A reimplementation of [std::iter::Peekable] for types that implement [`NextByte`].
///
/// [`Peekable::peek`] differs from the stdlib implementation in two ways:
///
/// - the peeked byte, if found, is copied;
/// - `Err` values are coalesced into `None`.
struct Peekable<I: NextByte> {
	inner: I,
	next: std::result::Result<Option<u8>, <I as NextByte>::Error>,
}

impl<I: NextByte> Peekable<I> {
	/// Constructs a new peekable wrapper around `I`.
	fn new(mut inner: I) -> Self {
		let next = inner.next_byte();
		Self { inner, next }
	}

	/// Reads ahead to the result of the next byte, if there is one, without appearing to advance
	/// the I/O object.
	fn peek(&self) -> Option<u8> {
		match self.next {
			Ok(Some(b)) => Some(b),
			_ => None,
		}
	}
}

impl<I: NextByte> NextByte for Peekable<I> {
	type Error = I::Error;

	fn next_byte(&mut self) -> std::result::Result<Option<u8>, Self::Error> {
		mem::replace(&mut self.next, self.inner.next_byte())
	}
}

impl<I: NextByte> fmt::Debug for Peekable<I> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		struct NextFormat<'a, I: NextByte>(&'a std::result::Result<Option<u8>, <I as NextByte>::Error>);

		impl<'a, I: NextByte> fmt::Debug for NextFormat<'a, I> {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				match *self.0 {
					Ok(Some(b)) => write!(f, "byte({:02x})", b),
					Ok(None) => f.write_str("None"),
					Err(_) => f.write_str("Err(_)"),
				}
			}
		}

		f.debug_struct("Peekable")
			.field("next", &NextFormat::<'_, I>(&self.next))
			.finish_non_exhaustive()
	}
}

/// The KeywordIter we all know and love, but with the possibility of yielding a space (U+0020)
/// immediately afterwards.
#[derive(Debug, Clone)]
struct KeywordIter2<'a> {
	keyword_iter: keyword::Iter<'a>,
	trail_space: bool,
}

impl<'a> KeywordIter2<'a> {
	fn new(keyword_iter: keyword::Iter<'a>, trail_space: bool) -> Self {
		Self {
			keyword_iter,
			trail_space,
		}
	}
}

impl<'a> Default for KeywordIter2<'a> {
	fn default() -> Self {
		Self {
			keyword_iter: keyword::Iter::empty(),
			trail_space: false,
		}
	}
}

impl<'a> Iterator for KeywordIter2<'a> {
	type Item = <keyword::Iter<'a> as Iterator>::Item;

	fn next(&mut self) -> Option<Self::Item> {
		if let b @ Some(_) = self.keyword_iter.next() {
			b
		} else {
			mem::replace(&mut self.trail_space, false).then_some(b'\x20')
		}
	}
}



#[cfg(test)]
mod test_support_structs {
	use super::*;
	use arrayvec::ArrayVec;

	struct NbSlice<'a> {
		slice: &'a [u8],
	}

	impl<'a> NextByte for NbSlice<'a> {
		type Error = core::convert::Infallible;

		fn next_byte(&mut self) -> std::result::Result<Option<u8>, Self::Error> {
			Ok(if let Some((first, rest)) = self.slice.split_first() {
				self.slice = rest;
				Some(*first)
			} else {
				None
			})
		}
	}

	impl<'a> From<&'a [u8]> for NbSlice<'a> {
		fn from(slice: &'a [u8]) -> Self {
			Self { slice }
		}
	}

	#[test]
	fn peek() {
		let mut p = Peekable::new(NbSlice::from(b"123".as_slice()));

		for i in b'1'..=b'3' {
			assert_eq!(Some(i), p.peek());
			assert_eq!(Ok(Some(i)), p.next_byte());
		}

		assert_eq!(None, p.peek());
		assert_eq!(Ok(None), p.next_byte());
		assert_eq!(None, p.peek());
	}

	// this is a function cause we can't use a const impl Index on stable (as of 1.68)
	#[allow(non_snake_case)]
	fn PRINT() -> &'static keyword::RawKeyword {
		token_data::TOKEN_MAP_DIRECT[0xf1].as_ref().unwrap()
	}

	#[test]
	fn keyword_iter2_with_space() {
		assert_eq!(
			&*KeywordIter2::new(PRINT().iter(), true).collect::<ArrayVec<u8, 6>>(),
			b"PRINT ",
		);
	}

	#[test]
	fn keyword_iter2_no_space() {
		assert_eq!(
			&*KeywordIter2::new(PRINT().iter(), false).collect::<ArrayVec<u8, 5>>(),
			b"PRINT",
		);
	}

	#[test]
	fn keyword_iter2_default() {
		assert!(KeywordIter2::default().next().is_none());
	}
}


#[cfg(test)]
mod test_parser {
	use super::*;

	#[derive(Debug)]
	#[repr(transparent)]
	pub(crate) struct InMemoryBytes<I: Iterator<Item = u8>>(pub I);

	impl<I: Iterator<Item = u8>> NextByte for InMemoryBytes<I> {
		type Error = Infallible;

		fn next_byte(&mut self) -> std::result::Result<Option<u8>, Self::Error> {
			Ok(self.0.next())
		}
	}

	fn expand_core(src: &[u8]) -> Parser<impl NextByte<Error = core::convert::Infallible> + '_> {
		let faux_io = InMemoryBytes(src.iter().copied());
		Parser::new(faux_io)
	}

	fn expand(line_number: u16, expected: &[u8], src: &[u8]) {
		println!("\ncase: {:02x?}", expected);
		let result = expand_core(src).next_line().unwrap();
		assert_eq!(Some(line_number), result.as_ref().map(|line| line.line_number));
		assert_eq!(expected, result.as_ref().map(|line| &*line.data).unwrap_or_default());
	}

	fn expand_err(expected: ErrorKind, src: &[u8]) {
		let mut parser = expand_core(src);
		let result = parser.next_line();
		assert_eq!(Some(&expected), result.as_ref().err().map(|e| &e.kind),
			"somehow parsed: {:02x?}", result.as_ref().ok());
	}

	#[test]
	fn expand_pure_ascii() {
		expand(10, b"hello", b"\x0d\x00\x0a\x09hello");
	}

	#[test]
	fn expand_direct() {
		expand(10, b"PRINT CHR$32", b"\x0d\x00\x0a\x09\xf1 \xbd32");
		expand(10, b"PRINTCHR$32", b"\x0d\x00\x0a\x08\xf1\xbd32");
	}

	#[test]
	fn expand_indirect() {
		expand(10, b"SYS87", b"\x0d\x00\x0a\x08\xc8\x9987");
	}

	#[test]
	fn abandoned_indirects() {
		expand(10, b"\xc7", &[13,0,10,5, 0xc7]);
	}

	#[test]
	fn failed_indirect() {
		expand(1, b"\xc6", &[13,0,1,5, 0xc6]);
		expand(1, b"\xc7", &[13,0,1,5, 0xc7]);
		expand(1, b"\xc8", &[13,0,1,5, 0xc8]);
	}

	#[test]
	fn interrupt_indirect_with_another() {
		// RISC OS behaviour with an invalid token reference is not consistent OS-wide:
		// - BASIC rejects the line outright ("Syntax error")
		// - !Edit skips over the invalid token
		// - !Zap prints the invalid token as its raw Latin-1 bytes
		// We choose the 'preserve as-is' option here, given that unpack's priority is
		//  round-trip preservation.
		expand(0, b"\xc7\xc6PRINT", &[13,0,0,7, 0xc7, 0xc6, 0xf1]);
	}

	#[test]
	fn no_decode_in_string_literals() {
		expand(1, b"LOAD \"\xc7\x95\"", b"\r\0\x01\x0b\xc7\x95 \"\xc7\x95\"");
	}

	#[test]
	fn multiline() {
		fn expand_multi<const N: usize>(
			line_numbers: &[u16; N],
			expected: &[&[u8]; N],
			src: &[u8],
		) {
			let mut parser = Parser::new(src.iter());
			for i in 0..N {
				let line = parser.next_line().expect("unexpected parse failure");
				assert_eq!(Some(line_numbers[i]), line.as_ref().map(|l| l.line_number));
				assert_eq!(Some(expected[i]), line.as_ref().map(|l| &*l.data));
			}

			assert_eq!(None, parser.next_line().expect("unexpected parse failure")
				.map(|_| ()));
		}

		expand_multi(&[10, 20], &[b"line 1", b"line 2"],
			b"\r\0\x0a\x0aline 1\r\0\x14\x0aline 2\r\xff");

		expand_multi(&[], &[], b"\x0d\xff");

		expand_multi(&[1], &[b"PRINT"], b"\r\0\x01\x05\xf1\r\xff");
	}

	#[test]
	fn just_look_around_you() {
		let mut parser = Parser::new(
			b"\r\0\x0a\x19\xf1 \"LOOK AROUND YOU \";\r\0\x14\x0a\xe5 \x8dTJ@\r\xff"
			.as_slice().iter());

		let mut expect_success = |line_number: u16, exp_data: &[u8]| {
			let line = parser.next_line()
				.expect("unexpected parse error")
				.expect("unexpected parsed EOF");
			assert_eq!(line_number, line.line_number);
			assert_eq!(exp_data, &*line.data);
		};

		expect_success(10, b"PRINT \"LOOK AROUND YOU \";");
		expect_success(20, b"GOTO 10");
		assert_eq!(Ok(None), parser.next_line());
	}

	#[test]
	fn goto_gosub() {
		expand(1, b"GOTO 10", b"\r\0\x01\x0a\xe5 \x8dTJ@");
		expand(2, b"GOSUB20", b"\r\0\x02\x09\xe4\x8dTT@");
	}

	#[test]
	fn bad_line_number() {
		eprintln!("case 1");
		expand_err(ErrorKind::UnexpectedEof, b"\r\0\x0a\x06\x8dT");
		eprintln!("case 2");
		expand_err(ErrorKind::UnexpectedEof, b"\r\0\x0a\x07\x8dTJ");
		eprintln!("case 3");
		expand_err(ErrorKind::InvalidLineReference, b"\r\0\x0a\x09\xe5\x8d\x18\0?");
	}

	#[test]
	fn referenced_lines() {
		let mut sut = Parser::new([
			13, 0, 10, 5,  0xf4,
			13, 0, 20, 9,  0xe5, 0x8d, 0x54, 0x5e, 0x40,
			13, 255
		].as_slice().iter());
		let expected = [
			(10, b"REM".as_slice()),
			(20, b"GOTO30"),
		];
		for (line_number, content) in expected {
			let line = sut.next_line().unwrap().unwrap();
			assert_eq!(line_number, line.line_number);
			assert_eq!(content, &*line.data, "got {:02x?}", &*line.data);
		}
		assert!(sut.referenced_lines.get(30));
		assert!(!sut.referenced_lines.get(20));
	}

	#[test]
	fn catch_compressed_forms() {
		/*
		There are some token forms in a tokenised BASIC file that do not survive a plaintext
		round-trip if converted as-is. For instance, the sequence `a3 8c` corresponds to
		`FALSETHEN`, but because `FALSE` is a non-greedy token, it will pass through the
		TokenScanner as its original ASCII (the same is true on Acorn systems).

		Ideally, we would find such situations and insert a space between them to keep the
		syntactic meaning intact on repacking, even if the byte-for-byte comparison no longer holds
		(and `basilisc` is not a BASIC squasher, so removing said spaces can remain out of scope).
		*/
		expand(10, b"FALSE THEN", b"\r\0\x0a\x06\xa3\x8c");

		// but don't do it on greedy tokens
		expand(11, b"ANDREA", b"\r\0\x0b\x08\x80REA");
	}
}
