use core::convert::Infallible;
use core::mem;
use std::io;

use crate::{
	support::*,
	common::*,
	token_data,
	line_numbers,
};

use arrayvec::ArrayVec;
use ascii::{AsciiChar,AsciiStr};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum UnpackError {
	#[error("unexpected end of file")]
	UnexpectedEof,
	#[error("expected 0Dh byte to start a new line, got {} instead", .0)]
	UnexpectedByte(u8),
	#[error("invalid line number")]
	InvalidLineNumber,
	#[error("line number out of range")]
	LineNumberOutOfRange(u32),
	#[error("io error: {0}")]
	IoError(String),
}

impl From<io::Error> for UnpackError {
	fn from(e: io::Error) -> Self {
		Self::IoError(e.to_string())
	}
}

impl From<Infallible> for UnpackError {
	fn from(e: Infallible) -> Self {
		match e {}
	}
}

type UnpackResult<T> = Result<T, UnpackError>;
type TokenLookup = ArrayVec<u8, 2>;
type LineNumberStage = ArrayVec<u8, 3>;

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


#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum StringState {
	#[default]
	NotInString,
	InString,
	MaybeClosed,
}


#[derive(Debug)]
enum LineNumberRef {
	None,
	Expect8D,
	Filling(LineNumberStage),
}

impl Default for LineNumberRef {
	fn default() -> Self {
		Self::None
	}
}


#[derive(Debug)]
pub struct Parser<I> {
	/// The inner byte I/O object
	inner: I,
	/// Buffer used for parsing, to minimise reallocations between lines
	buffer: Vec<u8>,
	/// The state of parsing metdata about the current line
	line_state: LineState,
	/// Handles line references after GOTO/GOSUB statements
	_line_ref: (), // TODO
	/// Buffer to fill the 1–2 bytes of a BBC BASIC token
	token_lookup: TokenLookup,
	/// Buffer for potential lookups that didn't match anything, or for format hacks
	byte_flush: TokenLookup,
	/// Iterator for remaining characters of a previous lookup match
	cur_token: ascii::Chars<'static>,
	/// Have we reached the end of the BASIC file?
	have_reached_end: bool,
	/// Are we in a string literal? (if so, don't expand tokens)
	string_state: StringState,
}

impl<I> Parser<I>
where I: NextByte, UnpackError: From<<I as NextByte>::Error> {
	pub fn new(inner: I) -> Self {
		Self {
			inner,
			buffer: Vec::with_capacity(256), // TODO how big can a line be?
			line_state: LineState::default(),
			_line_ref: (),
			token_lookup: ArrayVec::new_const(),
			byte_flush: ArrayVec::new_const(),
			cur_token: <&'static AsciiStr>::default().chars(),
			have_reached_end: false,
			string_state: StringState::default(),
		}
	}

	pub fn next_line(&mut self) -> UnpackResult<Option<Line>> {
		self.buffer.clear();
		self.line_state = LineState::HaveNothing;

		// terminated?
		if self.have_reached_end {
			return Ok(None);
		}

		let line_number = loop {
			// check for existing token unpacks first
			if let Some(b) = self.cur_token.next() {
				self.buffer.push(b.as_byte());
				continue;
			}

			// check for existing bytes to yield as-is
			if let Some(b) = self.byte_flush.pop_at(0) {
				self.buffer.push(b);
				continue;
			}

			// handle EOL before attempting to read next byte
			if let LineState::InLine {
				line_number: ln,
				remaining_bytes: 0
			} = self.line_state {
				// account for abandoned, incomplete indirects first
				if ! self.token_lookup.is_empty() {
					self.byte_flush = mem::replace(&mut self.token_lookup, TokenLookup::new_const());
					continue;
				}
				break ln;
			}

			let nb = self.inner.next_byte()?;

			// what's the line state?
			let (_, remaining_bytes) = match self.line_state {
				LineState::HaveNothing => match nb {
					Some(0x0d) => {
						self.line_state = LineState::Have0D;
						continue;
					},
					Some(what) => return Err(UnpackError::UnexpectedByte(what)),
					None => return Err(UnpackError::UnexpectedEof),
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
					None => return Err(UnpackError::UnexpectedEof),
				},
				LineState::HaveHalfLineNumber(lh) => match nb {
					Some(ll) => {
						let lf = ((lh as u16) << 8) + (ll as u16);
						self.line_state = LineState::HaveFullLineNumber(lf);
						continue;
					},
					None => return Err(UnpackError::UnexpectedEof),
				},
				LineState::HaveFullLineNumber(lf) => {
					let len = match nb {
						// retrieved 0 -- technically supported, kinda weird
						Some(0) => {
							self.line_state = LineState::HaveNothing;
							continue;
						},
						// one was retrieved
						Some(rem) => rem,
						// EOF
						None => return Err(UnpackError::UnexpectedEof),
					};
					self.line_state = LineState::InLine { line_number: lf, remaining_bytes: len };
					continue;
				},
				LineState::InLine { line_number, ref mut remaining_bytes }
					=> (line_number, remaining_bytes),
			};
			debug_assert!(*remaining_bytes > 0);
			*remaining_bytes -= 1;

			// if here, we are in a line, with at least one more byte we can (and must) read
			let next_byte = nb.ok_or(UnpackError::UnexpectedEof)?;

			// TODO: LF literals should be passed straight through; what do we do about that?

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
			will_box.extend_from_slice(&*self.buffer);
			will_box.into_boxed_slice()
		})))
	}

	fn update_lookup_stage(&mut self, next_byte: u8) -> Option<u8> {
		debug_assert!(self.cur_token.as_str().is_empty());
		debug_assert!(self.byte_flush.is_empty());

		fn queue_shift<I>(this: &mut Parser<I>, next_byte: u8) -> Option<u8> {
			this.byte_flush = mem::replace(&mut this.token_lookup, TokenLookup::new_const());
			if let old @ Some(_) = this.byte_flush.pop_at(0) {
				this.byte_flush.push(next_byte);
				old
			} else {
				Some(next_byte)
			}
		}

		if next_byte == 0x8d {
			// 8d is always a leading escape byte, never a continuation byte
			// so take whatever's in token_lookup, move it to the output
			// sequence, and queue up another attempt

			self.byte_flush = mem::replace(&mut self.token_lookup, TokenLookup::new_const());
			self.token_lookup.push(0x8d);
			return None;
		}

		let decode_map = match *self.token_lookup {
			[0x8d, 0xc6] => token_data::TOKEN_MAP_C6,
			[0x8d, 0xc7] => token_data::TOKEN_MAP_C7,
			[0x8d, 0xc8] => token_data::TOKEN_MAP_C8,
			[0x8d] if (0xc6..=0xc8).contains(&next_byte) => {
				// indirect is still ongoing
				self.token_lookup.push(next_byte);
				return None;
			}
			[] => token_data::TOKEN_MAP_DIRECT,
			_ => return queue_shift(self, next_byte),
		};

		// try lookup and conversion to token
		match decode_map.get(next_byte as usize).and_then(|&o| o) {
			Some(s) => {
				self.cur_token = s.chars();
				self.token_lookup.clear();
				None
			},
			None => {
				// no match
				let _8d = self.token_lookup.pop_at(0);
				debug_assert!(_8d.unwrap_or(0x8d) == 0x8d, "_8d is actually {:02x?}", _8d);
				queue_shift(self, next_byte)
			}
		}
	}
}

#[cfg(test)]
mod test_parser {
	use super::*;

	use std::borrow::Borrow;

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

	fn expand_err(expected: UnpackError, src: &[u8]) {
		let result = expand_core(src).next_line();
		assert_eq!(Some(expected), result.err());
	}

	#[test]
	fn expand_pure_ascii() {
		expand(10, b"hello", b"\x0d\x00\x0a\x05hello");
	}

	#[test]
	fn expand_direct() {
		expand(10, b"PRINT CHR$32", b"\x0d\x00\x0a\x05\xf1 \xbd32");
		expand(10, b"PRINTCHR$32", b"\x0d\x00\x0a\x04\xf1\xbd32");
	}

	#[test]
	fn expand_indirect() {
		expand(10, b"SYS87", b"\x0d\x00\x0a\x05\x8d\xc8\x1487");
	}

	#[test]
	fn abandoned_indirects() {
		expand(10, b"\x8d", &[13,0,10,1, 0x8d]);
		expand(10, b"\x8d\xc7", &[13,0,10,2, 0x8d, 0xc7]);
	}

	#[test]
	fn failed_direct() {
		expand(1, b"\xc6", &[13,0,1,1, 0xc6]);
		expand(1, b"\xc7", &[13,0,1,1, 0xc7]);
		expand(1, b"\xc8", &[13,0,1,1, 0xc8]);
	}

	#[test]
	fn interrupt_indirect_with_another() {
		expand(0, b"\x8dSUM", &[13,0,0,4, 0x8d, 0x8d, 0xc6, 0x03]);
		expand(0, b"\x8d\xc7SUM", &[13,0,0,5, 0x8d, 0xc7, 0x8d, 0xc6, 0x03]);
	}

	#[test]
	fn no_decode_in_string_literals() {
		expand(1, b"LOAD \"\x8d\xc7\x18\"", b"\r\0\x01\x09\x8d\xc7\x18 \"\x8d\xc7\x18\"");
	}

	#[test]
	fn multiline() {
		fn expand_multi<const N: usize>(
			line_numbers: &[u16; N],
			expected: &[&[u8]; N],
			src: &[u8],
		) {
			let mut parser = Parser::new(src);
			for i in 0..N {
				let line = parser.next_line().expect("unexpected parse failure");
				assert_eq!(Some(line_numbers[i]), line.as_ref().map(|l| l.line_number));
				assert_eq!(Some(expected[i]), line.as_ref().map(Borrow::<[u8]>::borrow));
			}

			assert_eq!(None, parser.next_line().expect("unexpected parse failure")
				.map(|_| ()));
		}

		expand_multi(&[10, 20], &[b"line 1", b"line 2"],
			b"\r\0\x0a\x06line 1\r\0\x14\x06line 2\r\xff");

		expand_multi(&[], &[], b"\x0d\xff");

		expand_multi(&[1], &[b"PRINT"], b"\r\0\x01\x01\xf1\r\xff");
	}
}


trait U8Ext: core::cmp::Eq + core::cmp::PartialEq<u8> {
	fn is_goto_or_gosub(&self) -> bool {
		// impl assumes that LINE_DEPENDENT_KEYWORD_BYTES is 2 bytes wide
		let [a, b] = token_data::LINE_DEPENDENT_KEYWORD_BYTES;
		*self == a || *self == b
	}
}

impl U8Ext for u8 {}
