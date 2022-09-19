use core::convert::Infallible;
use core::mem;
use std::io;

use crate::{
	support::*,
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


type TokenLookup = ArrayVec<u8, 2>;
type LineNumberStage = ArrayVec<u8, 3>;

#[derive(Debug, Clone, Copy)]
enum LineState {
	/// Not currently in a line
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

impl Default for LineState {
	#[inline]
	fn default() -> Self {
		Self::HaveNothing
	}
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
pub struct Unpacker<I> {
	/// The inner byte I/O object
	inner: I,
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
	///Do we need to send a trailing newline once we're done?
	must_send_trailing_newline: bool,
}

impl<I> Unpacker<I>
where I: NextByte, UnpackError: From<<I as NextByte>::Error> {
	pub fn new(inner: I) -> Self {
		Self {
			inner,
			line_state: LineState::default(),
			_line_ref: (),
			token_lookup: ArrayVec::new_const(),
			byte_flush: ArrayVec::new_const(),
			cur_token: <&'static AsciiStr>::default().chars(),
			have_reached_end: false,
			must_send_trailing_newline: false,
		}
	}

	pub fn next_byte(&mut self) -> Result<Option<u8>, UnpackError> {
		// terminated?
		if self.have_reached_end {
			return Ok(None);
		}

		let r = loop {
			// check for existing token unpacks first
			if let Some(b) = self.cur_token.next() {
				break b.as_byte();
			}

			// check for existing bytes to yield as-is
			if let Some(b) = self.byte_flush.pop() {
				break b;
			}


			// handle EOL before attempting to read next byte
			if matches!(self.line_state, LineState::InLine {
				line_number: _,
				remaining_bytes: 0
			}) {
				self.line_state = LineState::HaveNothing;
				break b'\n';
			}

			let nb = self.inner.next_byte()?;

			// what's the line state?
			let (line_number, remaining_bytes) = match self.line_state {
				LineState::HaveNothing => match nb {
					Some(0x0d) => {
						self.line_state = LineState::Have0D;
						continue
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
						continue
					},
					None => return Err(UnpackError::UnexpectedEof),
				},
				LineState::HaveHalfLineNumber(lh) => match nb {
					Some(ll) => {
						let lf = (lh as u16) << 8 + (ll as u16);
						self.line_state = LineState::HaveFullLineNumber(lf);
						continue
					},
					None => return Err(UnpackError::UnexpectedEof),
				},
				LineState::HaveFullLineNumber(lf) => {
					let len = match nb {
						// retrieved 0 -- technically supported, kinda weird
						Some(0) => {
							self.line_state = LineState::HaveNothing;
							continue
						},
						// one was retrieved
						Some(rem) => rem,
						// EOF
						None => return Err(UnpackError::UnexpectedEof),
					};
					self.line_state = LineState::InLine { line_number: lf, remaining_bytes: len };
					continue
				},
				LineState::InLine { line_number, ref mut remaining_bytes }
					=> (line_number, remaining_bytes),
			};
			debug_assert!(*remaining_bytes > 0);
			*remaining_bytes -= 1;

			// if here, we are in a line, with at least one more byte we can (and must) read
			let next_byte = self.inner.next_byte()
				.map_err(UnpackError::from)
				.and_then(|ob| ob.ok_or(UnpackError::UnexpectedEof))
				?;

			// TODO: LF literals should be passed straight through; what do we do about that?

			if let Some(to_push) = self.update_lookup_stage(next_byte) {
				break to_push;
			}

			// no byte to push here, but there might be something in the buffer fields
			// so drop through the bottom of the loop
		}; // loop

		self.must_send_trailing_newline = r != b'\n';
		Ok(Some(r))
	}

	fn update_lookup_stage(&mut self, next_byte: u8) -> Option<u8> {
		debug_assert!(self.cur_token.as_str().is_empty());
		debug_assert!(self.byte_flush.is_empty());

		fn queue_shift<I>(this: &mut Unpacker<I>, next_byte: u8) -> Option<u8> {
			this.byte_flush = mem::replace(&mut this.token_lookup, TokenLookup::new_const());
			if let old @ Some(_) = this.byte_flush.pop() {
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

			return queue_shift(self, next_byte);
		}

		let decode_map = match *self.token_lookup {
			[0x8d, 0xc6] => token_data::TOKEN_MAP_C6,
			[0x8d, 0xc7] => token_data::TOKEN_MAP_C7,
			[0x8d, 0xc8] => token_data::TOKEN_MAP_C8,
			[0x8d] => token_data::TOKEN_MAP_DIRECT,
			[] => {
				debug_assert!(next_byte != 0x8d);
				// not a token lookup
				return Some(next_byte);
			}
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
				let _8d = self.token_lookup.pop();
				debug_assert!(_8d == Some(0x8d));
				queue_shift(self, next_byte)
			}
		}
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
