//! Retokenise a text-format BASIC file.

// Regarding line ref keywords (GOTO and GOSUB), we make a couple assumptions in this module:
// - They have direct byte maps;
// - They're greedy.

use std::num::NonZeroU8;
use std::{fmt, mem};

use arrayvec::ArrayVec;

use crate::line_numbers;
use crate::support::{ArrayVecExt, HexArray};

use basilisc_base::keyword::{RawKeyword, TokenIter, TokenPosition};

// chars that didn't match anything
pub(super) type CharBuffer = ArrayVec<u8, { basilisc_base::keyword::MAX_LEN as usize }>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Error {
	InvalidLineRef { after: u8 },
	MissingLineRef { after: u8 },
}

impl Error {
	pub fn lookup(byte: u8) -> &'static ascii::AsciiStr {
		crate::token_data::TOKEN_MAP_DIRECT.get_flat(byte as usize)
			.map(RawKeyword::as_ascii_str)
			.unwrap_or(Self::LOOKUP_DEFAULT)
	}

	const LOOKUP_DEFAULT: &'static ascii::AsciiStr = unsafe {
		// SAFETY: string is obviously ASCII, and the crate doesn't give us any const fns for this
		core::mem::transmute("keyword")
	};
}

/// Core tokeniser.
pub(super) struct TokenScanner {
	char_buf: CharBuffer,
	char_out_buf: CharBuffer,
	best_match: Option<&'static RawKeyword>,
	pinch: &'static [RawKeyword],
	is_lhs: bool,
	else_hack: ElseHack,
	line_ref: LineRefState,
}

static PINCH_ALL: &[RawKeyword] = crate::token_data::LOOKUP_MAP.as_slice();

/// A (slightly hackish) way to handle that `ELSE` keywords tokenise differently when used in
/// multi-line `IF` statements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) struct ElseHack {
	stack: u32,
	on_then: bool,
}

impl ElseHack {
	pub(super) const THEN: u8 = 0x8c;
	pub(super) const ELSE: u8 = 0x8b;
	#[allow(dead_code)] // used by a `pack` test
	pub(super) const ALT_ELSE: u8 = 0xcc;
	pub(super) const ENDIF: u8 = 0xcd;

	#[inline]
	fn alt_else() -> TokenIter {
		TokenIter::new_direct(nonzero_ext::nonzero!(0xccu8))
	}

	#[inline]
	fn use_alt_else(&self) -> bool { self.stack > 0 }

	fn push(&mut self) {
		self.stack = self.stack.checked_add(1)
			.expect("ElseHack stack overflow"); // should never happen. you can't make enough lines
		self.on_then = false;
	}

	fn pop(&mut self) {
		// an over-pop isn't a problem for us
		self.stack = self.stack.saturating_sub(1);
		self.on_then = false;
	}
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
enum LineRefState {
	No,
	Expecting { kw_byte: u8 },
	Building { kw_byte: u8, stage: u16 },
}

impl TokenScanner {
	/// Constructs a new token scanner.
	pub fn new() -> Self {
		Self {
			char_buf: CharBuffer::new(),
			char_out_buf: CharBuffer::new(),
			best_match: None,
			pinch: PINCH_ALL,
			is_lhs: true,
			else_hack: ElseHack::default(),
			line_ref: LineRefState::No,
		}
	}

	/// Fetch any characters that have passed through the scanner.
	///
	/// You should always call this repeatedly until it returns `None` before calling
	/// [`push`](Self::push).
	pub fn try_pull(&mut self) -> Option<u8> {
		self.char_out_buf.pop_front()
	}

	/// Pushes a new byte (a RISC OS Latin-1 char) into the scanner.
	pub fn push(&mut self, ch: u8) -> Result<(), Error> {
		macro_rules! set_lhs {
			(char) => { self.is_lhs = ch == b':'; };
		}

		if ch == b'.' {
			// early match on short keyword
			if let Some(char_buf_len) = NonZeroU8::new(self.char_buf.len() as u8) {
				// (an empty char_buf wouldn't be useful)
				debug_assert!(self.pinch.len() < PINCH_ALL.len());
				let abbr_match = self.pinch.iter().find(|&kw| {
					let min_abbrev_len = kw.min_abbrev_len();
					min_abbrev_len.is_some() && min_abbrev_len <= Some(char_buf_len)
				});
				if let Some(kw) = abbr_match {
					self.commit_to(kw.tokens());
					return Ok(());
				}
			}
		}

		// ELSE hack
		if self.else_hack.on_then && ch != b' ' {
			// ELSE hack; we could've been in a multi-line IF, but we aren't
			self.else_hack.on_then = false;
		}

		// line refs
		let ch_is_digit = (b'0'..=b'9').contains(&ch);
		match self.line_ref {
			LineRefState::No => { } // do nothing, fall through to normal logic
			LineRefState::Expecting { kw_byte: _ } if matches!(ch, b' ' | b'\t')
				=> { } // pass through spaces
			LineRefState::Expecting { kw_byte } if ch_is_digit => {
				// time to start the line ref
				self.line_ref = LineRefState::Building { kw_byte, stage: (ch - b'0') as u16 };
				return Ok(()); // do nothing else with this byte
			}
			LineRefState::Expecting { kw_byte } // some unexpected character
				=> return Err(Error::InvalidLineRef { after: kw_byte }),
			LineRefState::Building { kw_byte, ref mut stage } if ch_is_digit => {
				let new_line_ref = (*stage as u32) * 10 + (ch - b'0') as u32;
				if new_line_ref >= line_numbers::LIMIT as u32 {
					self.line_ref = LineRefState::No;
					return Err(Error::InvalidLineRef { after: kw_byte }); // number went too high
				}
				*stage = new_line_ref as u16;
				return Ok(()); // do nothing else with this byte
			}
			LineRefState::Building { kw_byte, stage } => {
				// finished the line number; flush state
				self.push_enc_line_ref(kw_byte, stage)?;
				// this wasn't a line ref digit though, so fall through
			}
		}

		match (Self::is_keyword_char(ch), self.pinch.is_empty()) {
			(true, true) => {
				// already failed, possible variable name, do not restart searching
				self.char_out_buf.push(ch);
			},
			(false, true) => {
				// reset pinch, but this char will never match
				self.flush_char_buf();
				self.pinch = PINCH_ALL;
				self.char_out_buf.push(ch);
				set_lhs!(char);
			},
			(true, false) => {
				// it's narrowing time babey
				self.narrow(ch);
				self.try_extract();
			},
			(false, false) => {
				// this char will never match; flush all consideration chars
				self.commit_best_match();
				self.char_out_buf.push(ch);
				set_lhs!(char);
			},
		}

		Ok(())
	}

	/// Flushes any remaining characters out of the scanner.
	///
	/// You should always call this, then [`try_pull`](Self::try_pull), when you have no more bytes
	/// coming in from a line.
	pub fn flush(&mut self) -> Result<(), Error> {
		if let Some(kw) = self.best_match.take().filter(|kw|
			kw.len().get() as usize == self.char_buf.len()
		) {
			// crush best_match chars into token equiv
			// allow nongreedy keywords to do this if char_buf matches
			let ti = kw.tokens();
			let ti_first = ti.peek_first();
			self.char_out_buf.extend(ti.map(NonZeroU8::get));
			self.char_out_buf.extend(self.char_buf.iter().copied().skip(kw.len().get() as usize));

			if ti_first == ElseHack::ENDIF {
				// reset else hack
				self.else_hack.pop();
			}
		}
		else {
			self.char_out_buf.extend(self.char_buf.iter().copied());
		}
		self.char_buf.clear();
		self.pinch = PINCH_ALL;

		match self.line_ref {
			LineRefState::No => { } // no problem
			LineRefState::Expecting { kw_byte } => {
				// uh oh, we were expecting a line reference here
				return Err(Error::MissingLineRef { after: kw_byte });
			}
			LineRefState::Building { kw_byte, stage } => {
				// line finished with a line ref decimal byte. ok!
				self.push_enc_line_ref(kw_byte, stage)?;
			}
		}

		if self.else_hack.on_then {
			// we are in a multi-line IF, change the ELSE token
			self.else_hack.push();
		}

		Ok(())
	}

	fn push_enc_line_ref(&mut self, kw_byte: u8, stage: u16) -> Result<(), Error> {
		let Ok(enc) = line_numbers::try_encode(stage) else {
			return Err(Error::InvalidLineRef { after: kw_byte })
		};

		self.char_out_buf.try_push(line_numbers::MARKER_BYTE)
			.expect("no room for marker byte");

		self.char_out_buf.try_extend_from_slice(&enc)
			.expect("no room for line ref");

		self.line_ref = LineRefState::No;
		Ok(())
	}

	fn set_token_buf(&mut self, mut new: TokenIter) {
		let first = new.peek_first();

		// apply ELSE hack
		match first {
			ElseHack::THEN => {
				self.else_hack.on_then = true;
				self.is_lhs = true;
			},

			ElseHack::ELSE if self.else_hack.use_alt_else() => {
				new = ElseHack::alt_else();
			},

			ElseHack::ENDIF => {
				self.else_hack.pop();
			}

			_ => {},
		};

		self.char_out_buf.extend(new.map(NonZeroU8::get));
	}

	fn narrow(&mut self, ch: u8) {
		let pinch_idx = self.char_buf.len();
		self.char_buf.push(ch);

		// front byte first
		while let Some((left, remain)) = self.pinch.split_first() {
			let should_narrow_normal = left.as_ascii_str().as_bytes()
				.get(pinch_idx).map(|&b| b < ch) != Some(false);

			// skip over RHS token if there is one
			let should_narrow_lhs = self.is_lhs && left.position() == TokenPosition::Right;
			// if we did that, there must be a RHS one immediately after
			debug_assert!(!should_narrow_lhs
				|| remain.get(0).map(|kw| kw.position()) == Some(TokenPosition::Left));

			if should_narrow_normal || should_narrow_lhs {
				// not yet narrowed down to matching substrings
				self.pinch = remain;
			} else {
				break;
			}
		}

		// then back byte
		while let Some((right, remain)) = self.pinch.split_last() {
			if right.as_bytes()
			.get(pinch_idx).map(|&b| b > ch) == Some(true) {
				// too flabby on the right
				self.pinch = remain;
			} else {
				break;
			}
		}
	}

	fn try_extract(&mut self) {
		macro_rules! eq_char_buf {
			($kw:expr) => { &*self.char_buf == $kw.as_bytes() };
		}

		match *self.pinch {
			// perfect match, greedy match
			[ref kw] if kw.is_greedy() && eq_char_buf!(kw) => {
				self.commit_to(kw.tokens());
			},

			// a good match, but there might be a longer one
			[ref kw, ..] if eq_char_buf!(kw) => {
				self.best_match = Some(kw);
			},

			// confirmed no match; purge all known chars
			[] => {
				self.commit_best_match();
			},

			// still searching, no match yet
			[_, ..] => {
			},
		};
	}

	fn commit_to(&mut self, ti: TokenIter) {
		self.best_match = None; // forget that, we have a definite winner
		self.set_token_buf(ti.clone());
		self.char_buf.clear(); // all bytes accounted for
		self.pinch = PINCH_ALL; // ready for future stuff

		let first_byte = ti.peek_first();

		self.is_lhs = if first_byte == ElseHack::THEN {
			self.else_hack.on_then = true;
			true
		} else {
			false
		};

		if let Some(true) = crate::token_data::TOKEN_MAP_DIRECT.get_flat(first_byte as usize)
		.map(|k| k.triggers_line_ref()) {
			// we just pushed a line-ref-triggering keyword
			debug_assert!(matches!(self.line_ref, LineRefState::No));
			self.line_ref = LineRefState::Expecting { kw_byte: first_byte };
		}
	}

	fn commit_best_match(&mut self) {
		if let Some(kw) = self.best_match.take() {
			// should only commit if best match is either
			// - greedy
			// - nongreedy, but followed by something other than A-Z
			let should_take = kw.is_greedy() || {
				self.char_buf.get(kw.len().get() as usize)
					.map(|c| !c.is_ascii_uppercase())
					.unwrap_or(true)
			};
			if should_take {
				// take this subset of characters, use it
				self.set_token_buf(kw.tokens());

				// preserve unconsumed chars to re-add them
				self.char_buf.remove_first(kw.len().get() as usize);
				self.pinch = PINCH_ALL;

				self.is_lhs = false;

				match *self.char_buf {
					[] => {},
					[ch] => {
						self.char_buf.clear(); // `narrow` wants to re-add it
						self.narrow(ch);
					},
					_ => unreachable!("considered re-narrow with char_buf len {}; possible?",
						self.char_buf.len()),
				};
				return;
			}
		}

		// move all chars as-is
		self.flush_char_buf();

		self.pinch = PINCH_ALL;
	}

	fn flush_char_buf(&mut self) {
		self.char_out_buf.extend(mem::take(&mut self.char_buf).into_iter());
	}

	fn is_keyword_char(ch: u8) -> bool {
		// it's variable validity that matters here; getting one of these with an empty pinch
		// is *not* a case where we can reset the tokeniser
		// @ is not included; @% is an edge case, but not one we need to worry about
		ch.is_ascii_uppercase() || b"_$(".contains(&ch)
	}
}

impl Default for TokenScanner {
	#[inline(always)]
	fn default() -> Self {
		Self::new()
	}
}

impl fmt::Debug for TokenScanner {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		struct PinchDebug {
			front: Option<&'static RawKeyword>,
			back: Option<&'static RawKeyword>,
			count: usize,
		}

		impl PinchDebug {
			fn new(outer: &TokenScanner) -> Self {
				Self {
					front: outer.pinch.first(),
					back: outer.pinch.last(),
					count: outer.pinch.len(),
				}
			}
		}

		impl fmt::Debug for PinchDebug {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				if let Some(front) = self.front {
					write!(f, "\"{}\"", front.as_ascii_str())
				} else {
					f.write_str("(start)")
				}?;

				f.write_str(" to ")?;

				if let Some(back) = self.back {
					write!(f, "\"{}\"", back.as_ascii_str())
				} else {
					f.write_str("(end)")
				}?;

				write!(f, " (#{})", self.count)
			}
		}

		f.debug_struct("TokenScanner")
			.field("char_buf", &HexArray(&self.char_buf))
			.field("char_out_buf", &&*self.char_out_buf)
			.field("best_match", &self.best_match)
			.field("else_hack", &self.else_hack)
			.field("pinch", &PinchDebug::new(self))
			.finish()
	}
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn simple() {
		for (word, token) in [
			(&b"DRAW"[..], 0xdf),
			(b"ASC", 0x97),
			(b"OTHERWISE", 0x7f),
		] {
			let mut scanner = TokenScanner::new();
			for &ch in word {
				assert_eq!(None, scanner.try_pull());
				scanner.push(ch).unwrap();
			}
			assert_eq!(Some(token), scanner.try_pull());
		}
	}

	#[test]
	fn smush_cannot_match_longer() {
		assert_output(b"ASCII", b"\x97II");
	}

	#[test]
	fn smush_could_last_longer() {
		assert_output(b"GETS", b"\xa5S");
	}

	#[test]
	fn inkey() {
		assert_output(b"INKEY#", b"\xa6#");
		assert_output(b"INKEY$", b"\xbf");
	}

	#[test]
	fn not_in_middle() {
		const SRC: &'static [u8] = b"PTOLEMY"; // `TO` should not tokenise
		assert_output(SRC, SRC);
	}

	#[test]
	fn min_abbrev() {
		assert_output(b"MO.2", b"\xeb2");
		assert_output(b"P.\"yes\"", b"\xf1\"yes\"");
		assert_output(b"PRIN.\"yes\"", b"\xf1\"yes\"");
	}

	#[test]
	fn reset_after_tokenising() {
		let mut input = [b'P', b'R', b'I', b'N', b'T', 0, b'M', b'O', b'D', b'E'];
		const INP_POS: usize = 5;

		let mut output = [0xf1, 0, 0xeb];
		const OUTP_POS: usize = 1;

		for sep_char in [b':', b'/', 0x20, 7] {
			input[INP_POS] = sep_char;
			output[OUTP_POS] = sep_char;
			assert_output(input.as_slice(), output.as_slice());
		}
	}

	#[test]
	fn char_out_buf_limits() {
		static SRC: &'static [u8] = b"OTHERWISOTHERWISOTHERWISOTHERWIS";
		const CHUNK: usize = 8;

		for limit in (1..(SRC.len())).step_by(CHUNK) {
			let src = &SRC[..limit];
			assert_output(src, src);
		}
	}

	#[test]
	fn nongreedy() {
		assert_output(b"PRINTY", b"\xf1Y");
		assert_output(b"ENDY", b"ENDY");
	}

	#[test]
	#[allow(non_snake_case)]
	fn getDOLLAR() {
		assert_output(b"GET$", b"\xbe");
	}

	#[test]
	fn left_right_tokens() {
		assert_output(b"HIMEM=HIMEM", b"\xd3=\x93");
		assert_output(b"TIME:TIME", b"\xd1:\xd1");
		assert_output(b"IFTIME=1THENTIME=5", b"\xe7\x91=1\x8c\xd1=5");
	}

	#[test]
	fn nongreedy_limits() {
		assert_output(b"AND$", b"\x80$");
		assert_output(b"EXT1", b"\xa2\x31");
		assert_output(b"REPORT$", b"\xf6$");
	}

	#[test]
	fn else_hack() {
		assert_output(b"IFaTHENbELSEc", b"\xe7a\x8cb\x8bc");
	}

	#[test]
	fn swap_regression() {
		assert_output(
			b"FALSETHEN:TRUE:",
			b"FALSETHEN:\xb9:"
		);
		assert_output(b"OR(q", b"\x84(q");
	}

	#[test]
	fn found_regressions() {
		assert_output(b"+$STR$", b"+$\xc3");
		assert_output(b"+$STR$ERL", b"+$\xc3\x9e");
		assert_output(b"W%OF", b"W%\xca");
		assert_output(b"OFWHEN", b"\xca\xc9");
		assert_output(b"ENDPR", b"ENDPR");
		assert_output(b"PRINT\"it works\"", b"\xf1\"it works\"");
	}

	#[test]
	fn goto_gosub() {
		assert_output(b"GOTO10", b"\xe5\x8dTJ@");
		assert_output(b"GOSUB10", b"\xe4\x8dTJ@");
		assert_output(b"GOTO 600", b"\xe5 \x8dDXB");
		assert_output(b"GOTO 10 and then do sth else", b"\xe5 \x8dTJ@ and then do sth else");
		assert_output(b"finally, GOSUB 0", b"finally, \xe4 \x8dT@@");
	}

	#[track_caller]
	fn assert_output(input: &[u8], output: &[u8]) {
		let got = _inout(input);

		if &*got != output {
			panic!("token scan failed\n\nexpected `{}`\nbut got: `{}`\n",
				output.escape_ascii(), got.escape_ascii());
		}
	}

	fn _inout(src: &[u8]) -> Vec<u8> {
		if let Ok(s) = ascii::AsAsciiStr::as_ascii_str(src) {
			println!("src: {}", s);
		} else {
			println!("src: {}", HexArray(src));
		}
		let mut scanner = TokenScanner::new();
		let mut out_buf = Vec::with_capacity(src.len());

		for &ch in src {
			scanner.push(ch).unwrap();
			loop {
				match scanner.try_pull() {
					Some(b) => out_buf.push(b),
					None => break,
				}
			}
		}
		scanner.flush().unwrap();
		while let Some(b) = scanner.try_pull() {
			out_buf.push(b);
		}
		out_buf
	}
}
