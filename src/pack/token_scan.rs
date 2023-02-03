use std::num::NonZeroU8;
use std::{fmt, mem};

use arrayvec::ArrayVec;

use crate::{
	token_data::TokenLookupEntry,
	token_iter::TokenIter,
	support::{ArrayVecExt, HexArray},
	keyword::TokenPosition,
};

// chars that didn't match anything
pub(super) type CharBuffer = ArrayVec<u8, { crate::keyword::MAX_LEN as usize }>;

pub(super) struct TokenScanner {
	char_buf: CharBuffer,
	token_buf: Option<TokenIter>,
	char_out_buf: CharBuffer,
	best_match: Option<&'static TokenLookupEntry>,
	cur_token: Option<TokenIter>,
	pinch: &'static [TokenLookupEntry],
	is_lhs: bool,
}

static PINCH_ALL: &'static [TokenLookupEntry] = crate::token_data::LOOKUP_MAP.as_slice();

impl TokenScanner {
	pub fn new() -> Self {
		Self {
			char_buf: CharBuffer::new(),
			token_buf: None,
			char_out_buf: CharBuffer::new(),
			best_match: None,
			cur_token: None,
			pinch: PINCH_ALL,
			is_lhs: true,
		}
	}

	pub fn try_pull(&mut self) -> Option<u8> {
		'ti: loop {
			if let Some(ch) = self.cur_token.as_mut().and_then(Iterator::next) {
				return Some(ch.get());
			}

			// it's exhausted, try pulling from token_buf
			if let new @ Some(_) = self.token_buf.take() {
				self.cur_token = new;
			} else {
				break 'ti; // can't do anything else with tokens
			}
		}

		self.char_out_buf.pop_front()
	}

	pub fn push(&mut self, ch: u8) {
		macro_rules! set_lhs {
			(char) => { self.is_lhs = ch == b':'; };
		}

		if ch == b'.' {
			// early match on short keyword
			if let Some(char_buf_len) = NonZeroU8::new(self.char_buf.len() as u8) {
				// (an empty char_buf wouldn't be useful)
				debug_assert!(self.pinch.len() < PINCH_ALL.len());
				let abbr_match = self.pinch.iter().find(|&&(ref kw, _)| {
					let min_abbrev_len = kw.min_abbrev_len();
					min_abbrev_len.is_some() && min_abbrev_len <= Some(char_buf_len)
				});
				if let Some((_kw, ti)) = abbr_match {
					self.commit_to(ti);
					return;
				}
			}
		}
		match (Self::is_keyword_char(ch), self.pinch.is_empty()) {
			(true, true) => {
				// already failed, possible variable name, do not restart searching
				self.char_out_buf.push(ch);
			},
			(false, true) => {
				// reset pinch, but this char will never match
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
	}

	pub fn flush(&mut self) {
		if let Some((kw, ti)) = self.best_match.filter(|(kw, _)|
			kw.len().get() as usize == self.char_buf.len()
		) {
			// crush best_match chars into token equiv
			// allow nongreedy keywords to do this if char_buf matches
			let ti = ti.clone();
			self.char_out_buf.extend(ti.map(NonZeroU8::get));
			self.char_out_buf.extend(self.char_buf.iter().copied().skip(kw.len().get() as usize));
		}
		else {
			self.char_out_buf.extend(self.char_buf.iter().copied());
		}
		self.char_buf.clear();
	}

	fn narrow(&mut self, ch: u8) {
		let pinch_idx = self.char_buf.len();
		self.char_buf.push(ch);

		// front byte first
		while let Some((left, remain)) = self.pinch.split_first() {
			let should_narrow_normal = left.0.as_ascii_str().as_bytes()
				.get(pinch_idx as usize).map(|&b| b < ch) != Some(false);

			// skip over RHS token if there is one
			let should_narrow_lhs = self.is_lhs && left.0.position() == TokenPosition::Right;
			// if we did that, there must be a RHS one immediately after
			debug_assert!(!should_narrow_lhs
				|| remain.get(0).map(|(kw, _)| kw.position()) == Some(TokenPosition::Left));

			if should_narrow_normal || should_narrow_lhs {
				// not yet narrowed down to matching substrings
				self.pinch = remain;
			} else {
				break;
			}
		}

		// then back byte
		while let Some((right, remain)) = self.pinch.split_last() {
			if right.0.as_ascii_str().as_bytes()
			.get(pinch_idx as usize).map(|&b| b > ch) == Some(true) {
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
			[(ref kw, ref ti)] if kw.is_greedy() && eq_char_buf!(kw) => {
				self.commit_to(ti);
			},

			// a good match, but there might be a longer one
			[ref pair @ (ref kw, _), ..] if eq_char_buf!(kw) => {
				self.best_match = Some(pair);
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

	fn commit_to(&mut self, ti: &TokenIter) {
		self.best_match = None; // forget that, we have a definite winner
		self.token_buf = Some(ti.clone());
		self.char_buf.clear(); // all bytes accounted for
		self.pinch = PINCH_ALL; // ready for future stuff

		if ti.clone().peek_first() != 0xe9 {
			// all keywords *except* LET move us to RHS
			self.is_lhs = false;
		}
	}

	fn commit_best_match(&mut self) {
		if let Some((kw, best)) = self.best_match.take() {
			if kw.is_greedy() {
				// take this subset of characters, use it
				self.token_buf = Some(best.clone());

				// preserve unconsumed chars to re-add them
				self.char_buf.remove_first(kw.len().get() as usize);
				self.pinch = PINCH_ALL;

				if best.clone().peek_first() != 0xe9 {
					// all keywords *except* LET move us to RHS
					self.is_lhs = false;
				}

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
		self.char_out_buf.extend(mem::replace(
			&mut self.char_buf, Default::default()
			).into_iter());
	}

	fn is_keyword_char(ch: u8) -> bool {
		// it's variable validity that matters here; getting one of these with an empty pinch
		// is *not* a case where we can reset the tokeniser
		// @ is not included; @% is an edge case, but not one we need to worry about
		matches!(ch, b'A'..=b'Z') || b"_$(".contains(&ch)
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
			front: Option<&'static TokenLookupEntry>,
			back: Option<&'static TokenLookupEntry>,
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
				if let Some((front, _)) = self.front {
					write!(f, "\"{}\"", front.as_ascii_str())
				} else {
					f.write_str("(start)")
				}?;

				f.write_str(" to ")?;

				if let Some((back, _)) = self.back {
					write!(f, "\"{}\"", back.as_ascii_str())
				} else {
					f.write_str("(end)")
				}?;

				write!(f, " (#{})", self.count)
			}
		}

		f.debug_struct("TokenScanner")
			.field("char_buf", &HexArray(&*self.char_buf))
			.field("token_buf", &self.token_buf)
			.field("char_out_buf", &&*self.char_out_buf)
			.field("best_match", &self.best_match)
			.field("cur_token", &self.cur_token)
			.field("pinch", &PinchDebug::new(self))
			.finish()
	}
}

#[cfg(test)]
mod tests_try_pull {
	use super::*;

	use nonzero_ext::nonzero;

	#[test]
	fn head() {
		let mut scanner = TokenScanner::new();
		scanner.cur_token = Some(TokenIter::new_direct(nonzero!(1u8)));
		assert_eq!(Some(1), scanner.try_pull());
		assert_eq!(None, scanner.try_pull());

		scanner.cur_token = Some(TokenIter::new_indirect(nonzero!(0xc6u8), nonzero!(0xffu8)));
		assert_eq!(Some(0xc6), scanner.try_pull());
		assert_eq!(Some(0xff), scanner.try_pull());
		assert_eq!(None, scanner.try_pull());
	}

	#[test]
	fn in_queue() {
		let mut scanner = TokenScanner::new();
		scanner.token_buf = Some(TokenIter::new_direct(nonzero!(1u8)));
		assert_eq!(Some(1), scanner.try_pull());
		assert_eq!(None, scanner.try_pull());

		scanner.token_buf = Some(TokenIter::new_indirect(nonzero!(0xc6u8), nonzero!(0xffu8)));
		assert_eq!(Some(0xc6), scanner.try_pull());
		assert_eq!(Some(0xff), scanner.try_pull());
		assert_eq!(None, scanner.try_pull());
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
				scanner.push(ch);
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
	}

	#[test]
	fn found_regressions() {
		assert_output(b"OFWHEN", b"\xca\xc9");
		assert_output(b"ENDPR", b"ENDPR");
		assert_output(b"PRINT\"it works\"", b"\xf1\"it works\"");
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
			scanner.push(ch);
			scanner.try_pull().map(|b| out_buf.push(b));
		}
		scanner.flush();
		while let Some(b) = scanner.try_pull() {
			out_buf.push(b);
		}
		out_buf
	}
}
