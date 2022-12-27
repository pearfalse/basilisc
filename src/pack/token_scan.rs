use std::num::NonZeroU8;
use std::{fmt, mem};

use arrayvec::ArrayVec;

use crate::token_data::TokenLookupEntry;
use crate::{
	token_iter::TokenIter,
	support::{ArrayVecExt, HexArray},
};

// there is a known pathological case where 2 tokens appear for one byte, but that should be it
type TokenMatchBuffer = ArrayVec<TokenIter, 2>;

// chars that didn't match anything
type CharBuffer = ArrayVec<u8, { crate::keyword::MAX_LEN as usize }>;

struct TokenScanner {
	char_buf: CharBuffer,
	token_buf: TokenMatchBuffer,
	char_out_buf: CharBuffer,
	best_match: Option<&'static TokenLookupEntry>,
	cur_token: Option<TokenIter>,
	pinch: &'static [TokenLookupEntry],
}

static PINCH_ALL: &'static [TokenLookupEntry] = crate::token_data::LOOKUP_MAP.as_slice();

impl TokenScanner {
	pub fn new() -> Self {
		Self {
			char_buf: CharBuffer::new(),
			token_buf: TokenMatchBuffer::new(),
			char_out_buf: CharBuffer::new(),
			best_match: None,
			cur_token: None,
			pinch: PINCH_ALL,
		}
	}

	pub fn try_pull(&mut self) -> Option<u8> {
		'ti: loop {
			if let Some(ch) = self.cur_token.as_mut().and_then(Iterator::next) {
				return Some(ch.get());
			}

			// it's exhausted, try pulling from token_buf
			if let new @ Some(_) = self.token_buf.pop_front() {
				self.cur_token = new;
			} else {
				break 'ti; // can't do anything else with tokens
			}
		}

		self.char_out_buf.pop_front()
	}

	pub fn push(&mut self, ch: u8) {
		match (Self::is_keyword_char(ch), self.pinch.is_empty()) {
			(true, true) => {
				// already failed, possible variable name, do not restart searching
				self.char_out_buf.push(ch);
			},
			(false, true) => {
				// reset pinch, but this char will never match
				self.pinch = PINCH_ALL;
				self.char_out_buf.push(ch);
			},
			(true, false) => {
				// it's narrowing time babey
				self.narrow(ch);
				self.try_extract();
			},
			(false, false) => {
				// this char will never match; flush all consideration chars
				self.commit_best_match();
				self.char_buf.push(ch);
			},
		}
	}

	pub fn flush(&mut self) {
		if let Some((kw, ti)) = self.best_match {
			// crush best_match chars into token equiv
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
			if left.0.as_ascii_str().as_bytes()
			.get(pinch_idx as usize).map(|&b| b < ch) != Some(false) {
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
			($kw:expr) => { $kw.as_bytes() == &*self.char_buf };
		}

		match *self.pinch {
			// perfect match, greedy match
			[(ref kw, ref ti)] if eq_char_buf!(kw) => {
				self.best_match = None; // forget that, we have a definite winner
				self.token_buf.push(ti.clone());
				self.char_buf.clear(); // all bytes accounted for
				self.pinch = PINCH_ALL; // ready for future stuff
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

	fn commit_best_match(&mut self) {
		if let Some((kw, best)) = self.best_match.take() {
			// take this subset of characters, use it
			self.token_buf.push(best.clone());

			// preserve unconsumed chars to re-add them
			self.char_buf.remove_first(kw.len().get() as usize);
			self.pinch = PINCH_ALL;
		}
		else {
			// move all chars as-is
			self.char_out_buf.extend(mem::replace(
				&mut self.char_buf, Default::default()
				).into_iter());
		}
	}

	fn is_keyword_char(ch: u8) -> bool {
		// it's variable validity that matters here; getting one of these with an empty pinch
		// is *not* a case where we can reset the tokeniser
		// @ is not included; @% is an edge case, but not one we need to worry about
		matches!(ch, b'A'..=b'Z' | b'a'..=b'z')
			|| b"_$(.".contains(&ch)
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
			.field("token_buf", &&*self.token_buf)
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
		scanner.token_buf.push(TokenIter::new_direct(nonzero!(1u8)));
		assert_eq!(Some(1), scanner.try_pull());
		assert_eq!(None, scanner.try_pull());

		scanner.token_buf.push(TokenIter::new_indirect(nonzero!(0xc6u8), nonzero!(0xffu8)));
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
		assert_eq!(b"\x97II", &*_inout(b"ASCII"));
	}

	#[test]
	fn smush_could_last_longer() {
		assert_eq!(b"\xe0ING", &*_inout(b"ENDING"));
	}

	#[test]
	fn inkey() {
		assert_eq!(b"\xa6#", &*_inout(b"INKEY#"));
		assert_eq!(b"\xbf", &*_inout(b"INKEY$"));
	}

	#[test]
	fn not_in_middle() {
		const SRC: &'static [u8] = b"PTOLEMY"; // `TO` should not tokenise
		let out_buf = _inout(SRC);
		assert_eq!(SRC, &*out_buf);
	}

	#[test]
	fn char_out_buf_limits() {
		static SRC: &'static [u8] = b"OTHERWISOTHERWISOTHERWISOTHERWIS";
		const CHUNK: usize = 8;

		for limit in (1..(SRC.len())).step_by(CHUNK) {
			let src = &SRC[..limit];

			assert_eq!(src, &*_inout(src));
		}
	}

	#[test]
	#[ignore = "this will come later"]
	fn everyones_favourite_awful_edge_case() {
		// the text ENDPI switches from looking like `ENDPROC` to being `END` + `PI` in a single
		// char input :(
		assert_eq!(b"\xe0\xaf", &*_inout(b"ENDPI"));
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
