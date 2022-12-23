use std::mem;

use arrayvec::ArrayVec;

use crate::token_data::TokenLookupEntry;
use crate::support::{TokenIter, ArrayVecExt};

use super::TokenScanBuffer;

// there is a known pathological case where 2 tokens appear for one byte, but that should be it
type TokenMatchBuffer = ArrayVec<TokenIter, 2>;

// chars that didn't match anything
type MatchFailureChars = ArrayVec<u8, { crate::support::MAX_KEYWORD_LEN as usize }>;

#[derive(Debug)]
struct TokenScanner {
	char_buf: TokenScanBuffer,
	token_buf: TokenMatchBuffer,
	char_out_buf: MatchFailureChars,
	best_match: Option<TokenIter>,
	cur_token: Option<TokenIter>,
	pinch: &'static [TokenLookupEntry],
}

static PINCH_ALL: &'static [TokenLookupEntry] = crate::token_data::LOOKUP_MAP.as_slice();

impl TokenScanner {
	pub fn new() -> Self {
		Self {
			char_buf: TokenScanBuffer::new(),
			token_buf: TokenMatchBuffer::new(),
			char_out_buf: MatchFailureChars::new(),
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
				self.all_chars_in_to_out();
			},
		}
	}

	fn narrow(&mut self, ch: u8) {
		let pinch_idx = self.char_buf.len();
		self.char_buf.push(ch);

		// front byte first
		while let Some((left, remain)) = self.pinch.split_first() {
			if left.0.as_bytes().get(pinch_idx as usize).map(|&b| b < ch) != Some(false) {
				// not yet narrowed down to matching substrings
				self.pinch = remain;
			} else {
				break;
			}
		}

		// then back byte
		while let Some((right, remain)) = self.pinch.split_last() {
			if right.0.as_bytes().get(pinch_idx as usize).map(|&b| b > ch) == Some(true) {
				// too flabby on the right
				self.pinch = remain;
			} else {
				break;
			}
		}
	}

	fn try_extract(&mut self) {
		match *self.pinch {
			// perfect match, greedy match
			[(ref kw, ref ti)] if kw.as_bytes() == &*self.char_buf => {
				self.best_match = None; // forget that, we have a definite winner
				self.token_buf.push(ti.clone());
				self.char_buf.clear(); // all bytes accounted for
				self.pinch = PINCH_ALL; // ready for future stuff
			},

			// a good match, but there might be a longer one
			[(ref kw, ref ti), ..] if kw.as_bytes() == &*self.char_buf => {
			},

			// confirmed no match; purge all known chars
			[] => {
				// TODO this is where the ENDPI case needs handling
				self.all_chars_in_to_out();
			},

			// still searching, no match yet
			[_, ..] => {
			},
		}
	}

	fn all_chars_in_to_out(&mut self) {
		self.char_out_buf.extend(mem::replace(
			&mut self.char_buf, TokenScanBuffer::new()
		).into_iter());
	}

	fn is_keyword_char(ch: u8) -> bool {
		// it's variable validity that matters here; getting one of these with an empty pinch
		// is *not* a case where we can reset the tokeniser
		// @ is not included; @% is an edge case, but not one we need to worry about
		matches!(ch, b'A'..=b'Z' | b'a'..=b'z' | b'_')
	}
}

impl Default for TokenScanner {
	#[inline(always)]
	fn default() -> Self {
		Self::new()
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
	fn everyones_favourite_awful_edge_case() {
		// the text ENDPI switches from looking like `ENDPROC` to being `END` + `PI` in a single
		// char input :(
		let mut scanner = TokenScanner::new();
		for &ch in b"ENDPI" {
			assert_eq!(None, scanner.try_pull());
			scanner.push(ch);
		}

		assert_eq!(Some(0xe0), scanner.try_pull());
		assert_eq!(Some(0xaf), scanner.try_pull());
	}
}