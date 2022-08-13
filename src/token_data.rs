use core::fmt;
use core::fmt::Debug;

use arrayvec::ArrayVec;
use ascii::AsciiStr;
use thiserror::Error;

use basc_macros::declare_token_data;
use crate::latin1::CharExt;
use crate::line_numbers;

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub(crate) enum UnpackError {
	#[error("unexpected end of file")]
	UnexpectedEof,
	#[error("invalid line number")]
	InvalidLineNumber,
	#[error("line number out of range")]
	LineNumberOutOfRange(u32),
}

impl From<line_numbers::DecodeError> for UnpackError {
	fn from(src: line_numbers::DecodeError) -> Self {
		match src {
			line_numbers::DecodeError::Eof => Self::UnexpectedEof,
			line_numbers::DecodeError::OutOfRange(r) => Self::LineNumberOutOfRange(r),
		}
	}
}


type ExpandBuf = ArrayVec<u8, 3>;

/// Iterator adapter to detokenise a BASIC file.
///
/// This struct encapsulates a stream of bytes, decoding line numbers and expanding keyword tokens. It outputs Unix newlines (`\n`) and converts Latin-1 non-ASCII bytes in strings to their Unicode equivalents. Control codes in strings are replaced with printable equivalents where possible.
// TODO line numbers and string cleanups
pub(crate) struct TokenUnpacker<I> {
	src: I,
	token_read: ascii::Chars<'static>, // remaining chars from a matched token
	token_key_buf: ExpandBuf, // bytes that might form a token key
	output_buf: ExpandBuf, // bytes that didn't actually form a token key
	have_output_trailing_newline: Option<bool>, // trailing newline after we get None from src
	in_string_literal: InStringLiteral, // are we in a string literal right now?
	line_number: LineNumber, // what's the state of line number parsing?
}

/// Tracks the state of whether parsing is in the middle of a string literal. If it is, token
/// expansion is not performed.
///
/// This is a three-state enum, because what may look like a closing quote mark could also be
/// one of a pair that denote an escaped literal quote mark character.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InStringLiteral {
	/// Not in a string literal; look for token expansions.
	No,
	/// In a string literal; treat all non-ASCII bytes as Latin-1 literal characters.
	Yes,
	/// Either the string literal was just closed, or we are halfway through an escaped quote mark.
	MaybeJustClosed,
}

/// Tracks the state of parsing an encoded line number.
#[derive(Debug)]
enum LineNumber {
	Building(LineNumberBuilding),
	Releasing(LineNumberReleasing),
	None,
}

type LineNumberBuilding = ArrayVec<u8, 3>;
type LineNumberReleasing = arrayvec::IntoIter<u8, 6>; // 18-bit number, remember

impl<I> TokenUnpacker<I>
where I: Iterator<Item = u8> + Debug {
	/// Creates a new unpacker from a byte iterator.
	pub(crate) fn new(src: I) -> Self {
		Self {
			src,
			token_read: <&'static AsciiStr>::default().chars(),
			token_key_buf: ExpandBuf::default(),
			output_buf: ExpandBuf::default(),
			have_output_trailing_newline: None,
			in_string_literal: InStringLiteral::No,
			line_number: LineNumber::Building(ArrayVec::new()),
		}
	}

	#[inline(always)]
	fn should_decode_tokens(&self) -> bool { self.in_string_literal == InStringLiteral::No }

	/// Takes an ASCII string literal from `declare_token_data!`, stores the iterator in
	/// `self`, and returns the first character (which is guaranteed to be present).
	fn read_new_expansion(&mut self, tok: &'static AsciiStr) -> u8 {
		self.token_read = tok.chars();
		match self.token_read.next() {
			Some(ch) => ch,
			None => unsafe {
				debug_assert!(false, "declare_token_data map contains an empty string");
				// SAFETY: the AsciiStr in `tok` is sourced from proc macro `declare_token_data`,
				// which disallows empty strings at compile time
				core::hint::unreachable_unchecked()
			}
		}.as_byte()
	}
}

impl<I> Debug for TokenUnpacker<I>
where I: Iterator<Item = u8> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("TokenUnpacker")
			.field("of found token", &self.token_read.as_str())
			.field("building lookup key", &&*self.token_key_buf)
			.field("of output buffer", &&*self.output_buf)
			.finish()
	}
}

impl<I> Iterator for TokenUnpacker<I>
where I: Iterator<Item = u8> + Debug {
	type Item = Result<char, UnpackError>;

	fn next(&mut self) -> Option<Self::Item> {
		let unclean = (|| loop {
			// check any existing tokens to be flushed out
			if let Some(token_char) = self.token_read.next() {
				return Some(Ok(token_char.as_byte()));
			}

			// check for chars that we tried to turn into a token key but couldn't
			if let non_key @ Some(_) = self.output_buf.pop_at(0) {
				return non_key.map(Result::Ok);
			}

			// check for releasing decoded line number
			if let LineNumber::Releasing(ref mut digits) = self.line_number {
				if let Some(d) = digits.next() {
					return Some(Ok(d));
				} else {
					self.line_number = LineNumber::None;
				}
			}

			let nc = match self.src.next() {
				Some(c) => {
					if self.have_output_trailing_newline.is_none() {
						// there was something on the input, so care about trailing newlines
						self.have_output_trailing_newline = Some(false);
					}

					if c == b'"' {
						// update whether we're in a string literal
						self.in_string_literal = match self.in_string_literal {
							// this quote mark opens it
							InStringLiteral::No => InStringLiteral::Yes,
							// this closes it, or is half of an escaped `"`
							InStringLiteral::Yes => InStringLiteral::MaybeJustClosed,
							// this was an escape, we're still in it
							InStringLiteral::MaybeJustClosed => InStringLiteral::Yes,
						};
					} else if self.in_string_literal == InStringLiteral::MaybeJustClosed {
						// previous `"` wasn't an escape, we really are closed now
						self.in_string_literal = InStringLiteral::No;
					}

					c
				},
				None => {
					// check for any incomplete line numbers
					if matches!(self.line_number, LineNumber::Building(ref x) if x.len() > 0) {
						return Some(Err(UnpackError::UnexpectedEof));
					}

					// there may be incomplete tokens to flush
					self.output_buf = core::mem::replace(&mut self.token_key_buf,
						ExpandBuf::default());
					if ! self.output_buf.is_empty() { continue; }
					if self.have_output_trailing_newline == Some(false) {
						self.have_output_trailing_newline = Some(true);
						return Some(Ok(b'\r')); // use \r here to bypass ^I expansion later
					}
					return None;
				},
			};

			debug_assert!(! matches!(self.line_number, LineNumber::Releasing(_)));
			// are we expecting an encoded line number?
			if let LineNumber::Building(ref mut b) = self.line_number {
				match nc {
					b' ' | b'\t' if b.is_empty() => continue, // ignore leading spaces
					_ => {},
				}
				b.push(nc);
				if b.len() == 3 {
					let mut raw_ln = match super::line_numbers::try_decode_from(
						b.clone().into_iter()
					) {
						Ok(n) => n,
						Err(e) => return Some(Err(e.into())),
					};
					let mut digits = ArrayVec::new();
					if raw_ln == 0 { digits.push(b'0'); }
					while raw_ln > 0 {
						let single_digit = (raw_ln % 10) as u8;
						digits.push(single_digit + b'0');
						raw_ln /= 10;
					}
					digits.reverse();
					self.line_number = LineNumber::Releasing(digits.into_iter());
				}
				continue; // that character was consumed, get another one to output
			}

			// it's a debug assert in theory, but other unskippable bounds checks occur later
			// so we try to combine them
			assert!(self.output_buf.is_empty());

			if self.should_decode_tokens() {
				self.token_key_buf.push(nc);
				// we may have a token here
				match query_token(&mut self.token_key_buf) {
					LookupResult::Direct(tok) => {
						// 1-char token

						// prepare matched token as future byte
						return Some(Ok(self.read_new_expansion(tok)));
					},
					LookupResult::Indirect(tok) => {
						// 3-char token

						// prepare matched token as future byte
						return Some(Ok(self.read_new_expansion(tok)));
					},
					LookupResult::DirectFailure(b) => {
						// 1 byte confirmed not to be a token

						return Some(Ok(b));
					},
					LookupResult::InterruptedIndirect(b) => {
						// to push out is INDIRECT_PREFIX, b
						self.output_buf.clear();
						self.output_buf.push(b);

						return Some(Ok(INDIRECT_PREFIX)); // it was just a byte after all
					}
					LookupResult::IndirectFailure([a, b, c]) => {
						// 3 bytes that aren't a token

						self.output_buf = (&[b, c][..]).try_into().unwrap();
						return Some(Ok(a));
					},
					LookupResult::NotYet => continue,
				}
			} else { return Some(Ok(nc)); }
		})();

		unclean.map(|r| r.map(|c| match c {
			// output unix newlines
			b'\r' => {
				// string literals never wrap lines
				self.in_string_literal = InStringLiteral::No;

				// expect a line number next
				self.line_number = LineNumber::Building(ArrayVec::new());

				'\n'
			},

			// then convert from latin-1
			_ => char::from_risc_os_latin1(c),
		}))
	}
}

const INDIRECT_PREFIX: u8 = 0x8d;
const INDIRECT_C6: u8 = 0xc6;
const INDIRECT_C7: u8 = 0xc7;
const INDIRECT_C8: u8 = 0xc8;

/// The result of attempting to match up to 3 bytes with known token encodings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum LookupResult {
	/// We found a direct (1 byte) token.
	Direct(&'static AsciiStr),
	/// We found an indirect (3 bytes) token.
	Indirect(&'static AsciiStr),
	/// This may be an indirect token, but there aren't enough bytes to know yet.
	NotYet,
	/// This byte could not be a token; return it as-is.
	DirectFailure(u8),
	/// These bytes were not an indirect token; return them as they are.
	IndirectFailure([u8; 3]),
	/// A potential indirect sequence was interrupted; return the first byte as-is.
	InterruptedIndirect(u8),
}

/// Try to look up a keyword from a potential token, returning as much as is known.
fn query_token(src: &mut ExpandBuf) -> LookupResult {
	match src.get(0) {
		Some(&INDIRECT_PREFIX) => {
			let table2 = match src.get(1) {
				Some(&INDIRECT_C6) => Some(&TOKEN_MAP_8D_C6),
				Some(&INDIRECT_C7) => Some(&TOKEN_MAP_8D_C7),
				Some(&INDIRECT_C8) => Some(&TOKEN_MAP_8D_C8),
				_ => None, // fall through to failure branch
			};
			if let Some(table2) = table2 {
				if let Some(found) = src.get(2).and_then(|&idx| table2[idx as usize]) {
					// indirect token matched; clear buffer
					src.clear();
					return LookupResult::Indirect(found);
				}
			}
		},
		Some(&b) => if let Some(found) = TOKEN_MAP_DIRECT[b as usize] {
			// direct token matched; clear buffer
			src.clear();
			return LookupResult::Direct(found);
		}
		None => return LookupResult::NotYet,
	};

	match **src {
		[INDIRECT_PREFIX, ind, INDIRECT_PREFIX] => {
			// incomplete indirect token; consider this an interruption
			src.clear();
			src.push(INDIRECT_PREFIX);
			LookupResult::InterruptedIndirect(ind)
		},

		// no match? okay, how many bytes should we remove?
		[INDIRECT_PREFIX, ind, ind2] if (INDIRECT_C6..=INDIRECT_C8).contains(&ind) => {
			// we formed an indirect lookup, but it didn't match anything
			src.clear();
			LookupResult::IndirectFailure([INDIRECT_PREFIX, ind, ind2])
		},

		[INDIRECT_PREFIX, ind] if (INDIRECT_C6..=INDIRECT_C8).contains(&ind) => {
			// this could be a successful indirect lookup, but it isn't yet. leave it alone
			LookupResult::NotYet
		},

		[INDIRECT_PREFIX] => {
			// this isn't finished yet either. go easy
			LookupResult::NotYet
		},

		[INDIRECT_PREFIX, not_ind] => {
			// the indirect prefix didn't form anything; pop it on its own
			*src = ExpandBuf::default();
			src.push(not_ind);
			LookupResult::DirectFailure(INDIRECT_PREFIX)
		},

		[byte] => {
			// turns out this was just a byte!
			src.clear();
			LookupResult::DirectFailure(byte)
		},

		_ => unreachable!(
			"token lookup is >1 but not leading with indirect prefix!"),

	}
}

#[cfg(test)]
mod test_unpack {
	use super::*;

	#[test]
	fn query_token_matches() {
		let data = [
			([0x80, 0x0d, 0x0d], LookupResult::Direct(
				AsciiStr::from_ascii("AND").unwrap()
			)),
			([0x8a, 0x0d, 0x0d], LookupResult::Direct(
				AsciiStr::from_ascii("TAB(").unwrap()
			)),

			([0x8d, 0xc6, 0x02], LookupResult::Indirect(
				AsciiStr::from_ascii("BEAT").unwrap()
			)),

			([0x8d, 0xc7, 0x16], LookupResult::Indirect(
				AsciiStr::from_ascii("TEXTLOAD").unwrap()
			)),
			([0x8d, 0xc7, 0x17], LookupResult::Indirect(
				AsciiStr::from_ascii("SAVE").unwrap()
			)),
		];

		for (bytes, result) in data.into_iter() {
			let mut av = ExpandBuf::default();
			for b in bytes.into_iter().take_while(|b| *b != 0x0d) {
				av.push(b);
			}
			println!("{:?}", av);

			assert_eq!(result, query_token(&mut av));
			assert!(av.is_empty());
		}
	}

	#[test]
	fn query_token_no_match() {
		let data = [
			([0x20, 0x0d, 0x0d], LookupResult::DirectFailure(0x20)), // ASCII char
			([0xc6, 0x0d, 0x0d], LookupResult::DirectFailure(0xc6)), // no prefix
			([0x8d, 0xc6, 0xff], LookupResult::IndirectFailure([0x8d, 0xc6, 0xff])), // not a token
			([0x8d, 0xc6, 0x0d], LookupResult::NotYet), // sequence did not complete
			([0x8d, 0x40, 0x0d], LookupResult::DirectFailure(0x8d)), // not a fully formed prefix
		];
		for (bytes, result) in data.into_iter() {
			let mut av = ExpandBuf::default();
			for b in bytes.into_iter().take_while(|b| *b != 0x0d) {
				av.push(b);
			}
			println!("{:?}", av);

			assert_eq!(result, query_token(&mut av));
		}
	}

	fn expand(expected: &'static str, src: &[u8]) {
		println!("\ncase: {:?}", expected);
		let expander = super::TokenUnpacker::new(src.iter().copied());
		let result = expander.collect::<Result<String, _>>();
		assert_eq!(Ok(expected), result.as_deref());
	}

	fn expand_err(expected: UnpackError, src: &[u8]) {
		let expander = super::TokenUnpacker::new(src.iter().copied());
		let result = expander.collect::<Result<String, _>>();
		assert_eq!(Err(expected), result);
	}

	#[test]
	fn expand_pure_ascii() {
		expand("10hello\n", b"TJ@hello");
	}

	#[test]
	fn expand_direct() {
		expand("10PRINT CHR$32\n", b"TJ@\xf1 \xbd32");
		expand("10PRINTCHR$32\n", b"TJ@\xf1\xbd32");
	}

	#[test]
	fn expand_indirect() {
		expand("10SYS87\n", b"TJ@\x8d\xc8\x1487")
	}

	#[test]
	fn multiline() {
		expand("10line 1\n20line 2\n", b"TJ@line 1\rTT@line 2");
		expand("", b"");
	}

	#[test]
	fn abandoned_indirects() {
		expand("10™\n", b"TJ@\x8d");
		expand("10™Ç\n11™È\n", b"TJ@\x8d\xc7\rTK@\x8d\xc8")
	}

	#[test]
	fn failed_direct() {
		expand("1Æ\n", b"TA@\xc6");
		expand("1Ç\n", b"TA@\xc7");
		expand("1È\n", b"TA@\xc8");
	}

	#[test]
	fn interrupt_indirect_with_another() {
		expand("0™SUM\n", b"T@@\x8d\x8d\xc6\x03");
		expand("0™ÇSUM\n", b"T@@\x8d\xc7\x8d\xc6\x03");
	}

	#[test]
	fn just_look_around_you() {
		// TODO line number references don't work like this
		expand("10 PRINT \"LOOK AROUND YOU \";\n20 GOTO 10\n",
				b"TJ@ \xf1 \"LOOK AROUND YOU \";\rTT@ \xe5 10");
	}

	#[test]
	fn string_literals() {
		// basic case
		expand("1\"test\"\n", b"TA@\"test\"");
		// escaped literal quote
		expand("1\"A\"\"B\"\"\"\"C\"\n", b"TA@\"A\"\"B\"\"\"\"C\"");
		// unterminated literal handled realtively gracefully
		expand("1PRINT \"unclosed\n2END\n", b"TA@\xf1 \"unclosed\rTB@\xe0");
	}

	#[test]
	fn no_decode_in_string_literals() {
		expand("1LOAD \"™Ç\u{2418}\"\n", b"TA@\x8d\xc7\x18 \"\x8d\xc7\x18\"")
	}

	#[test]
	fn bad_line_number() {
		expand_err(UnpackError::UnexpectedEof, b"T");
		expand_err(UnpackError::UnexpectedEof, b"TJ");
	}
}

declare_token_data! {
	// non-prefixed ones first
	0x80 => "AND",
	0x81 => "DIV",
	0x82 => "EOR",
	0x83 => "MOD",
	0x84 => "OR",
	0x85 => "ERROR",
	0x86 => "LINE",
	0x87 => "OFF",
	0x88 => "STEP",
	0x89 => "SPC",
	0x8a => "TAB(",
	0x8b => "ELSE",
	0x8c => "THEN",
	// 0x8d is a prefix, we hardcode that special case
	0x8e => "OPENIN",
	0x8f => "PTR",
	0x90 => "PAGE",
	0x91 => "TIME",
	0x92 => "LOMEM",
	0x93 => "HIMEM",
	0x94 => "ABS",
	0x95 => "ACS",
	0x96 => "ADVAL",
	0x97 => "ASC",
	0x98 => "ASN",
	0x99 => "ATN",
	0x9a => "BGET",
	0x9b => "COS",
	0x9c => "COUNT",
	0x9d => "DEG",
	0x9e => "ERL",
	0x9f => "ERR",
	0xa0 => "EVAL",
	0xa1 => "EXP",
	0xa2 => "EXT",
	0xa3 => "FALSE",
	0xa4 => "FN",
	0xa5 => "GET",
	0xa6 => "INKEY",
	0xa7 => "INSTR(",
	0xa8 => "INT",
	0xa9 => "LEN",
	0xaa => "LN",
	0xab => "LOG",
	0xac => "NOT",
	0xad => "OPENUP",
	0xae => "OPENOUT",
	0xaf => "PI",
	0xb0 => "POINT(",
	0xb1 => "POS",
	0xb2 => "RAD",
	0xb3 => "RND",
	0xb4 => "SGN",
	0xb5 => "SIN",
	0xb6 => "SQR",
	0xb7 => "TAN",
	0xb8 => "TO",
	0xb9 => "TRUE",
	0xba => "USR",
	0xbb => "VAL",
	0xbc => "VPOS",
	0xbd => "CHR$",
	0xbe => "GET$",
	0xbf => "INKEY$",
	0xc0 => "LEFT$(",
	0xc1 => "MID$(",
	0xc2 => "RIGHT$(",
	0xc3 => "STR$",
	0xc4 => "STRING$(",
	0xc5 => "EOF",

	prefix(0xc6) => {
		0x02 => "BEAT",
		0x03 => "SUM",
	},

	prefix(0xc7) => {
		0x02 => "AUTO",
		0x03 => "APPEND",
		0x10 => "TWIN",
		0x11 => "TEXTSAVE",
		0x13 => "TWINO",
		0x14 => "RENUMBER",
		0x15 => "OLD",
		0x16 => "TEXTLOAD",
		0x17 => "SAVE",
		0x18 => "LOAD",
		0x19 => "LIST",
		0x1a => "NEW",
		0x1b => "LVAR",
		0x1c => "DELETE",
		0x1d => "CRUNCH",
		0x1e => "HELP",
		0x1f => "EDIT",
		0x84 => "HELP",
		0x87 => "APPEND",
		0x95 => "AUTO",
		0xa5 => "EDIT",
	},

	prefix(0xc8) => {
		0x02 => "CIRCLE",
		0x03 => "CASE",
		0x10 => "ELLIPSE",
		0x11 => "TINT",
		0x12 => "TEMPO",
		0x13 => "BEATS",
		0x14 => "SYS",
		0x15 => "QUIT",
		0x16 => "LIBRARY",
		0x17 => "INSTALL",
		0x18 => "WHILE",
		0x19 => "SWAP",
		0x1a => "MOUSE",
		0x1b => "WAIT",
		0x1c => "ORIGIN",
		0x1d => "FILL",
		0x1e => "RECTANGLE",
		0x1f => "POINT",
		0x2c => "VOICE",
		0x2d => "VOICES",
		0x2e => "OVERLAY",
		0x2f => "STEREO",
		0x87 => "INSTALL",
		0xc4 => "QUIT",
		0xc7 => "BEATS",
	},

	0xc9 => "WHEN",
	0xca => "OF",
	0xcb => "ENDCASE",
	0xcc => "ELSE",
	0xcd => "ENDIF",
	0xce => "ENDWHILE",
	0xcf => "PTR",
	0xd0 => "PAGE",
	0xd1 => "TIME",
	0xd2 => "LOMEM",
	0xd3 => "HIMEM",
	0xd4 => "SOUND",
	0xd5 => "BPUT",
	0xd6 => "CALL",
	0xd7 => "CHAIN",
	0xd8 => "CLEAR",
	0xd9 => "CLOSE",
	0xda => "CLG",
	0xdb => "CLS",
	0xdc => "DATA",
	0xdd => "DEF",
	0xde => "DIM",
	0xdf => "DRAW",
	0xe0 => "END",
	0xe1 => "ENDPROC",
	0xe2 => "ENVELOPE",
	0xe3 => "FOR",
	0xe4 => "GOSUB",
	0xe5 => "GOTO",
	0xe6 => "GCOL",
	0xe7 => "IF",
	0xe8 => "INPUT",
	0xe9 => "LET",
	0xea => "LOCAL",
	0xeb => "MODE",
	0xec => "MOVE",
	0xed => "NEXT",
	0xee => "ON",
	0xef => "VDU",
	0xf0 => "PLOT",
	0xf1 => "PRINT",
	0xf2 => "PROC",
	0xf3 => "READ",
	0xf4 => "REM",
	0xf5 => "REPEAT",
	0xf6 => "REPORT",
	0xf7 => "RESTORE",
	0xf8 => "RETURN",
	0xf9 => "RUN",
	0xfa => "STOP",
	0xfb => "COLOUR",
	0xfc => "TRACE",
	0xfd => "UNTIL",
	0xfe => "WIDTH",
	0xff => "OSCLI",
}

#[cfg(test)]
mod test_proc_macro_output {
	use super::{TOKEN_MAP_DIRECT, TOKEN_MAP_8D_C6, TOKEN_MAP_8D_C7, TOKEN_MAP_8D_C8};
	use ascii::AsciiStr;

	#[test]
	fn test_direct() {
		let data = [
			(0x80u8, "AND"),
			(0x8cu8, "THEN"),
			(0xc5u8, "EOF"),
		];
		for (byte, word) in data.into_iter() {
			assert_eq!(Some(word), TOKEN_MAP_DIRECT[byte as usize].map(AsciiStr::as_str));
		}
		assert_eq!(None, TOKEN_MAP_DIRECT[0x8d]);
	}

	#[test]
	fn test_indirect() {
		let data = [
			(&TOKEN_MAP_8D_C6, 0x02u8, "BEAT"),
			(&TOKEN_MAP_8D_C6, 0x03u8, "SUM"),
			(&TOKEN_MAP_8D_C7, 0x03u8, "APPEND"),
			(&TOKEN_MAP_8D_C7, 0x1cu8, "DELETE"),
			(&TOKEN_MAP_8D_C8, 0x03u8, "CASE"),
			(&TOKEN_MAP_8D_C8, 0x14u8, "SYS"),
		];
		for (arr, byte, word) in data.into_iter() {
			assert_eq!(Some(word), arr[byte as usize].map(AsciiStr::as_str));
		}
	}

	#[test]
	fn proof_we_disallowed_empty_strings() {
		fn all_str_lengths(table: &'static [Option<&'static AsciiStr>; 256])
			-> impl Iterator<Item = usize> {
				table.iter().filter_map(Option::as_deref).map(AsciiStr::len)
			}

		assert!(all_str_lengths(&TOKEN_MAP_DIRECT)
			.chain(all_str_lengths(&TOKEN_MAP_8D_C6))
			.chain(all_str_lengths(&TOKEN_MAP_8D_C7))
			.chain(all_str_lengths(&TOKEN_MAP_8D_C8))
			.all(|sl| sl > 0));
	}

	#[test]
	fn check_flagged_goto_gosub() {
		use super::LINE_DEPENDENT_KEYWORD_BYTES;
		for keyword in ["GOTO", "GOSUB"] {
			let as_ascii_str = AsciiStr::from_ascii(keyword).unwrap();
			let byte = TOKEN_MAP_DIRECT.iter()
				.map(Option::as_deref)
				.position(|k| k == Some(as_ascii_str))
				.and_then(|u| u8::try_from(u).ok())
				.unwrap();
			assert!(LINE_DEPENDENT_KEYWORD_BYTES.iter().find(|&&l| l == byte).is_some(),
				"could not find {} byte equiv ({:02x}) in LINE_DEPENDENT_KEYWORD_BYTES ({:?})",
				keyword, byte, LINE_DEPENDENT_KEYWORD_BYTES);
		}
	}
}
