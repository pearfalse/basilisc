//! Handles packing a textual BBC BASIC representation into a tokenised BASIC file (type `&FFB`).

use crate::token_data::{TokenLookupEntry, self};

use std::{num::NonZeroU16, io};

use arrayvec::ArrayVec;
use nonzero_ext::nonzero;

use crate::{
	support::{NextByte, ArrayVecExt as _},
	token_iter::TokenIter,
};

mod gaps;
use gaps::*;

mod token_scan;

const MAX_LINE_LEN: usize = 251;
const LINE_NUMBER_CAP: u16 = 0xff00;

#[derive(Debug, thiserror::Error)]
pub enum Error {
	#[error("too many lines in program (limit is 65280)")]
	TooManyLines,
	#[error("too many lines to fully number the program (had room for {max_possible}, but needed {needed}")]
	TooManyUnnumberedLines { max_possible: u16, needed: u32 },
	#[error("line too long (must be under 252 chars, but was {length})")]
	LineTooLong { length: u16 },
	#[error("line number too large ({found}, must be less than 65280)")]
	LineNumberOutOfRange { found: u32 },
	#[error("invalid source character '{}'", (.0).escape_unicode())]
	InvalidChar(char),
	#[error("I/O error: {0}")]
	IoError(#[from] std::io::Error),
}

impl From<std::convert::Infallible> for Error {
	fn from(src: std::convert::Infallible) -> Self {
		match src { }
	}
}

impl PartialEq for Error {
	fn eq(&self, other: &Self) -> bool {
		use Error::*;
		match (self, other) {
			(
				&TooManyUnnumberedLines { max_possible: max1, needed: need1 },
				&TooManyUnnumberedLines { max_possible: max2, needed: need2 },
			) => max1 == max2 && need1 == need2,
			(
				&LineTooLong { length: len1 },
				&LineTooLong { length: len2 },
			) => len1 == len2,
			(
				&LineNumberOutOfRange { found: a },
				&LineNumberOutOfRange { found: b },
			) => a == b,
			(
				&InvalidChar(a), &InvalidChar(b),
			) => a == b,
			_ => false,
		}
	}
}

#[derive(Debug)]
pub struct Line {
	line_number: u16,
	contents: Box<[u8]>,
}

impl Line {
	pub fn write(&self, target: &mut dyn io::Write) -> io::Result<()> {
		debug_assert!(self.contents.len() <= MAX_LINE_LEN);
		let hdr_len = unsafe {
			// SAFETY: only internal code can create `contents`, and we know it's short enough
			(self.contents.len() as u8).checked_add(4).unwrap_unchecked() // header length
		};
		let line_number = self.line_number.to_be_bytes();
		let header = [0x0d, line_number[0], line_number[1], hdr_len];
		target.write_all(&header)?;
		target.write_all(&*self.contents)
	}
}

#[derive(Debug)]
struct UnnumberedLine {
	line_number: Option<u16>,
	contents: Box<[u8]>,
}

type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Parser<I> {
	buf: ArrayVec<u8, MAX_LINE_LEN>,
	lines: Vec<UnnumberedLine>,
	token_scan: TokenScan,
	inner: I,
	is_eof: bool,
}

impl<I> Parser<I> where
	I: NextByte,
	Error: From<<I as NextByte>::Error>,
{
	pub fn new(src: I) -> Self {
		Self {
			buf: ArrayVec::new(),
			lines: Vec::new(),
			token_scan: TokenScan::new(),
			inner: src,
			is_eof: false,
		}
	}

	pub fn next_line(&mut self) -> Result<bool> {
		// we need early return for fuse behaviour
		if self.is_eof { return Ok(false); }

		if self.lines.len() == LINE_NUMBER_CAP as usize {
			// cannot add more lines, no way no how
			return Err(Error::TooManyLines);
		}

		if let Some(maybe_ln) = self.raw_line()? {
			self.lines.push(UnnumberedLine {
				line_number: maybe_ln,
				contents: (&*self.buf).into(),
			});
		} else {
			self.is_eof = true;
		}
		Ok(!self.is_eof)
	}

	fn raw_line(&mut self) -> Result<Option<Option<u16>>> {
		// TODO: encode line number references properly
		// TODO: read incoming characters as UTF-8, not bytes
		#[derive(Debug)]
		enum LineParser {
			BeforeLineNumber,
			ParsingLineNumber { stage: u16 },
			InLineBody { line_number: Option<u16> },
		}

		let mut state = LineParser::BeforeLineNumber;
		self.buf.clear();
		self.is_eof = true; // assume EOF, prove wrong when breaking on `\n`
		while let Some(byte) = self.inner.next_byte()? {
			match byte {
				b'\r' => continue, // blunt way of handling CRLF
				b'\n' => { self.is_eof = false; break }, // end of line
				_ => {},
			};
			match state {
				LineParser::BeforeLineNumber => match byte {
					b' ' | b'\t' => {},
					b'0'..=b'9' => {
						// make digits, add to digit
						state = LineParser::ParsingLineNumber { stage: (byte - b'0') as u16 };
						continue;
					},
					other => {
						// no line number, line has started
						state = LineParser::InLineBody { line_number: None };
						self.update_body(other)?;
					}
				},
				LineParser::ParsingLineNumber { ref mut stage } => match byte {
					b'0'..=b'9' => {
						let new = ((*stage) as u32) * 10 + (byte - b'0') as u32;
						*stage = u16::try_from(new).ok()
							.filter(|&ln| ln < LINE_NUMBER_CAP)
							.ok_or_else(|| Error::LineNumberOutOfRange { found: new })?;
					},
					other => {
						state = LineParser::InLineBody { line_number: Some(*stage) };
						self.update_body(other)?;
					},
				},
				LineParser::InLineBody { line_number: _ } => {
					self.update_body(byte)?
				},
			};
		}

		self.token_scan.flush();

		let flushed_bytes = (&mut self.token_scan).collect::<TokenScanBuffer>();
		self.try_add_bytes(&*flushed_bytes)?;

		let final_line_number = match state {
			LineParser::BeforeLineNumber => return Ok(None),
			LineParser::ParsingLineNumber { stage } => Some(stage),
			LineParser::InLineBody { line_number } => line_number,
		};

		Ok(Some(final_line_number))
	}

	pub fn write(self, target: &mut dyn io::Write) -> Result<()> {
		let lines = self.into_lines()?;

		for line in lines {
			target.write_all(&*line.contents)?;
		}

		target.write_all(&[0x0d, 0xff])?;
		Ok(())
	}

	fn into_lines(mut self) -> Result<Vec<Line>> {
		for gap in FindGaps::within(&mut *self.lines).collect::<Vec<Gap>>() {
			let to_apply = infer_line_number_range(gap.before, gap.after,
				gap.lines.len().try_into().unwrap())?;
			for (ln, target) in to_apply.zip(gap.lines.iter_mut()) {
				target.line_number = Some(ln);
			}

		}

		Ok(self.lines.into_iter().enumerate().map(|(idx, ul)| {
			let line_number = match ul.line_number {
				Some(ln) => ln,
				None => panic!("No line number for line at index {}", idx),
			};

			Line { line_number, contents: ul.contents }
		}).collect::<Vec<Line>>())
	}

	fn update_body(&mut self, byte: u8) -> Result<()> {
		self.token_scan.narrow(byte);

		// narrowing can release a token too
		while let Some(b) = self.token_scan.try_pull() {
			self.try_add_bytes(&[b])?;
		}

		Ok(())
	}

	fn try_add_bytes(&mut self, bytes: &[u8]) -> Result<()> {
		let error = {
			let add_len: u16 = bytes.len().try_into().expect("can't add over 65K bytes!");
			let old_buf_len = self.buf.len() as u16;
			move || Error::LineTooLong {
				length: match old_buf_len.saturating_add(add_len) {
					fits if fits <= u16::MAX => fits,
					too_big => panic!("tried to create line length {}, which is impossibly large",
						too_big),
				},
			}
		};

		self.buf.try_extend_from_slice(bytes).map_err(|_| error())
	}
}


#[cfg(test)]
mod test_parser {
	use super::*;

	#[test]
	fn bogus_line_numbers() {
		use std::fmt::Write;

		let mut buf = arrayvec::ArrayString::<10>::new();
		for (attempt, reach) in [
			(0xff00, 0xff00),
			(u32::MAX, 429496),
		] {
			buf.clear();
			println!("case: attempt {}, reach {}", attempt, reach);
			write!(buf, "{}", attempt).unwrap();
			assert_eq!(
				Err(Error::LineNumberOutOfRange { found: reach }),
				Parser::new(buf.as_str().as_bytes().iter()).next_line(),
			);
		}
	}

	#[test]
	fn no_tokens() {
		let mut parser = Parser::new(b"100 hooray".as_slice().iter());
		assert_eq!(Ok(false), parser.next_line());
		let line = match &*parser.lines {
			&[_] => parser.lines.pop(),
			_ => None,
		}.expect("couldn't find single line");
		assert_eq!(Some(100), line.line_number);
		assert_eq!(&b" hooray"[..], &*line.contents);
	}

	#[test]
	fn eof_vs_eol() {
		let mut parser = Parser::new(b"10\n!\n20".as_slice().iter());
		// this is line 10
		assert_eq!(Ok(true), parser.next_line());
		// this is a blank-ish line (line 15?)
		assert_eq!(Ok(true), parser.next_line());
		// this is line 20, with no explicit \n
		assert_eq!(Ok(false), parser.next_line());
		// this shouldn't try to read anything
		assert_eq!(Ok(false), parser.next_line());

		let [line1, line2, line3]: [UnnumberedLine; 3] = parser.lines.try_into().unwrap();

		assert_eq!(Some(10), line1.line_number);
		assert_eq!(None, line2.line_number);
		assert_eq!(Some(20), line3.line_number);

		assert!(line1.contents.is_empty());
		assert_eq!(&[b'!'], &*line2.contents);
		assert!(line3.contents.is_empty());
	}

	#[test]
	fn oh_boy_tokens() {
		let mut parser = Parser::new(b"PRINT\"it works\"\nEND".as_slice().iter());
		assert_eq!(Ok(true), parser.next_line());
		assert_eq!(Ok(false), parser.next_line());

		let [line1, line2]: [UnnumberedLine; 2] = parser.lines.try_into().unwrap();

		assert!(line1.line_number.is_none());
		assert!(line2.line_number.is_none());

		assert_eq!(b"\xf1\"it works\"".as_slice(), &*line1.contents);
		assert_eq!(&[0xe0][..], &*line2.contents);
	}

	#[test]
	fn problematic_prefixes() {
		let mut parser = Parser::new(b"10EN\n20END\n30ENDPR\n40ENDPROC\n50ENDPROCK\n60ENDPI"
			.as_slice().iter());
		for _ in 0..5 {
			assert_eq!(Ok(true), parser.next_line());
		}
		assert_eq!(Ok(false), parser.next_line());

		const EXPECT: [&'static [u8]; 6] = [b"EN", b"\xe0", b"\xe0PR", b"\xe1", b"\xe1K",
			b"\xe0\xaf"];
		let mut ln = (10u16..).step_by(10);
		let mut expect = EXPECT.iter().copied();
		for line in parser.lines {
			assert_eq!(ln.next(), line.line_number);
			assert_eq!(expect.next().unwrap(), &*line.contents);
		}
	}
}

type TokenScanBuffer = arrayvec::ArrayVec<u8, { crate::keyword::MAX_LEN as usize }>;

#[derive(Debug)]
struct TokenScan {
	/// Bytes of untokenised user input.
	bytes: TokenScanBuffer,
	/// A decreasing slice of the assumed-sorted token map array, containing all tokens for which
	/// the bytes in `bytes` are a valid subset.
	pinch: &'static [TokenLookupEntry],
	/// The best-matched token so far, which may get replaced with a longer matching one.
	best_match: Option<&'static TokenLookupEntry>,
	/// A matched token to read from before doing anything else.
	token: TokenIter,
}

static PINCH_DEFAULT: &'static [TokenLookupEntry] = token_data::LOOKUP_MAP.as_slice();

impl TokenScan {
	fn new() -> Self {
		Self {
			bytes: TokenScanBuffer::default(),
			pinch: PINCH_DEFAULT,
			best_match: None,
			token: TokenIter::default(), // empty iterator
		}
	}

	fn narrow(&mut self, byte: u8) {
		self.bytes.push(byte);
		self._pinch(self.bytes.len() - 1);

		match self.pinch {
			[perfect] if perfect.0.as_bytes() == &*self.bytes => {
				// perfect match
				self.token = perfect.1.clone();
				self.best_match = None;
				self.bytes.clear();
				self.pinch = PINCH_DEFAULT;
				self._pinch(0); // re-pinch
			},

			[prefix, ..] if prefix.0.as_bytes() == &*self.bytes => {
				// at least one match, but there might be more
				self.best_match = Some(prefix);
			},

			[] if self.best_match.is_some() => {
				// actively apply best match
				let best_match = self.best_match.take().unwrap();
				self.token = best_match.1.clone();
				self.bytes.remove_first(best_match.0.len().get() as usize);

				// remake pinch sequence for remaining characters
				self.pinch = PINCH_DEFAULT;
				self._pinch(0);

				// after pinching remaining bytes, re-match best effort
				if let [be, ..] = self.pinch {
					if self.bytes.starts_with(be.0.as_bytes()) {
						self.best_match = Some(be);
					}
				}
			}
			_ => {},
		};
	}

	#[must_use]
	fn try_pull(&mut self) -> Option<u8> {
		// flushing a token?
		if let Some(next) = self.token.next() {
			return Some(next.get());
		}

		// if pinch is empty and buffer isn't, we're still flushing
		if self.pinch.is_empty() {
			if let next @ Some(_) = self.bytes.pop_front() {
				return next;
			}
			// if here, we've finished flushing known-not-token bytes
			self.pinch = PINCH_DEFAULT;
		}
		None
	}

	fn flush(&mut self) {
		if let Some(&(ref keyword, ref iter)) = self.best_match.take() {
			// the token is almost definitely shorter than the word it replaces
			// how much shifting will we have to do?
			self.token = iter.clone();

			// remove effective padding
			self.bytes.remove_first(keyword.len().get() as usize);
			self._pinch(0);
		}
		else {
			// give up on partial keyword matches
			self.pinch = &[];
		}
	}

	fn _pinch(&mut self, start: usize) {
		for (pinch_idx, byte) in self.bytes.iter().enumerate().skip(start) {
			// front byte first
			while let Some((left, remain)) = self.pinch.split_first() {
				if left.0.as_bytes().get(pinch_idx as usize).map(|b| b < byte) != Some(false) {
					// not yet narrowed down to matching substrings
					self.pinch = remain;
				} else {
					break;
				}
			}

			// then back byte
			while let Some((right, remain)) = self.pinch.split_last() {
				if right.0.as_bytes().get(pinch_idx as usize).map(|b| b > byte) == Some(true) {
					// too flabby on the right
					self.pinch = remain;
				} else {
					break;
				}
			}

			if self.pinch.is_empty() { break; }
		}
	}
}

impl Iterator for TokenScan {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		self.try_pull()
	}
}


#[cfg(test)]
mod test_token_scan {
    use crate::pack::TokenScanBuffer;
	use super::TokenScan;

	#[test]
	fn find_in_range() {
		for (word, token_byte) in [
			(b"CHAIN".as_slice(), 215),
			(b"WHEN", 201),
			(b"INT", 168),
		] {
			let mut scanner = TokenScan::new();
			for &b in word {
				assert!(scanner.try_pull().is_none());
				scanner.narrow(b);
			}

			assert_eq!(Some(token_byte), scanner.try_pull());
			assert_eq!(None, scanner.try_pull());
		}
	}

	#[test]
	fn unambigious_but_incomplete() {
		// CHAI is an unambigious prefix, but it's not actually a keyword
		let mut scanner = TokenScan::new();
		for &b in b"CHAI" {
			assert!(scanner.try_pull().is_none());
			scanner.narrow(b);
		}
		scanner.flush();

		let kw = (&mut scanner).collect::<TokenScanBuffer>();
		assert_eq!(&b"CHAI"[..], &*kw);
	}

	#[test]
	fn final_char_failure() {
		let mut scanner = TokenScan::new();
		for &b in b"CHAIR" {
			assert!(scanner.try_pull().is_none());
			scanner.narrow(b);
		}
		let result = scanner.collect::<TokenScanBuffer>();
		assert_eq!(b"CHAIR".as_slice(), &*result);
	}

	#[test]
	fn early_failure() {
		let mut scanner = TokenScan::new();
		// none of these letters are token prefixes
		for &b in b"XYZZY" {
			scanner.narrow(b);
			assert_eq!(Some(b), scanner.try_pull());
		}
	}

	#[test]
	fn match_subset() {
		let mut scanner = TokenScan::new();
		for &b in b"END" {
			assert!(scanner.try_pull().is_none());
			scanner.narrow(b)
		}
		scanner.flush();

		let result = scanner.collect::<TokenScanBuffer>();
		assert_eq!(&[0xe0u8][..], &*result);
	}

	#[test]
	#[ignore = "this is just proof of a need of a rewrite"]
	fn must_be_word_start() {
		let mut scanner = TokenScan::new();
		for &b in b"T.TOLEMY" {
			scanner.narrow(b);
		}
		scanner.flush();

		let result = scanner.collect::<TokenScanBuffer>();
		assert_eq!(b"T.\xb8LEMY", &*result);
	}
}


/// A generative iterator for inferred line numbers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Range {
	pub start: u16,
	pub step: NonZeroU16,
}

impl Range {
	#[inline]
	fn new(start: u16, step: NonZeroU16) -> Self {
		Self { start, step }
	}
}

impl Iterator for Range {
	type Item = u16;

	fn next(&mut self) -> Option<Self::Item> {
		let prev = self.start;
		self.start = self.start.checked_add(self.step.get())?;
		Some(prev)
	}
}


static INFERENCE_ALIGNMENT_TRIES: [(u16, NonZeroU16); 4] = [
	(10, nonzero!(10u16)),
	(5, nonzero!(10u16)),
	(5, nonzero!(5u16)),
	(1, nonzero!(1u16)),
];

/// Infers line numbers for a block of unnumbered lines.
///
/// This algorithm can fail if there is no room to add a unique number to every line requested;
/// `basc` does not renumber a program.
fn infer_line_number_range(line_before: Option<u16>, line_after: Option<u16>, num_lines: u16)
-> Result<Range> {
	// we can flatten line_after immediately, but line_before has to wait
	//  (can't go below 0, which is a number we can't pre-reserve as a dummy line_before)
	let line_after = line_after.unwrap_or(0xff00);
	debug_assert!(line_before.unwrap_or(0) < line_after);

	if num_lines == 0 {
		return Ok(Range::new(10, nonzero!(10u16))); // always succeeds
	}

	// the one case where we need to use line 0
	if line_before.is_none() && line_after == num_lines {
		return Ok(Range::new(0, nonzero!(1u16)));
	}

	// try various heuristics, in very subjective order of niceness
	for (align, step) in INFERENCE_ALIGNMENT_TRIES {
		let line_before = line_before.unwrap_or_else(|| match step.get() {
			1 => 0,
			_ => 1,
		});

		if let Some(done) = (|| -> Option<Range> {
			let aligned_first = line_before.checked_add(align)?
			/ align * align;

			// `aligned_last` calc can overflow in extremely wrong cases
			let aligned_last = (num_lines - 1)
				.checked_mul(step.get())?
				.checked_add(aligned_first)?;

			if aligned_last < line_after {
				Some(Range::new(aligned_first, step))
			} else {
				None
			}
		})() {
			return Ok(done);
		}
	}

	Err(Error::TooManyUnnumberedLines {
		max_possible: line_after - line_before.map(|lb| lb + 1).unwrap_or(0),
		needed: num_lines.into(),
	})
}

#[cfg(test)]
mod test_line_number_inference {
	use super::*;

	fn range(begin: u16, step: u16) -> Result<Range> {
		Ok(Range::new(begin, NonZeroU16::new(step).unwrap()))
	}

	#[test]
	fn absolute() {
		fn case(exp_start: u16, exp_step: u16, line_before: u16, line_after: u16, num_lines: u16) {
			let expected = Range::new(exp_start, NonZeroU16::new(exp_step).unwrap());
			assert_eq!(Ok(expected), infer_line_number_range(
				Some(line_before), Some(line_after), num_lines));
		}
		case(10, 10, 5, 110, 10);
		case(5, 10, 4, 26, 3);
		case(6, 1, 5, 11, 5);
	}

	#[test]
	fn absolute_spacious() {
		assert_eq!(range(10, 10), infer_line_number_range(Some(5), Some(2000), 5));
	}

	#[test]
	fn unnumbered_start() {
		assert_eq!(range(10, 10), infer_line_number_range(None, Some(200), 19));
	}

	#[test]
	fn full_program() {
		for num_lines in [1, 100, 0x197f /* this * 10 + 10 == 0xff00 */, 0] {
			assert_eq!(range(10, 10), infer_line_number_range(None, None, num_lines));
		}
	}

	#[test]
	fn no_room() {
		assert_eq!(
			Err(Error::TooManyUnnumberedLines { max_possible: 10, needed: 11, }),
			infer_line_number_range(Some(100), Some(111), 11)
		);

		assert_eq!(
			Err(Error::TooManyUnnumberedLines { max_possible: 0, needed: 1 }),
			infer_line_number_range(Some(1), Some(2), 1)
		);

		assert_eq!(
			Err(Error::TooManyUnnumberedLines { max_possible: 100, needed: 101 }),
			infer_line_number_range(None, Some(100), 101)
		);

		assert_eq!(
			Err(Error::TooManyUnnumberedLines { max_possible: 255, needed: 256 }),
			infer_line_number_range(Some(0xfe00), None, 256)
		);
	}

	#[test]
	fn arith_overflow_checks() {
		// highest one with reasonable integers
		assert!(infer_line_number_range(Some(0xfefe), Some(0xfeff), 0xff00).is_err());
		// MORE
		assert!(infer_line_number_range(Some(u16::MAX - 1), Some(u16::MAX), u16::MAX).is_err());
	}
}
