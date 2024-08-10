//! Handles packing a textual BBC BASIC representation into a tokenised BASIC file (type `&FFB`).

use std::{fmt, num::NonZeroU16, io};

use arrayvec::ArrayVec;
use nonzero_ext::nonzero;

use crate::support::IoObject;

mod gaps;
use gaps::*;

mod token_scan;
use token_scan::TokenScanner;

mod byte_decode;
use byte_decode::ByteDecoder;

/// The maximum length a BASIC line can be (`u8::MAX` minus 4 bytes of header).
const MAX_LINE_LEN: usize = 251;

/// The maximum number of lines allowable in a BASIC file.
const LINE_NUMBER_CAP: u16 = 0xff00;


/// All errors that can occur when encoding.
#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
	/// User tried to add too many lines to the program.
	#[error("too many lines in program (limit is 65280)")]
	TooManyLines,

	/// Program cannot be fully numbered, due to existing pre-numbered lines.
	#[error("too many lines to fully number the program (had room for {max_possible}, but needed {needed}")]
	TooManyUnnumberedLines { max_possible: u16, needed: u32 },

	/// Given line was too long after tokenisation.
	#[error("line too long (must be under 252 chars, but was {length})")]
	LineTooLong { length: u16 },

	/// Requested line number was too large.
	#[error("line number too large ({found}, must be less than 65280)")]
	LineNumberOutOfRange { found: u32 },

	/// Source file contained invalid UTF-8.
	#[error("invalid UTF-8 character at file position {start_pos}")]
	InvalidUtf8 { start_pos: u64 },

	/// Source file contained a UTF-8 character that has no RISC OS Latin-1 equivalent.
	#[error("character '{}' has no RISC OS Latin-1 equivalent", (.0).escape_unicode())]
	NoLatin1Char(char),

	/// Source file contained a GOTO/GOSUB with a line number out of range.
	#[error("invalid line number after {}", .0)]
	InvalidLineReference(&'static ascii::AsciiStr),

	/// I/O error.
	#[error("I/O error: {0}")]
	IoError(#[from] std::io::Error),
}

/// Struct for errors that can occur when encoding a text file.
#[derive(Debug, PartialEq)]
pub struct Error {
	line_number: u32,
	pub kind: ErrorKind,
}

impl Error {
	/// Returns the line number that this error occurred on, if known.
	///
	/// Keep in mind that this is the 1-indexed line of the _text_ file, _not_ the BASIC line number.
	pub fn line_number(&self) -> Option<u32> {
		Some(self.line_number).filter(|&n| n != u32::MAX)
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Display::fmt(&self.kind, f)?;
		if self.line_number != u32::MAX {
			write!(f, " at line {}", self.line_number)?;
		}
		Ok(())
	}
}

impl std::error::Error for Error { }

impl From<io::Error> for Error {
	fn from(ioe: io::Error) -> Self {
		Self { line_number: u32::MAX, kind: ErrorKind::IoError(ioe) }
	}
}

impl From<std::convert::Infallible> for ErrorKind {
	fn from(src: std::convert::Infallible) -> Self {
		match src { }
	}
}

impl From<token_scan::Error> for ErrorKind {
	fn from(inner: token_scan::Error) -> Self {
		match inner {
			token_scan::Error::InvalidLineRef { after }
				=> Self::InvalidLineReference(token_scan::Error::lookup(after)),
		}
	}
}

impl PartialEq for ErrorKind {
	fn eq(&self, other: &Self) -> bool {
		use ErrorKind::*;
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
				&InvalidUtf8 { start_pos: a },
				&InvalidUtf8 { start_pos: b },
			) => a == b,
			(
				&NoLatin1Char(a), &NoLatin1Char(b),
			) => a == b,
			(
				&InvalidLineReference(a), &InvalidLineReference(b)
			) => *a == *b,
			_ => false,
		}
	}
}

/// A line in the final program.
#[derive(Debug)]
pub struct Line {
	line_number: u16,
	contents: Box<[u8]>,
}

impl Line {
	/// Writes the serialised form of this line to the given I/O object.
	pub fn write(&self, target: &mut dyn io::Write) -> io::Result<()> {
		debug_assert!(self.contents.len() <= MAX_LINE_LEN);
		let hdr_len = unsafe {
			// SAFETY: only internal code can create `contents`, and we know it's short enough
			(self.contents.len() as u8).checked_add(4).unwrap_unchecked() // header length
		};
		let line_number = self.line_number.to_be_bytes();
		let header = [0x0d, line_number[0], line_number[1], hdr_len];
		target.write_all(&header)?;
		target.write_all(&self.contents)
	}
}

/// A line in the final program that might not yet have a line number.
#[derive(Debug)]
struct UnnumberedLine {
	line_number: Option<u16>,
	contents: Box<[u8]>,
}

/// Common result type for this module.
type Result<T> = ::std::result::Result<T, Error>;
type KindResult<T> = ::std::result::Result<T, ErrorKind>;

/// The core structure for encoding a BASIC file.
#[derive(Debug)]
pub struct Parser<'a> {
	buf: Box<ArrayVec<u8, MAX_LINE_LEN>>,
	lines: Vec<UnnumberedLine>,
	token_scan: TokenScanner,
	inner: ByteDecoder<'a>,
	is_eof: bool,
}

impl<'a> Parser<'a> {
	/// Constructs a new parser.
	pub fn new(src: IoObject<'a>) -> Self {
		Self {
			buf: Box::new(ArrayVec::new()),
			lines: Vec::new(),
			token_scan: TokenScanner::new(),
			inner: ByteDecoder::new(src),
			is_eof: false,
		}
	}

	/// Attempts to read and conver the next line of the file.
	///
	/// Upon success, returns whether the parser expects there to be more lines to convert.
	pub fn next_line(&mut self) -> Result<bool> {
		// we need early return for fuse behaviour
		if self.is_eof { return Ok(false); }

		if self.lines.len() == LINE_NUMBER_CAP as usize {
			// cannot add more lines, no way no how
			return Err(Error {
				line_number: LINE_NUMBER_CAP as u32,
				kind: ErrorKind::TooManyLines,
			});
		}

		if let Some(maybe_ln) = self.raw_line()? {
			self.lines.push(UnnumberedLine {
				line_number: maybe_ln,
				contents: (&**self.buf).into(),
			});
		} else {
			self.is_eof = true;
		}
		Ok(!self.is_eof)
	}

	fn raw_line(&mut self) -> Result<Option<Option<u16>>> {
		// TODO: encode line number references properly
		#[derive(Debug)]
		enum LineParser {
			BeforeLineNumber,
			ParsingLineNumber { stage: u16 },
			InLineBody { line_number: Option<u16> },
		}

		let this_line_number = self.lines.len().saturating_add(1).min(u32::MAX as usize) as u32;
		let wrap_error = move |kind| Error {
			line_number: this_line_number,
			kind,
		};

		let mut state = LineParser::BeforeLineNumber;
		self.buf.clear();
		self.is_eof = true; // assume EOF, prove wrong when breaking on `\n`
		while let Some(byte) = self.inner.read_next().map_err(wrap_error)? {
			match byte {
				b'\r' => continue, // blunt way of handling CRLF
				b'\n' => {
					self.is_eof = false;
					self.flush_token_scanner().map_err(wrap_error)?; // reset token scanning, sort of
					break
				}, // end of line
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
						self.update_body(other).map_err(wrap_error)?;
					}
				},
				LineParser::ParsingLineNumber { ref mut stage } => match byte {
					b'0'..=b'9' => {
						let new = ((*stage) as u32) * 10 + (byte - b'0') as u32;
						*stage = u16::try_from(new).ok()
							.filter(|&ln| ln < LINE_NUMBER_CAP)
							.ok_or(ErrorKind::LineNumberOutOfRange { found: new })
							.map_err(wrap_error)?;
					},
					other => {
						state = LineParser::InLineBody { line_number: Some(*stage) };
						self.update_body(other).map_err(wrap_error)?;
					},
				},
				LineParser::InLineBody { line_number: _ } => {
					self.update_body(byte).map_err(wrap_error)?
				},
			};
		}

		self.flush_token_scanner().map_err(wrap_error)?;

		let final_line_number = match state {
			LineParser::BeforeLineNumber => return Ok(None),
			LineParser::ParsingLineNumber { stage } => Some(stage),
			LineParser::InLineBody { line_number } => line_number,
		};

		Ok(Some(final_line_number))
	}

	fn flush_token_scanner(&mut self) -> KindResult<()> {
		self.token_scan.flush()?;
		let mut flushed_bytes = token_scan::CharBuffer::new();
		while let Some(b) = self.token_scan.try_pull() {
			flushed_bytes.push(b);
		}
		self.try_add_bytes(&flushed_bytes)?;

		Ok(())
	}

	/// Writes the converted BASIC file to the given I/O object.
	pub fn write(self, target: &mut dyn io::Write) -> Result<()> {
		let lines = self.into_lines()?;

		let ln = std::cell::Cell::new(0u32);
		let wrap_error = |ioe| Error {
			line_number: ln.get() as u32,
			kind: ErrorKind::IoError(ioe)
		};

		for (i, line) in lines.into_iter().enumerate() {
			ln.set(i as u32);
			line.write(target).map_err(wrap_error)?;
		}

		ln.set(u32::MAX);
		target.write_all(&[0x0d, 0xff]).map_err(wrap_error)?;
		Ok(())
	}

	fn into_lines(mut self) -> Result<Vec<Line>> {
		for gap in FindGaps::within(&mut self.lines).collect::<Vec<Gap>>() {
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

	fn update_body(&mut self, byte: u8) -> KindResult<()> {
		self.token_scan.push(byte)?;

		// narrowing can release a token too
		if let Some(b) = self.token_scan.try_pull() {
			self.try_add_bytes(&[b])?;
		}

		Ok(())
	}

	fn try_add_bytes(&mut self, bytes: &[u8]) -> KindResult<()> {
		let error = {
			let add_len: u16 = bytes.len().try_into().expect("can't add over 65K bytes!");
			let old_buf_len = self.buf.len() as u16;

			move || ErrorKind::LineTooLong {
				length: match old_buf_len.saturating_add(add_len) {
					fits if fits < u16::MAX => fits, // ???
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
				Err(Error { line_number: 1, kind: ErrorKind::LineNumberOutOfRange { found: reach } }),
				Parser::new(&mut io::Cursor::new(buf.as_str().as_bytes())).next_line(),
			);
		}
	}

	#[test]
	fn no_tokens() {
		for set_line_numbers in [true, false] {
			expect_success(b"100 hooray", &[(100, b" hooray")], set_line_numbers);
		}
	}

	#[test]
	fn eof_vs_eol() {
		expect_success(b"10\n!\n20", &[
			(10, b""),
			(15, b"!"),
			(20, b"")
		], true);
	}

	#[test]
	fn tokens_no_line_numbers() {
		expect_success(b"PRINT\"it works\"\nEND", &[
			(10, b"\xf1\"it works\""),
			(20, b"\xe0"),
		], true);
	}

	#[test]
	fn problematic_prefixes() {
		expect_success(b"10EN\n20END\n30ENDPR\n40ENDPROC\n50ENDPROCK\n60ENDPI", &[
			(10, b"EN"),
			(20, b"\xe0"),
			(30, b"ENDPR"),
			(40, b"\xe1"),
			(50, b"ENDPROCK"),
			(60, b"ENDPI"),
		], false);
	}

	#[test]
	fn gap_before_line_numbers() {
		static ALL: &'static [u8] = b"\x20\x20\x20 30END";
		for i in 0..3 {
			expect_success(&ALL[i..], &[(30, b"\xe0")], false);
		}
	}

	#[test]
	fn else_hack_flat() {
		use super::token_scan::ElseHack;

		expect_success(b"ELSE\nTHEN\nELSE\nENDIF\nELSE", &[
			(10, &[ElseHack::ELSE]), // normal ELSE to start with
			(20, &[ElseHack::THEN]), // trailing THEN activates else hack
			(30, &[ElseHack::ALT_ELSE]), // use alt multiline ELSE
			(40, &[ElseHack::ENDIF]), // ENDIF ends things
			(50, &[ElseHack::ELSE]), // normal ELSE again
		], true);
	}

	#[test]
	fn else_hack_nested() {
		use super::token_scan::ElseHack;

		expect_success(b"ELSE\nTHEN\nELSE\nTHEN\nELSE\nENDIF\nELSE\nENDIF\nELSE", &[
			(10, &[ElseHack::ELSE]), // normal ELSE to start with
			(20, &[ElseHack::THEN]), // trailing THEN activates else hack
			(30, &[ElseHack::ALT_ELSE]), // use alt multiline ELSE
			(40, &[ElseHack::THEN]), // trailing THEN activates else hack
			(50, &[ElseHack::ALT_ELSE]), // use alt multiline ELSE
			(60, &[ElseHack::ENDIF]), // ENDIF pops 1 layer
			(70, &[ElseHack::ALT_ELSE]), // still technically in an ENDIF
			(80, &[ElseHack::ENDIF]), // ENDIF resets
			(90, &[ElseHack::ELSE]), // normal ELSE again
		], true);
	}

	#[test]
	fn known_regressions() {
		expect_success(b"IFaTHENb\nIFcTHENdELSEe",
			&[
				(10, b"\xe7a\x8cb"),
				(20, b"\xe7c\x8cd\x8be"),
			], true);
		expect_success(b".\nc$=\"\":f%=TRUE", &[(10, b"."), (20, b"c$=\"\":f%=\xb9")], true);
		expect_success(b"CASEr%OF\nWHEN", &[
				(10, b"\xc8\x8er%\xca"),
				(20, b"\xc9"),
			], true);
	}

	#[test]
	fn goto_gosub() {
		expect_success(b"10GOTO20",
			&[(10, b"\xe5\x8dTT@")], false);
		expect_success(b"10GOTO20:PRINT\"never print this\"",
			&[(10, b"\xe5\x8dTT@:\xf1\"never print this\"")], false);
	}

	fn expect_success(input: &[u8], output: &[(u16, &[u8])], set_numbers: bool) {
		let mut cursor = io::Cursor::new(input);
		let mut parser = Parser::new(&mut cursor);

		let expected_nl_count = input.iter().filter(|b| **b == b'\n').count();

		for _ln in 0..expected_nl_count {
			println!("line {}: {:?}", _ln, &parser.token_scan);
			assert_eq!(Ok(true), parser.next_line());
		}
		println!("line {}: {:?}", expected_nl_count, &parser.token_scan);
		assert_eq!(Ok(false), parser.next_line());

		assert_eq!(parser.lines.len(), output.len());

		macro_rules! check {
			($search:expr, $proc_line_number:expr) => {
				for (line, (exp_line_number, exp_line_contents))
				in $search.into_iter().zip(output.iter().copied()) {
					assert_eq!($proc_line_number(exp_line_number), line.line_number);
					::assert_hex::assert_eq_hex!(exp_line_contents, &*line.contents);
				}
			};
		}

		if set_numbers {
			check!(parser.into_lines().unwrap(), core::convert::identity);
		} else {
			check!(parser.lines, ::std::option::Option::Some);
		}
	}
}

#[cfg(test)]
mod test_line_write {
	use super::*;

	#[test]
	fn basic() {
		write(27, b"ABCDE", b"\r\0\x1b\x09ABCDE");
		write(0xfeff, b"\x00\xff", b"\r\xfe\xff\x06\x00\xff");
	}

	fn write(line_number: u16, contents: &[u8], expect: &[u8]) {
		let mut target = Vec::new();
		Line {
			line_number,
			contents: Vec::from(contents).into_boxed_slice(),
		}.write(&mut target).unwrap();
		assert_eq!(expect, &*target);
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
/// Line numbers are selected based on subjective heuristics; it aims to use lines on multiples of
/// 10, then tries smaller subdivisions of that, falling back to increments of 1 if nothing else
/// works.
/// This algorithm can fail if there is no room to add a unique number to every line requested;
/// `basilisc` does not renumber a program.
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

	Err(Error {
		line_number: u32::MAX,
		kind: ErrorKind::TooManyUnnumberedLines {
			max_possible: line_after - line_before.map(|lb| lb + 1).unwrap_or(0),
			needed: num_lines.into(),
		}
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
		fn assert_one(
			line_before: Option<u16>, line_after: Option<u16>, num_lines: u16,
			max_possible: u16, needed: u32
		) {
			assert_eq!(
				Err(Error {
					line_number: u32::MAX,
					kind: ErrorKind::TooManyUnnumberedLines { max_possible, needed }
				}),
				infer_line_number_range(line_before, line_after, num_lines)
			);
		}

		assert_one(Some(100), Some(111), 11, 10, 11);
		assert_one(Some(1), Some(2), 1, 0, 1);
		assert_one(None, Some(100), 101, 100, 101);
		assert_one(Some(0xfe00), None, 256, 255, 256);
	}

	#[test]
	fn arith_overflow_checks() {
		// highest one with reasonable integers
		assert!(infer_line_number_range(Some(0xfefe), Some(0xfeff), 0xff00).is_err());
		// MORE
		assert!(infer_line_number_range(Some(u16::MAX - 1), Some(u16::MAX), u16::MAX).is_err());
	}
}
