//! Handle line number encoding in RISC OS tokenised BASIC files.
//!
//! RISC OS' BASIC module adds a new file format, different from the one used on the BBC Micro. This new format has a different encoding for line numbers, which seems to have been done for two reasons:
//!
//! - Allow the use of larger line numbers. BASIC on the BBC Micro caps line numbers at 32767 (0x7fff), whereas RISC OS BASIC files can go up to 65279 (0xfeff).
//! - Ensure that RISC OS BASIC files are always unreadable by tools that only understand the BBC Micro format, presumably to protect against encoded tokens being destroyed.
//!
//! The new line number format consists of three bytes that encode an 18-bit number. The format appears to be more futureproofed than the BASIC interpreter will actually allow; it is unknown what bits 16 and 17 could represent. The extra redundancy in this encoding is used in Acorn-generated BASIC files (from BASIC's `SAVE`, and !Edit's BASIC file support) to keep all three bytes in the numeric range of printable ASCII characters.
//!
//! This encoded line number form appears at the start of each line, as well as to encode the targets of `GOTO` and `GOSUB` statements.
//!
//! Given three source bytes `A` `B` `C`, and a final line number `F`, the decoding process looks like this:
//!
//! • initialise `F` to `0x4040`;
//! • xor F[15->14] with A[3->2];
//! • xor F[13->8] with C[5->0];
//! • xor F[7->6] with A[5->4];
//! • xor F[5->0] with B[5->0].
//!
//! The encoding process is the direct inverse of this, although for full correctness, any final bytes between `0x00` and `0x3e` should be OR'd with `0x40`.
//!

use thiserror::Error;

pub type Encoded = [u8; 3];

#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
pub enum DecodeError {
	#[error("unexpected end of file (required 3 bytes to decode)")]
	Eof,
	#[error("out of range (decoded value was &{0:x}, but must be < &ff00")]
	OutOfRange(u32),
}

pub fn try_decode_from<I: Iterator<Item = u8>>(mut encoded: I) -> Result<u32, DecodeError> {
	let mut try_next = move || match encoded.next() {
		Some(i) => Ok(i),
		None => Err(DecodeError::Eof)
	};

	let a = try_next()?;
	let b = try_next()?;
	let c = try_next()?;

	try_decode([a, b, c])
}

pub fn try_decode(encoded: Encoded) -> Result<u32, DecodeError> {

	let a = encoded[0] as u32;
	let b = encoded[1] as u32;
	let c = encoded[2] as u32;

	let tgt = 0x4040u32

	// bit copy operations are described in notes (as [A, B, C] -> F)
	// syntax is not Rust; `a..b` Rust equivalent is `b..=a`

	// F[17..16] ^= A[1..0]
	^ ((a & 0b0011) << 16)

	// F[15..14] ^= A[3..2]
	^ ((a & 0b1100) << 12)

	// F[13..8] ^= C[5..0]
	^ ((c & 0b11_1111) << 8)

	// F[7..6] ^= A[5..4]
	^ ((a & 0b11_0000) << 2)

	// F[5..0] ^= B[5..0]
	^ (b & 0b11_1111)

	;
	if tgt < 0xff00 { Ok(tgt) } else { Err(DecodeError::OutOfRange(tgt)) }
}

#[cfg(test)]
mod test_decode {
	use super::*;

	#[test]
	fn simple_cases() {
		let data_cases = [
			([0x14, 0x00, 0x00], 0),
			([0x14, 0x0a, 0x00], 10),
			([0x04, 0x00, 0x00], 64),
			([0x28, 0x3f, 0x3e], 0xfeff),

			// same as above, but with some 0x40s OR'd in to remove ASCII control codes
			([b'T', b'@', b'@'], 0),
			([b'T', b'J', b'@'], 10),
			([b'D', b'@', b'@'], 64),
			([b'(', b'?', b'>'], 0xfeff),
		];
		for (data, expected) in data_cases {
			assert_eq!(Ok(expected), try_decode(data));
		}
	}

	#[test]
	fn err_too_short() {
		let data_cases = [
			&[][..],
			&[0x40],
			&[0x40, 0x50],
		];
		for data in data_cases {
			assert_eq!(Err(DecodeError::Eof), try_decode_from(data.iter().copied()));
		}
	}

	#[test]
	fn err_out_of_range() {
		let data_cases = [
			// number is greater than 16-bit
			([1, 0, 0], 0x1_4040),
			([2, 0, 0], 0x2_4040),
			([3, 0, 0], 0x3_4040),

			// number is 0xff00, smallest disallowed number 1011 1111 0100 0000
			([0x18, 0x00, 0x3f], 0xff00),
		];

		for (data, expected) in data_cases {
			match try_decode(data) {
				Err(DecodeError::OutOfRange(e)) if e == expected => expected,
				Err(DecodeError::OutOfRange(other)) =>
					panic!("unexpected OOR number (expected &{0:05x}, got &{1:05x}",
						expected, other),
				r#else => panic!("unexpected other value {:?}", r#else),
			};
		}
	}
}