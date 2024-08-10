//! Handle line number encoding in GOTO/GOSUB references.
//!
//! BBC BASIC uses a different line number encoding when following a GOTO or GOSUB statement;
//! this is done for performance and interpreter simplicity[^1].
//!
//! The line number format consists of a 0x8d byte, followed by three bytes that encode the line
//! reference. The extra redundancy in this encoding is used to keep all three bytes with the range
//! of printable, non-numeric ASCII characters.
//!
//! Given three source bytes `A` `B` `C`, and a final line number `F`, the decoding process looks
//! like this:
//!
//! - initialise `F` to `0x4040`;
//! - xor F[15 → 14] with A[3 → 2];
//! - xor F[13 → 8] with C[5 → 0];
//! - xor F[7 → 6] with A[5 → 4];
//! - xor F[5 → 0] with B[5 → 0].
//!
//! The encoding process is the direct inverse of this, although for full correctness, any final
//! bytes between `0x00` and `0x3e` should be OR'd with `0x40`. `0x0d` must be avoided.
//!
//! [^1]: <https://web.archive.org/web/20081203145001/www.nelsonit.net/~jon/BBCMicro/2005/06/27/165311.html>,
//! via <https://xania.org/200711/bbc-basic-line-number-format>

/// The limit of line numbers; all valid line numbers in a `u16` must be strictly less than this.
pub const LIMIT: u16 = 0xff00;

/// The marker byte indicating a coded line reference.
pub const MARKER_BYTE: u8 = 0x8d;

use thiserror::Error;

/// A line number reference has three bytes of meaningful info, after the `0x8D` header (not
/// included here).
pub type Encoded = [u8; 3];

#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
pub enum DecodeError {
	#[error("out of range (decoded value was &{0:x}, but must be less than &ff00")]
	OutOfRange(u32),
}

#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
pub enum EncodeError {
	#[error("out of range (line number must be less than &ff00)")]
	OutOfRange,
}

/// Encodes a line number into its equivalent line reference format.
///
/// Returns `Err(EncodeError::OutOfRange)` if `line_number` is not below [`LIMIT`].
pub fn try_encode(line_number: u16) -> Result<Encoded, EncodeError> {
	if line_number >= LIMIT { return Err(EncodeError::OutOfRange); }

	let mut r = [0b0101_0100u8, 0b0100_0000, 0b0100_0000];
	let [ref mut a, ref mut b, ref mut c] = r;

	*b ^= ( line_number &             0b11_1111) as u8;
	*a ^= ((line_number &           0b1100_0000) >> 2) as u8;
	*c ^= ((line_number &   0b11_1111_0000_0000) >> 8) as u8;
	*a ^= ((line_number & 0b1100_0000_0000_0000) >> 12) as u8;
	Ok(r)
}


/// Decodes a byte array into its equivalent numeric format.
pub fn try_decode(encoded: Encoded) -> Result<u16, DecodeError> {
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
	if tgt < 0xff00 {
		Ok(tgt as u16)
	} else {
		Err(DecodeError::OutOfRange(tgt))
	}
}

#[cfg(test)]
mod test {
	use super::*;

	macro_rules! assert_case {
		($encoded:literal, $number:literal) => {
			::assert_hex::assert_eq_hex!(Ok(*$encoded), try_encode($number));
			::assert_hex::assert_eq_hex!(Ok($number), try_decode(*$encoded));
		};
	}

	#[test]
	fn simple_cases() {
		assert_case!(b"T@@", 0);
		assert_case!(b"TJ@", 10);
		assert_case!(b"D@@", 64);
		assert_case!(b"h\x7f~", 0xfeff);
		assert_case!(b"dH@", 200);
		assert_case!(b"DE@", 69);
		assert_case!(b"tdA", 420);
		assert_case!(b"t[B", 667);
		assert_case!(b"dHG", 1992);
		assert_case!(b"Typ", 12345);
		assert_case!(b"DcC", 867);
		assert_case!(b"t}T", 5309);
	}

	#[test]
	fn encode_out_of_range() {
		for i in LIMIT..(u16::MAX) {
			assert_eq!(Err(EncodeError::OutOfRange), try_encode(i));
		}
	}

	#[test]
	fn y_u_no_54() {
		// I created this test to try and work out why the encode fn needs to represent the
		// first byte starting value of 0x54, and the decode fn doesn't. I still have no idea.
		for ln in 0..LIMIT {
			assert_eq!(Some(ln), try_encode(ln).ok().and_then(|e| try_decode(e).ok()));
		}
	}

	#[test]
	fn decode_out_of_range() {
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