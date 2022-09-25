#![cfg_attr(build, allow(dead_code))]

use ascii::AsciiStr;

const MAX_LEN: usize = 10;
pub(crate) struct Keyword([u8; MAX_LEN]);

impl Keyword {
	pub(crate) fn new(src: &str) -> Self {
		let slice = Some(src).filter(|&src| src.bytes().all(|b| b >= 0x20))
			.expect("string contains ASCII control codes")
			.as_bytes();

		let _ = AsciiStr::from_ascii(slice)
			.expect("string is not ASCII");

		Self({
			let mut a = [0u8; MAX_LEN];
			a[..slice.len()].copy_from_slice(slice);
			a
		})
	}

	pub(crate) const fn new_unchecked(src: [u8; MAX_LEN]) -> Self {
		Self(src)
	}

	pub(crate) fn as_array(&self) -> [u8; MAX_LEN] { self.0 }

	pub(crate) fn as_ascii_str(&self) -> &AsciiStr {
		let mut slice: &[u8] = self.0.as_slice();

		while let Some((0, x)) = slice.split_last() {
			slice = x;
		}

		unsafe {
			// SAFETY: we don't allow construction of non-ASCII byte slices
			AsciiStr::from_ascii_unchecked(slice)
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn as_ascii_str() {
		let k = Keyword::new("ASCIIstr");
		assert_eq!(AsciiStr::from_ascii(b"ASCIIstr"), Ok(k.as_ascii_str()));
	}

	#[test]
	fn as_array() {
		let k = Keyword::new("ABCDEFGH");
		assert_eq!([65,66,67,68,69,70,71,72,0,0], k.as_array());
	}

	#[test]
	#[should_panic]
	fn fail_too_long() {
		let _ = Keyword::new("ASCIIstring");
	}

	#[test]
	#[should_panic]
	fn fail_not_ascii() {
		let _ = Keyword::new("1234\u{4e94}");
	}

	#[test]
	#[should_panic]
	#[allow(non_snake_case)]
	fn fail_C0() {
		let _ = Keyword::new("keywo\rd");
	}
}