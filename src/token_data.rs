include!(concat!(env!("OUT_DIR"), "/token_data.rs"));

#[cfg(test)]
mod tests{
	use super::{TOKEN_MAP_DIRECT, TOKEN_MAP_C6, TOKEN_MAP_C7, TOKEN_MAP_C8};

	#[test]
	fn test_direct() {
		let data = [
			(0x80u8, "AND"),
			(0x8cu8, "THEN"),
			(0xc5u8, "EOF"),
		];
		for (byte, word) in data.into_iter() {
			assert_eq!(Some(word.as_bytes()), TOKEN_MAP_DIRECT[byte as usize]);
		}
		assert_eq!(None, TOKEN_MAP_DIRECT[0x8d]);
	}

	#[test]
	fn test_indirect() {
		let data = [
			(&TOKEN_MAP_C6, 0x8eu8, "SUM"),
			(&TOKEN_MAP_C6, 0x8fu8, "BEAT"),
			(&TOKEN_MAP_C7, 0x8eu8, "APPEND"),
			(&TOKEN_MAP_C7, 0x91u8, "DELETE"),
			(&TOKEN_MAP_C8, 0x8eu8, "CASE"),
			(&TOKEN_MAP_C8, 0x99u8, "SYS"),
		];
		for (arr, byte, word) in data.into_iter() {
			assert_eq!(Some(word.as_bytes()), arr[byte as usize]);
		}
	}

	#[test]
	fn proof_we_disallowed_empty_strings() {
		fn all_str_lengths(table: &'static [Option<&'static [u8]>; 256])
			-> impl Iterator<Item = usize> {
				table.iter().filter_map(|elem| *elem).map(<[u8]>::len)
			}

		assert!(all_str_lengths(&TOKEN_MAP_DIRECT)
			.chain(all_str_lengths(&TOKEN_MAP_C6))
			.chain(all_str_lengths(&TOKEN_MAP_C7))
			.chain(all_str_lengths(&TOKEN_MAP_C8))
			.all(|sl| sl > 0));
	}

	#[test]
	fn check_flagged_goto_gosub() {
		use super::LINE_DEPENDENT_KEYWORD_BYTES;
		for keyword in ["GOTO", "GOSUB"] {
			let byte = TOKEN_MAP_DIRECT.iter()
				.map(Option::as_deref)
				.position(|k| k == Some(keyword.as_bytes()))
				.and_then(|u| u8::try_from(u).ok())
				.unwrap();
			assert!(LINE_DEPENDENT_KEYWORD_BYTES.iter().find(|&&l| l == byte).is_some(),
				"could not find {} byte equiv ({:02x}) in LINE_DEPENDENT_KEYWORD_BYTES ({:?})",
				keyword, byte, LINE_DEPENDENT_KEYWORD_BYTES);
		}
	}
}
