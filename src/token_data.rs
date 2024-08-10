//! All compiled data `basilisc` knows about BASIC keywords.

/// To generate the contents of this file, run `cargo run -p token-data-gen`.
#[path = "../meta-src/token_data.rs"]
mod gen_data;
pub(crate) use gen_data::{TOKEN_MAP_DIRECT, TOKEN_MAP_C6, TOKEN_MAP_C7, TOKEN_MAP_C8, LOOKUP_MAP};

#[cfg(test)]
mod test_lookup {
	// These tests are for lookups on generated data

	use basilisc_base::subarray::SubArray;

	use basilisc_base::keyword::RawKeyword;
	use super::{TOKEN_MAP_DIRECT, TOKEN_MAP_C6, TOKEN_MAP_C7, TOKEN_MAP_C8};

	#[test]
	fn test_direct() {
		let data = [
			(0x80u8, "AND"),
			(0x8cu8, "THEN"),
			(0xc5u8, "EOF"),
		];
		for (byte, word) in data.into_iter() {
			let kw: Option<&'static RawKeyword> = TOKEN_MAP_DIRECT.get_flat(byte as usize);
			assert_eq!(
				Some(word.as_bytes()),
				kw.map(|k| k.as_ascii_str().as_bytes())
			);
		}
		assert_eq!(None, TOKEN_MAP_DIRECT.get_flat(0x8d));
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
			let kw = arr.get_flat(byte as usize).unwrap();
			assert_eq!(word, kw.as_ascii_str().as_str());
		}
	}

	#[test]
	fn proof_we_disallowed_empty_strings() {
		fn all_str_lengths(table: SubArray<'static, Option<RawKeyword>>)
			-> impl Iterator<Item = usize> {
				table.as_raw_slice().iter()
					.flat_map(Option::as_ref)
					.map(|k| k.as_ascii_str().len())
			}

		assert!(all_str_lengths(TOKEN_MAP_DIRECT)
			.chain(all_str_lengths(TOKEN_MAP_C6))
			.chain(all_str_lengths(TOKEN_MAP_C7))
			.chain(all_str_lengths(TOKEN_MAP_C8))
			.all(|sl| sl > 0));
	}

	#[test]
	fn token_iter() {
		use arrayvec::ArrayVec;

		const MAX_LEN: usize = basilisc_base::keyword::MAX_LEN as usize;
		assert_eq!(b"PRINT", &*crate::token_data::TOKEN_MAP_DIRECT[0xf1]
			.unwrap().iter()
			.collect::<ArrayVec<u8, MAX_LEN>>());
	}
}
