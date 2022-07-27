use ascii::AsciiStr;

use basc_macros::declare_token_data;


type ExpandBuf = arrayvec::ArrayVec<u8, 3>;

const INDIRECT_PREFIX: u8 = 0x8d;
const INDIRECT_C6: u8 = 0xc6;
const INDIRECT_C7: u8 = 0xc7;
const INDIRECT_C8: u8 = 0xc8;

fn query_token(src: &ExpandBuf) -> Option<&'static AsciiStr> {
	match src.get(0) {
		Some(&INDIRECT_PREFIX) => {
			let table2: &'static [Option<&'static AsciiStr>; 256] = match *src.get(1)? {
				INDIRECT_C6 => &TOKEN_MAP_8D_C6,
				INDIRECT_C7 => &TOKEN_MAP_8D_C7,
				INDIRECT_C8 => &TOKEN_MAP_8D_C8,
				_ => return None,
			};
			table2[*src.get(2)? as usize]
		},
		Some(&b) => TOKEN_MAP_DIRECT[b as usize],
		None => None,
	}
}

#[cfg(test)]
mod test_unpack {
	use super::*;

	#[test]
	fn query_token_matches() {
		let data = [
			([0x80, 0x0d, 0x0d], "AND"),
			([0x8a, 0x0d, 0x0d], "TAB("),

			([0x8d, 0xc6, 0x02], "BEAT"),

			([0x8d, 0xc7, 0x16], "TEXTLOAD"),
			([0x8d, 0xc7, 0x17], "SAVE"),
		];

		for (bytes, word) in data.into_iter() {
			let mut av = ExpandBuf::default();
			for b in bytes.into_iter().take_while(|b| *b != 0x0d) {
				av.push(b);
			}

			assert_eq!(Some(word), query_token(&av).map(AsciiStr::as_str));
		}
	}

	#[test]
	fn query_token_no_match() {
		let data = [
			[0x20, 0x0d, 0x0d], // ASCII char
			[0xc6, 0x02, 0x0d], // no prefix
			[0x8d, 0xc6, 0xff], // nothing in this slot
			[0x8d, 0x21, 0x0d], // sequence did not complete
			[0x8d, 0x8d, 0xc6], // not correct as-is
		];
		for bytes in data.into_iter() {
			let mut av = ExpandBuf::default();
			for b in bytes.into_iter().take_while(|b| *b != 0x0d) {
				av.push(b);
			}

			assert_eq!(None, query_token(&av));
		}
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
	}
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
}
