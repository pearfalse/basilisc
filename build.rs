use std::{
	fs,
	io,
	io::Write,
	path::Path, num::NonZeroU8,
};

#[path = "meta-src"]
mod meta_src {
	mod keyword;
	pub(super) use keyword::Keyword;

	mod token_iter;
	pub(super) use token_iter::{TokenIter, Codegen};
}

use meta_src::*;

type RawTokenMap = &'static [(u8, &'static str)];

fn main() -> io::Result<()> {
	// OUT_DIR
	let out_dir = std::env::var_os("OUT_DIR").unwrap();
	let out_dir = Path::new(&out_dir);

	// early-runtime data gen
	let (token_goto, token_gosub) = {
		fn _find_direct(key: &'static str) -> u8 {
			TOKEN_MAP_DIRECT.iter().find(|&&(_, val)| val == key).unwrap().0
		}
		(_find_direct("GOTO"), _find_direct("GOSUB"))
	};

	// make the file
	let mut gen_token_data = fs::File::create(out_dir.join("token_data.rs"))?;

	write!(gen_token_data, r#"// auto-generated
use core::num::NonZeroU8;

use crate::support::{{RawKeyword, Keyword, TokenIter}};

type TokenDecodeMap = [RawKeyword; 256];

"#)?;

	// write arrays
	macro_rules! write_array {
		($var:ident) => {_write_array(&mut gen_token_data, $var, stringify!($var))};
	}
	write_array!(TOKEN_MAP_DIRECT)?;
	write_array!(TOKEN_MAP_C6)?;
	write_array!(TOKEN_MAP_C7)?;
	write_array!(TOKEN_MAP_C8)?;

	writeln!(&mut gen_token_data,
		"pub(crate) static LINE_DEPENDENT_KEYWORD_BYTES: [u8; 2] = [0x{:02x}, 0x{:02x}];\n",
		token_goto, token_gosub)?;


	// TODO write parsing map
	write_parse_map(&mut gen_token_data)?;


	gen_token_data.sync_all()?;
	return Ok(());

	fn _write_array(file: &mut fs::File, arr: RawTokenMap, name: &'static str) -> io::Result<()> {
		let mut flat_arr: [Option<&'static str>; 256] = [None; 256];
		for &(byte, val) in arr {
			flat_arr[byte as usize] = Some(val);
		}

		write!(file, "pub(crate) static {}: TokenDecodeMap = [\n", name)?;
		for maybe_val in flat_arr {
			debug_assert!(maybe_val.map(str::is_ascii).unwrap_or(true));
			let (raw_arr, comment_text) = match maybe_val {
				Some(s) => (Keyword::new(s).as_array(), s),
				None => (Default::default(), "<none>"),
			};
			writeln!(file, "\t{:?}, // {}", raw_arr, comment_text)?;
		}
		writeln!(file, "];\n")
	}
}

fn write_parse_map(file: &mut fs::File) -> io::Result<()> {
	let mut list = Vec::with_capacity(TOKEN_MAP_DIRECT.len()
		+ TOKEN_MAP_C6.len() + TOKEN_MAP_C7.len() + TOKEN_MAP_C8.len());

	let baked_prefix = [
		(TOKEN_MAP_DIRECT, None),
		(TOKEN_MAP_C6, NonZeroU8::new(0xc6)),
		(TOKEN_MAP_C7, NonZeroU8::new(0xc7)),
		(TOKEN_MAP_C8, NonZeroU8::new(0xc8)),
	];

	for (array, prefix) in baked_prefix {
		for (value, string) in array.iter().copied() {
			let value = NonZeroU8::new(value).unwrap();
			list.push((Keyword::new(string), if let Some(prefix) = prefix {
				TokenIter::new_indirect(prefix, value)
			} else {
				TokenIter::new_direct(value)
			}));
		}
	}

	writeln!(file, "pub(crate) static LOOKUP_MAP: [(Keyword<RawKeyword>, TokenIter); {}] = [", list.len())?;
	for (keyword, value) in list {
		writeln!(file, "\t( // {}", keyword.as_ascii_str().as_str())?;
		writeln!(file, "\t\tunsafe {{ /* SAFETY: values checked at build time */ \
Keyword::new_unchecked({:?}) }},", keyword.as_array())?;
		writeln!(file, "\t\t{},", Codegen::from(value))?;
		writeln!(file, "\t),")?;
	}
	writeln!(file, "];")?;

	Ok(())
}

static TOKEN_MAP_DIRECT: RawTokenMap = &[
	(0x7f, "OTHERWISE"),
	(0x80, "AND"),
	(0x81, "DIV"),
	(0x82, "EOR"),
	(0x83, "MOD"),
	(0x84, "OR"),
	(0x85, "ERROR"),
	(0x86, "LINE"),
	(0x87, "OFF"),
	(0x88, "STEP"),
	(0x89, "SPC"),
	(0x8a, "TAB("),
	(0x8b, "ELSE"),
	(0x8c, "THEN"),
	// 0x8d is a fix, we hardcode that special case
	(0x8e, "OPENIN"),
	(0x8f, "PTR"),
	(0x90, "PAGE"),
	(0x91, "TIME"),
	(0x92, "LOMEM"),
	(0x93, "HIMEM"),
	(0x94, "ABS"),
	(0x95, "ACS"),
	(0x96, "ADVAL"),
	(0x97, "ASC"),
	(0x98, "ASN"),
	(0x99, "ATN"),
	(0x9a, "BGET"),
	(0x9b, "COS"),
	(0x9c, "COUNT"),
	(0x9d, "DEG"),
	(0x9e, "ERL"),
	(0x9f, "ERR"),
	(0xa0, "EVAL"),
	(0xa1, "EXP"),
	(0xa2, "EXT"),
	(0xa3, "FALSE"),
	(0xa4, "FN"),
	(0xa5, "GET"),
	(0xa6, "INKEY"),
	(0xa7, "INSTR("),
	(0xa8, "INT"),
	(0xa9, "LEN"),
	(0xaa, "LN"),
	(0xab, "LOG"),
	(0xac, "NOT"),
	(0xad, "OPENUP"),
	(0xae, "OPENOUT"),
	(0xaf, "PI"),
	(0xb0, "POINT("),
	(0xb1, "POS"),
	(0xb2, "RAD"),
	(0xb3, "RND"),
	(0xb4, "SGN"),
	(0xb5, "SIN"),
	(0xb6, "SQR"),
	(0xb7, "TAN"),
	(0xb8, "TO"),
	(0xb9, "TRUE"),
	(0xba, "USR"),
	(0xbb, "VAL"),
	(0xbc, "VPOS"),
	(0xbd, "CHR$"),
	(0xbe, "GET$"),
	(0xbf, "INKEY$"),
	(0xc0, "LEFT$("),
	(0xc1, "MID$("),
	(0xc2, "RIGHT$("),
	(0xc3, "STR$"),
	(0xc4, "STRING$("),
	(0xc5, "EOF"),
	// c6 c7 c8 are fixes for two-byte tokens
	(0xc9, "WHEN"),
	(0xca, "OF"),
	(0xcb, "ENDCASE"),
	(0xcc, "ELSE"),
	(0xcd, "ENDIF"),
	(0xce, "ENDWHILE"),
	(0xcf, "PTR"),
	(0xd0, "PAGE"),
	(0xd1, "TIME"),
	(0xd2, "LOMEM"),
	(0xd3, "HIMEM"),
	(0xd4, "SOUND"),
	(0xd5, "BPUT"),
	(0xd6, "CALL"),
	(0xd7, "CHAIN"),
	(0xd8, "CLEAR"),
	(0xd9, "CLOSE"),
	(0xda, "CLG"),
	(0xdb, "CLS"),
	(0xdc, "DATA"),
	(0xdd, "DEF"),
	(0xde, "DIM"),
	(0xdf, "DRAW"),
	(0xe0, "END"),
	(0xe1, "ENDPROC"),
	(0xe2, "ENVELOPE"),
	(0xe3, "FOR"),
	(0xe4, "GOSUB"),
	(0xe5, "GOTO"),
	(0xe6, "GCOL"),
	(0xe7, "IF"),
	(0xe8, "INPUT"),
	(0xe9, "LET"),
	(0xea, "LOCAL"),
	(0xeb, "MODE"),
	(0xec, "MOVE"),
	(0xed, "NEXT"),
	(0xee, "ON"),
	(0xef, "VDU"),
	(0xf0, "PLOT"),
	(0xf1, "PRINT"),
	(0xf2, "PROC"),
	(0xf3, "READ"),
	(0xf4, "REM"),
	(0xf5, "REPEAT"),
	(0xf6, "REPORT"),
	(0xf7, "RESTORE"),
	(0xf8, "RETURN"),
	(0xf9, "RUN"),
	(0xfa, "STOP"),
	(0xfb, "COLOUR"),
	(0xfc, "TRACE"),
	(0xfd, "UNTIL"),
	(0xfe, "WIDTH"),
	(0xff, "OSCLI"),
];

static TOKEN_MAP_C6: RawTokenMap = &[
	(0x8e, "SUM"),
	(0x8f, "BEAT"),
];

static TOKEN_MAP_C7: RawTokenMap = &[
	(0x8f, "AUTO"),
	(0x8e, "APPEND"),
	(0x9d, "TWIN"),
	(0x9c, "TEXTSAVE"),
	(0x9e, "TWINO"),
	(0x99, "RENUMBER"),
	(0x98, "OLD"),
	(0x9b, "TEXTLOAD"),
	(0x9a, "SAVE"),
	(0x95, "LOAD"),
	(0x94, "LIST"),
	(0x97, "NEW"),
	(0x96, "LVAR"),
	(0x91, "DELETE"),
	(0x90, "CRUNCH"),
	(0x93, "HELP"),
	(0x92, "EDIT"),
	(0x93, "HELP"),
	(0x8e, "APPEND"),
	(0x8f, "AUTO"),
	(0x92, "EDIT"),
	(0x9f, "INSTALL"),
];

static TOKEN_MAP_C8: RawTokenMap = &[
	(0x8f, "CIRCLE"),
	(0x8e, "CASE"),
	(0x9d, "ELLIPSE"),
	(0x9c, "TINT"),
	(0x9f, "TEMPO"),
	(0x9e, "BEATS"),
	(0x99, "SYS"),
	(0x98, "QUIT"),
	(0x9b, "LIBRARY"),
	(0x95, "WHILE"),
	(0x94, "SWAP"),
	(0x97, "MOUSE"),
	(0x96, "WAIT"),
	(0x91, "ORIGIN"),
	(0x90, "FILL"),
	(0x93, "RECTANGLE"),
	(0x92, "POINT"),
	(0xa1, "VOICE"),
	(0xa0, "VOICES"),
	(0xa3, "OVERLAY"),
	(0xa2, "STEREO"),
	(0x98, "QUIT"),
	(0x9e, "BEATS"),
];
