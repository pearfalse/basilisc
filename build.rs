use std::{
	fs,
	io,
	io::Write,
	path::Path,
};

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

	gen_token_data.write_all(br#"// auto-generated

type TokenDecodeMap = [Option<&'static ::ascii::AsciiStr>; 256];
type TokenEncodeMap = phf::Map<&'static str, u8>;

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
			match maybe_val {
				Some(s) => writeln!(file, "\tSome(unsafe {{
\t\t//SAFETY: this is transmuted from a fixed, all-ASCII byte string
\t\t// (and we can't use an AsciiStr ctor in a const context)
\t\t::core::mem::transmute::<&'static str, &'static AsciiStr>(\"{}\")
\t}}),", s),
				None => writeln!(file, "\tNone,"),
			}?;
		}
		writeln!(file, "];\n")
	}
}

static TOKEN_MAP_DIRECT: RawTokenMap = &[
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
	(0x02, "BEAT"),
	(0x03, "SUM"),
];

static TOKEN_MAP_C7: RawTokenMap = &[
	(0x02, "AUTO"),
	(0x03, "APPEND"),
	(0x10, "TWIN"),
	(0x11, "TEXTSAVE"),
	(0x13, "TWINO"),
	(0x14, "RENUMBER"),
	(0x15, "OLD"),
	(0x16, "TEXTLOAD"),
	(0x17, "SAVE"),
	(0x18, "LOAD"),
	(0x19, "LIST"),
	(0x1a, "NEW"),
	(0x1b, "LVAR"),
	(0x1c, "DELETE"),
	(0x1d, "CRUNCH"),
	(0x1e, "HELP"),
	(0x1f, "EDIT"),
	(0x84, "HELP"),
	(0x87, "APPEND"),
	(0x95, "AUTO"),
	(0xa5, "EDIT"),
];

static TOKEN_MAP_C8: RawTokenMap = &[
	(0x02, "CIRCLE"),
	(0x03, "CASE"),
	(0x10, "ELLIPSE"),
	(0x11, "TINT"),
	(0x12, "TEMPO"),
	(0x13, "BEATS"),
	(0x14, "SYS"),
	(0x15, "QUIT"),
	(0x16, "LIBRARY"),
	(0x17, "INSTALL"),
	(0x18, "WHILE"),
	(0x19, "SWAP"),
	(0x1a, "MOUSE"),
	(0x1b, "WAIT"),
	(0x1c, "ORIGIN"),
	(0x1d, "FILL"),
	(0x1e, "RECTANGLE"),
	(0x1f, "POINT"),
	(0x2c, "VOICE"),
	(0x2d, "VOICES"),
	(0x2e, "OVERLAY"),
	(0x2f, "STEREO"),
	(0x87, "INSTALL"),
	(0xc4, "QUIT"),
	(0xc7, "BEATS"),
];
