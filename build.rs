use std::{
	fs,
	io,
	io::Write,
	path::Path, num::NonZeroU8,
};


#[path = "meta-src"]
mod meta_src {
	pub mod keyword;
	pub mod token_iter;

	pub(super) mod cooked_keyword;
}
use meta_src::{keyword, cooked_keyword::Keyword, token_iter::{self, TokenIter}};

#[allow(dead_code)]
fn dead_code_build_rs_exemptions() {
	let _ = TokenIter::peek_first;
}

fn main() -> io::Result<()> {
	// OUT_DIR
	let out_dir = std::env::var_os("OUT_DIR").unwrap();
	let out_dir = Path::new(&out_dir);

	// early-runtime data gen
	let (token_goto, token_gosub) = {
		fn _find_direct(key: &'static str) -> u8 {
			TOKEN_MAP_DIRECT.iter().find(|kw| kw.keyword().as_str() == key).unwrap().byte().get()
		}
		(_find_direct("GOTO"), _find_direct("GOSUB"))
	};

	// make the file
	let mut gen_token_data = fs::File::create(out_dir.join("token_data.rs"))?;

	write!(gen_token_data, r#"// auto-generated
use core::num::NonZeroU8;

use crate::{{
	keyword::RawKeyword,
	subarray::SubArray,
	token_iter::TokenIter,
}};

type TokenDecodeMap = SubArray<'static, Option<RawKeyword>>;

"#)?; // TODO: there's no reason to have these be RawKeywords, there's no fallible conversion here

	// write arrays
	macro_rules! write_array {
		($var:ident) => {_write_array(&mut gen_token_data, &*$var, stringify!($var))};
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

	fn _write_array(file: &mut fs::File, arr: &'static [Keyword], name: &'static str)
	-> io::Result<()> {
		fn reduce_front<T>(mut slice: &[Option<T>]) -> (&[Option<T>], usize) {
			let mut from = 0usize;
			while let Some((None, x)) = slice.split_first() {
				slice = x;
				from += 1;
			}
			(slice, from)
		}
		fn reduce_back<T>(mut slice: &[Option<T>]) -> &[Option<T>] {
			while let Some((None, x)) = slice.split_last() {
				slice = x;
			}
			slice
		}

		// for efficient storage, this needs to be sorted by token byte
		let mut flat_arr: [Option<&'static Keyword>; 256] = [None; 256];
		for kw in arr {
			flat_arr[kw.byte().get() as usize] = Some(kw);
		}

		let (flat_arr, from) = reduce_front(&mut flat_arr[..]);
		let flat_arr = reduce_back(flat_arr);

		writeln!(file, "pub(crate) static {}: TokenDecodeMap = TokenDecodeMap::new(unsafe {{&[",
			name)?;
		writeln!(file, "\t// SAFETY: valid arrays were generated at build time")?;
		for (i, maybe_val) in flat_arr.iter().copied().enumerate() {
			let i = i + from;
			if let Some(kw) = maybe_val {
				writeln!(file, "\tSome(RawKeyword::new_unchecked({:?})), // {:02X} = {}",
					kw.as_array(), i, kw.keyword().as_str())?;
			} else {
				writeln!(file, "\tNone, // {:02X} = <none>", i)?;
			}
		}
		writeln!(file, "]}}, 0x{:02x});\n", from)
	}
}

fn write_parse_map(file: &mut fs::File) -> io::Result<()> {
	let mut list: Vec<(&'static Keyword, TokenIter)>
	= Vec::with_capacity(TOKEN_MAP_DIRECT.len()
		+ TOKEN_MAP_C6.len() + TOKEN_MAP_C7.len() + TOKEN_MAP_C8.len());

	let baked_prefix: [(&'static [Keyword], _); 4] = [
		(&*TOKEN_MAP_DIRECT, None),
		(&*TOKEN_MAP_C6, NonZeroU8::new(0xc6)),
		(&*TOKEN_MAP_C7, NonZeroU8::new(0xc7)),
		(&*TOKEN_MAP_C8, NonZeroU8::new(0xc8)),
	];

	for (map, prefix) in baked_prefix {
		for kw in map {
			list.push((kw, if let Some(prefix) = prefix {
				TokenIter::new_indirect(prefix, kw.byte())
			} else {
				TokenIter::new_direct(kw.byte())
			}));
		}
	}

	list.sort_unstable();

	writeln!(file, "pub(crate) type TokenLookupEntry = (RawKeyword, TokenIter);")?;
	writeln!(file, "// SAFETY: values are generated from the same parsed type at build time")?;
	writeln!(file,
		"pub(crate) static LOOKUP_MAP: [TokenLookupEntry; {}] = unsafe {{[",
		list.len(),
	)?;

	for (keyword, value) in list {
		writeln!(file, "\t( // {}", keyword.keyword().as_str())?;
		writeln!(file, "\t\tRawKeyword::new_unchecked({:?}),", keyword.as_array())?;
		writeln!(file, "\t\t{},", token_iter::Codegen::from(value, false))?;
		writeln!(file, "\t),")?;
	}
	writeln!(file, "]}};")?;

	Ok(())
}

macro_rules! _token_greedy {
	(nongreedy) => { $crate::cooked_keyword::Match::Nongreedy };
	() => { $crate::cooked_keyword::Match::Greedy };
}

macro_rules! _token_impl {
	($byte:literal, $word:literal, $abbr:expr, $pos:ident, $greedy:expr) => {
		$crate::meta_src::cooked_keyword::Keyword::try_new($byte, $word,
			$abbr,
			$crate::meta_src::keyword::TokenPosition::$pos,
			$greedy)
		.unwrap()
	}
}

macro_rules! _token_once {
	(($byte:expr, $word:literal, nongreedy))
	=> {_token_impl!(
		$byte,
		$word,
		None,
		Any,
		false
	)};
	(($byte:expr, $word:literal, abbr $abbr:literal, nongreedy))
	=> {_token_impl!(
		$byte,
		$word,
		Some(::nonzero_ext::nonzero!($abbr as u8)),
		Any,
		false
	)};

	(($byte:expr, $word:literal))
	=> {_token_impl!(
		$byte,
		$word,
		None,
		Any,
		true
	)};
	(($byte:expr, $word:literal, abbr $abbr:literal))
	=> {_token_impl!(
		$byte,
		$word,
		Some(::nonzero_ext::nonzero!($abbr as u8)),
		Any,
		true
	)};
	(($byte:expr, $word:literal, abbr $abbr:literal, pos $pos:ident, nongreedy))
	=> {_token_impl!(
		$byte,
		$word,
		Some(::nonzero_ext::nonzero!($abbr as u8)),
		$pos,
		true
	)};
}

macro_rules! token_map {
	($name:ident, $( $groups:tt ,)*) => {
		::lazy_static::lazy_static! {
			static ref $name: &'static [Keyword] = vec![$(
				_token_once!($groups),
			)*].leak();
		}
	}
}

token_map![TOKEN_MAP_DIRECT,
	(0x7f, "OTHERWISE"),
	(0x80, "AND", abbr 1),
	(0x81, "DIV"),
	(0x82, "EOR"),
	(0x83, "MOD"),
	(0x84, "OR"),
	(0x85, "ERROR", abbr 3),
	(0x86, "LINE", abbr 3),
	(0x87, "OFF"),
	(0x88, "STEP", abbr 1),
	(0x89, "SPC"),
	(0x8a, "TAB("),
	(0x8b, "ELSE"),
	(0x8c, "THEN", abbr 2),
	// 0x8d is a fix, we hardcode that special case
	(0x8e, "OPENIN", abbr 2),
	(0x8f, "PTR", abbr 2, nongreedy),
	(0x90, "PAGE", abbr 2, pos Right, nongreedy),
	(0x91, "TIME", abbr 2, pos Right, nongreedy),
	(0x92, "LOMEM", abbr 3, pos Right, nongreedy),
	(0x93, "HIMEM", abbr 1, pos Right, nongreedy),
	(0x94, "ABS"),
	(0x95, "ACS"),
	(0x96, "ADVAL", abbr 2),
	(0x97, "ASC"),
	(0x98, "ASN"),
	(0x99, "ATN"),
	(0x9a, "BGET", abbr 1, nongreedy),
	(0x9b, "COS"),
	(0x9c, "COUNT", abbr 3, nongreedy),
	(0x9d, "DEG"),
	(0x9e, "ERL", nongreedy),
	(0x9f, "ERR", nongreedy),
	(0xa0, "EVAL", abbr 2),
	(0xa1, "EXP"),
	(0xa2, "EXT", nongreedy),
	(0xa3, "FALSE", abbr 2, nongreedy),
	(0xa4, "FN"),
	(0xa5, "GET"),
	(0xa6, "INKEY"),
	(0xa7, "INSTR(", abbr 3),
	(0xa8, "INT"),
	(0xa9, "LEN"),
	(0xaa, "LN"),
	(0xab, "LOG"),
	(0xac, "NOT"),
	(0xad, "OPENUP"),
	(0xae, "OPENOUT", abbr 5),
	(0xaf, "PI", nongreedy),
	(0xb0, "POINT(", abbr 2),
	(0xb1, "POS", nongreedy),
	(0xb2, "RAD"),
	(0xb3, "RND", nongreedy),
	(0xb4, "SGN"),
	(0xb5, "SIN"),
	(0xb6, "SQR"),
	(0xb7, "TAN", abbr 1),
	(0xb8, "TO"),
	(0xb9, "TRUE", nongreedy),
	(0xba, "USR"),
	(0xbb, "VAL"),
	(0xbc, "VPOS", abbr 2, nongreedy),
	(0xbd, "CHR$", abbr 3),
	(0xbe, "GET$", abbr 2),
	(0xbf, "INKEY$"),
	(0xc0, "LEFT$(", abbr 2),
	(0xc1, "MID$(", abbr 1),
	(0xc2, "RIGHT$(", abbr 2),
	(0xc3, "STR$", abbr 3),
	(0xc4, "STRING$(", abbr 4),
	(0xc5, "EOF", nongreedy),
	// c6 c7 c8 are fixes for two-byte tokens
	// TODO: they map to AUTO, DELETE and LOAD on Beeb
	(0xc9, "WHEN"),
	(0xca, "OF"),
	(0xcb, "ENDCASE", nongreedy),
	// ELSE, but in multiline IF statements; handled separately
	(0xcd, "ENDIF", nongreedy),
	(0xce, "ENDWHILE", nongreedy),
	(0xcf, "PTR", abbr 2, nongreedy),
	(0xd0, "PAGE", abbr 2, pos Left, nongreedy),
	(0xd1, "TIME", abbr 2, pos Left, nongreedy),
	(0xd2, "LOMEM", abbr 3, pos Left, nongreedy),
	(0xd3, "HIMEM", abbr 1, pos Left, nongreedy),
	(0xd4, "SOUND", abbr 2),
	(0xd5, "BPUT", abbr 2, nongreedy),
	(0xd6, "CALL", abbr 2),
	(0xd7, "CHAIN", abbr 2),
	(0xd8, "CLEAR", abbr 2, nongreedy),
	(0xd9, "CLOSE", abbr 3, nongreedy),
	(0xda, "CLG", nongreedy),
	(0xdb, "CLS", nongreedy),
	(0xdc, "DATA", abbr 1),
	(0xdd, "DEF"),
	(0xde, "DIM"),
	(0xdf, "DRAW", abbr 2),
	(0xe0, "END", nongreedy),
	(0xe1, "ENDPROC", abbr 1, nongreedy),
	(0xe2, "ENVELOPE", abbr 2),
	(0xe3, "FOR", abbr 1),
	(0xe4, "GOSUB", abbr 3),
	(0xe5, "GOTO", abbr 1),
	(0xe6, "GCOL", abbr 2),
	(0xe7, "IF"),
	(0xe8, "INPUT", abbr 1),
	(0xe9, "LET"),
	(0xea, "LOCAL", abbr 3),
	(0xeb, "MODE", abbr 2),
	(0xec, "MOVE", abbr 3),
	(0xed, "NEXT", abbr 1),
	(0xee, "ON"),
	(0xef, "VDU", abbr 1),
	(0xf0, "PLOT", abbr 2),
	(0xf1, "PRINT", abbr 1),
	(0xf2, "PROC", abbr 3),
	(0xf3, "READ", abbr 3),
	(0xf4, "REM"),
	(0xf5, "REPEAT", abbr 3),
	(0xf6, "REPORT", abbr 4, nongreedy),
	(0xf7, "RESTORE", abbr 3),
	(0xf8, "RETURN", abbr 1, nongreedy),
	(0xf9, "RUN", nongreedy),
	(0xfa, "STOP", abbr 3, nongreedy),
	(0xfb, "COLOUR", abbr 1),
	(0xfc, "TRACE", abbr 2),
	(0xfd, "UNTIL", abbr 1),
	(0xfe, "WIDTH", abbr 1),
	(0xff, "OSCLI", abbr 2),
];

token_map![TOKEN_MAP_C6,
	(0x8e, "SUM"),
	(0x8f, "BEAT"),
];

token_map![TOKEN_MAP_C7,
	(0x8e, "APPEND"),
	(0x8f, "AUTO", abbr 2),
	(0x90, "CRUNCH"),
	(0x91, "DELETE", abbr 3),
	(0x92, "EDIT"),
	(0x93, "HELP", nongreedy),
	(0x94, "LIST", abbr 1),
	(0x95, "LOAD", abbr 2),
	(0x96, "LVAR", nongreedy),
	(0x97, "NEW", nongreedy),
	(0x98, "OLD", abbr 1, nongreedy),
	(0x99, "RENUMBER", abbr 3),
	(0x9a, "SAVE", abbr 2),
	(0x9b, "TEXTLOAD"),
	(0x9c, "TEXTSAVE"),
	(0x9d, "TWIN", nongreedy),
	(0x9e, "TWINO"),
	(0x9f, "INSTALL"),
];

token_map![TOKEN_MAP_C8,
	(0x8e, "CASE"),
	(0x8f, "CIRCLE"),
	(0x90, "FILL"),
	(0x91, "ORIGIN"),
	(0x92, "POINT"),
	(0x93, "RECTANGLE"),
	(0x94, "SWAP"),
	(0x95, "WHILE"),
	(0x96, "WAIT", nongreedy),
	(0x97, "MOUSE"),
	(0x98, "QUIT", nongreedy),
	(0x99, "SYS"),
	(0x9b, "LIBRARY"),
	(0x9c, "TINT"),
	(0x9d, "ELLIPSE"),
	(0x9e, "BEATS"),
	(0x9f, "TEMPO"),
	(0xa0, "VOICES"),
	(0xa1, "VOICE"),
	(0xa2, "STEREO"),
	(0xa3, "OVERLAY"),
];
