//! Maps from RISC OS' Latin-1 code page to and from matching printable Unicode characters.

/// A map from RISC OS Latin-1 to Unicode. Each index of this array corresponds to the Latin-1 byte, and the character at that position to its Unicode equivalent.
///
/// C0 control codes, while preserved in Unicode, are replaced with printable equivalents in the _Control Pictures_ block.
static LATIN1_MAP: [char; 256] = [
	// C0 control codes
	'\u{2400}', '\u{2401}', '\u{2402}', '\u{2403}', '\u{2404}', '\u{2405}', '\u{2406}', '\u{2407}',
	'\u{2408}', '\u{2409}', '\u{240A}', '\u{240B}', '\u{240C}', '\u{240D}', '\u{240E}', '\u{240F}',
	'\u{2410}', '\u{2411}', '\u{2412}', '\u{2413}', '\u{2414}', '\u{2415}', '\u{2416}', '\u{2417}',
	'\u{2418}', '\u{2419}', '\u{241A}', '\u{241B}', '\u{241C}', '\u{241D}', '\u{241E}', '\u{241F}',

	// Basic ASCII is straightforward
	' ', '!', '"', '#', '$', '%', '&', '\'','(', ')', '*', '+', ',', '-', '.', '/',
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
	'@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
	'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\',']', '^', '_',
	'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
	'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~',
	// but we encode \x7f
	'\u{2421}',

	// C1 gets … interesting
	'€', 'Ŵ', 'ŵ', '\u{25f0}', '\u{1fbc0}', 'Ŷ', 'ŷ', '\u{fffd}' /* 8^7 */,
	'⇦', '⇨', '⇩', '⇧', '…', '™', '‰', '•',
	'‘', '’', '‹', '›', '“', '”', '„', '\u{2013}', '\u{2014}', '\u{2212}',
	'Œ', 'œ', '†', '‡', 'ﬁ', 'ﬂ',

	// A0
	'\u{2420}', '¡', '¢', '£', '¤', '¥', '¦', '§',
	'\u{0308}', '©', 'ª', '«', '¬', '\u{ad}', '®', '\u{af}',
	'°', '±', '²', '³', '´', 'µ', '¶', '·', '¸', '¹', 'º', '»', '¼', '½', '¾', '¿',
	'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
	'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
	'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï',
	'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ',
];

pub(crate) mod ext {
	use sealed::sealed;

	#[sealed]
	pub(crate) trait CharExt {
		fn as_risc_os_latin1(self) -> Option<u8>;
	}

	#[sealed]
	impl CharExt for char {
		fn as_risc_os_latin1(self) -> Option<u8> {
			super::LATIN1_MAP.iter().position(|&l| l == self).map(|idx| idx as u8)
		}
	}


	#[sealed]
	pub(crate) trait ByteExt {
		fn to_risc_os_latin1(self) -> char;
	}

	#[sealed]
	impl ByteExt for u8 {
		fn to_risc_os_latin1(self) -> char {
			super::LATIN1_MAP[self as usize]
		}
	}
}

#[cfg(test)]
mod tests {
	use super::ext::*;

	#[test]
	fn to_unicode() {
		assert_eq!('€', 0x80.to_risc_os_latin1());
		assert_eq!('\u{2400}', 0x00.to_risc_os_latin1());
	}

	#[test]
	fn to_latin1() {
		assert_eq!(Some(0x00), '\u{2400}'.as_risc_os_latin1());
		assert_eq!(Some(0x8d), '™'.as_risc_os_latin1());
		assert_eq!(None, 'ア'.as_risc_os_latin1());
	}
}
