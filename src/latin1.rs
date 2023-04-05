//! Maps from RISC OS' Latin-1 code page to and from matching printable Unicode characters. All
//! mappings are reversible in a round-trip conversion.
//!
//! # Encoding variations
//!
//! C0 control codes, while normally preserved in Unicode, are replaced with printable equivalents
//! in the _Control Pictures_ block. The same is done for the non-breaking space (sometimes called
//! _hard space_) at 0xA0, where it is replaced with SYMBOL FOR SPACE (U+2420).
//!
//! The character for byte 0x87 has no single-character Unicode equivalent and will be replaced with
//! SUPERSCRIPT SEVEN (U+2077).

/// A map from RISC OS Latin-1 to Unicode. Each index of this array corresponds to the Latin-1 byte,
/// and the character at that position to its Unicode equivalent.
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
	'€', 'Ŵ', 'ŵ', '\u{25f0}', '\u{1fbc0}', 'Ŷ', 'ŷ', '⁷' /* 8^7 */,
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

mod private {
	#[doc(hidden)] pub trait Sealed { }
}

/// Extensions to `char` for convenient conversion between RISC OS Latin-1 and approximate Unicode
/// equivalents.
pub(crate) trait CharExt : private::Sealed {
	/// Creates a character from a byte interpreted as RISC OS Latin-1.
	fn from_risc_os_latin1(src: u8) -> Self;

	/// Tries to encode the char as a RISC OS Latin-1 byte.
	#[allow(clippy::wrong_self_convention)] // this trait only applies to `char`, which is `Copy`
	fn as_risc_os_latin1(self) -> Option<u8>;
}

impl private::Sealed for char { }

impl CharExt for char {
	#[inline]
	fn from_risc_os_latin1(src: u8) -> char {
		LATIN1_MAP[src as usize]
	}

	fn as_risc_os_latin1(self) -> Option<u8> {
		let this = &(self as u32);
		// ASCII printable, C0 direct, raw DEL
		if (0x00..=0x7f).contains(this) {
			return Some(self as u8);
		}
		// C0 soft forms
		if (0x2400..=0x241f).contains(this) {
			return Some((self as u32 - 0x2400) as u8);
		}
		// DEL soft form
		if *this == 0x2421 {
			return Some(0x7f);
		}
		LATIN1_MAP[0x80..].iter().position(|&l| l == self).map(|idx| idx as u8 + 0x80)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn to_unicode() {
		assert_eq!('C', char::from_risc_os_latin1(b'C'));
		assert_eq!('€', char::from_risc_os_latin1(0x80));
		assert_eq!('\u{2400}', char::from_risc_os_latin1(0x00));
	}

	#[test]
	fn to_latin1() {
		assert_eq!(Some(0x00), '\u{2400}'.as_risc_os_latin1());
		assert_eq!(Some(0x8d), '™'.as_risc_os_latin1());
		assert_eq!(Some(0x7f), '\u{2421}'.as_risc_os_latin1());
		assert_eq!(Some(0xff), 'ÿ'.as_risc_os_latin1());
		assert_eq!(None, 'ア'.as_risc_os_latin1());
	}

	#[test]
	fn roundtrip_consistency() {
		// `as_risc_os_latin1` has bespoke logic that can theoretically get out of sync with the
		// basic lookup

		for i in 0u8..=255 {
			let ch = char::from_risc_os_latin1(i);
			assert_eq!(Some(i), ch.as_risc_os_latin1());
		}
	}
}
