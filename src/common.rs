//! Data types common to both packing and unpacking.

/// State machine for tracking whether the parse is in a string literal.
///
/// In unpacking, this controls escaping of literal " chars (U+0022), as well as how high-bit-set
/// bytes are interpreted. In packing, this is used to prevent tokenising happening within string
/// literals (e.g. the string "LEAVE ME ALONE" contains bytes for the greedy keyword `ON`, but we
/// shouldn't be treating it as such.)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum StringState {
	/// Not currently in a string literal
	#[default]
	NotInString,

	/// Currently in a string literal
	InString,

	/// Hovering over a closing quote mark (a second one indicates an escaped U+0022)
	MaybeClosed,
}
