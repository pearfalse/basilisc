use std::collections::BTreeMap;

extern crate proc_macro;
use proc_macro2::Span;
use syn::{
	Ident, LitInt, LitStr,
	parse::{ParseBuffer, ParseStream, Result, Error},
};

mod unpack_data;

use ascii::AsciiString;

#[proc_macro]
pub fn declare_token_data(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let all = syn::parse_macro_input!(input as AllMaps);

	unpack_data::generate(&all).into()
}

type TokenMap = BTreeMap<u8, AsciiString>;

#[derive(Debug, Default)]
pub(crate) struct AllMaps {
	tokens_direct: TokenMap,
	tokens_8d_c6 : TokenMap,
	tokens_8d_c7 : TokenMap,
	tokens_8d_c8 : TokenMap,
}


impl syn::parse::Parse for AllMaps {
	fn parse(input: ParseStream) -> Result<Self> {
		let mut tokens_direct = TokenMap::default();
		let mut tokens_8d_c6: Option<TokenMap> = None;
		let mut tokens_8d_c7: Option<TokenMap> = None;
		let mut tokens_8d_c8: Option<TokenMap> = None;

		while ! input.is_empty() {
			if let Some((byte, expanded, expanded_span)) = is_direct_token(&input, true)? {
				insert(&mut tokens_direct, byte, expanded, expanded_span)?;
			}
			else if let Some((prefix, prefix_span, inner)) = is_prefix(&input)? {
				let tgt = match prefix {
					0xc6 => &mut tokens_8d_c6,
					0xc7 => &mut tokens_8d_c7,
					0xc8 => &mut tokens_8d_c8,
					_ => return Err(Error::new(prefix_span, "unrecognised prefix byte")),
				};
				if tgt.is_some() {
					return Err(Error::new(prefix_span, format!(
						"tokens for prefix {:02x} cannot be specified more than once", prefix)));
				}
				let tgt = tgt.insert(TokenMap::default());

				while ! inner.is_empty() {
					if let Some((byte, expanded, expanded_span))
					= is_direct_token(&&inner, false)? {
						insert(tgt, byte, expanded, expanded_span)?;
					}
					else {
						return Err(unexpected_token(&inner));
					}
				}

				// finally, trailing comma
				if ! input.is_empty() {
					let _ = input.parse::<syn::Token![,]>()?;
				}
			}
			else {
				return Err(unexpected_token(input));
			}
		}

		Ok(AllMaps {
			tokens_direct,
			tokens_8d_c6: tokens_8d_c6.unwrap_or_default(),
			tokens_8d_c7: tokens_8d_c7.unwrap_or_default(),
			tokens_8d_c8: tokens_8d_c8.unwrap_or_default(),
		})
	}
}

fn unexpected_token(stream: ParseStream) -> Error {
	stream.error("expected direct token or prefix here")
}

fn insert(map: &mut TokenMap, byte: u8, expanded: String, expanded_span: Span) -> Result<()> {
	use std::collections::btree_map::Entry;
	let expanded = AsciiString::from_ascii(expanded).map_err(|_|
		Error::new(expanded_span, "string is not ASCII"))?;
	match map.entry(byte) {
		Entry::Vacant(slot) => slot.insert(expanded),
		Entry::Occupied(_) => return Err(Error::new(expanded_span, ""))
	};

	Ok(())
}

fn is_direct_token(input: &ParseStream, enforce_high_bit: bool)
-> Result<Option<(u8, String, Span)>> {
	use syn::parse::discouraged::Speculative;
	let fork = input.fork();
	if let Ok(int) = fork.parse::<LitInt>() {
		input.advance_to(&fork);
		let (int, _) = parse_basic_token_defn(int, enforce_high_bit)?;
		let _ = input.parse::<syn::Token![=>]>()?;
		let expanded = input.parse::<LitStr>()?;
		if ! input.is_empty() {
			// parse trailing comma too
			let _ = input.parse::<syn::Token![,]>()?;
		}
		Ok(Some(( int, expanded.value(), expanded.span() )))
	} else {
		Ok(None)
	}
}

fn is_prefix<'a>(input: &'_ ParseStream<'a>) -> Result<Option<(u8, Span, ParseBuffer<'a>)>> {
	use syn::parse::discouraged::Speculative;
	let fork = input.fork();
	match fork.parse::<Ident>() {
		Ok(l) if l == "prefix" => {
			input.advance_to(&fork);
			let prefix_group;
			syn::parenthesized!(prefix_group in input);
			let prefix = prefix_group.parse::<LitInt>()?;
			let (int, int_span) = parse_basic_token_defn(prefix, true)?;
			let _ = input.parse::<syn::Token![=>]>()?;

			let inner;
			syn::braced!(inner in input);
			Ok(Some((int, int_span, inner)))
		},
		Ok(l) => Err(input.error(format!(
			"expected direct byte or literal 'prefix' here, got '{}' instead", l.to_string()
			))),
		Err(_) => Ok(None),
	}
}

fn parse_basic_token_defn(int: LitInt, enforce_high_bit: bool) -> Result<(u8, Span)> {
	let (int, int_span) = (int.base10_parse::<u8>()?, int.span());
	if enforce_high_bit && int < 0x80 {
		return Err(Error::new(int_span, "token byte must have the high bit set"));
	}
	Ok((int, int_span))
}
