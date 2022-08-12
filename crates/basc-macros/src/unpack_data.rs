use super::*;

use proc_macro2::{TokenStream, TokenTree, Delimiter, Literal, Group};
use quote::quote;

use ascii::AsciiStr;

pub(super) fn generate(table: &AllMaps) -> TokenStream {
	// this is fairly straightforward tbh

	let mut ts = TokenStream::new();
	let mut line_keywords = Vec::with_capacity(2);
	output(&mut ts, &mut line_keywords, &table.tokens_direct, "DIRECT");
	output(&mut ts, &mut line_keywords, &table.tokens_8d_c6, "8D_C6");
	output(&mut ts, &mut line_keywords, &table.tokens_8d_c7, "8D_C7");
	output(&mut ts, &mut line_keywords, &table.tokens_8d_c8, "8D_C8");


	let mut ts_ldk = TokenStream::new();
	for &ldbyte in &line_keywords {
		ts_ldk.extend(quote!(#ldbyte,));
	}
	let ts_ldk_len = line_keywords.len();

	let ts_ldk = TokenTree::Group(Group::new(Delimiter::Bracket, ts_ldk));

	ts.extend(quote! {
		static LINE_DEPENDENT_KEYWORD_BYTES: [u8; #ts_ldk_len] = #ts_ldk;
	});

	return ts;

	fn output<'a>(ts: &mut TokenStream, line_keywords: &mut Vec<u8>,
		table: &TokenMap, name_suffix: &'static str
	) {
		let mut inside_array = TokenStream::new();
		let none = quote!(::core::option::Option::None,);

		let mut as_array: [Option<&AsciiStr>; 256] = [None; 256];
		for (&byte, string) in table {
			let s = string.as_ref();
			if s == "GOTO" || s == "GOSUB" {
				// these keywords rely on line numbers
				assert!(&name_suffix[..2] != "8D", "GOTO/GOSUB cannot be indirect");
				line_keywords.push(byte);
			}
			as_array[byte as usize] = Some(s);
		}
		let as_array = as_array;

		for slot in as_array.into_iter() {
			inside_array.extend(slot.as_ref().map(|s| {
				// convert to byte literal
				let s_bliteral = TokenTree::Literal(
					Literal::byte_string(s.as_bytes())
				);
				quote! {
					::core::option::Option::Some(unsafe {
						// SAFETY: this is a byte string literal, generated from an AsciiStr inside
						// a proc macro
						// we transmute since `from_ascii_unchecked` is a trait method, which
						// cannot be const
						::core::mem::transmute::<&'static [u8], &'static ::ascii::AsciiStr>
						(&*#s_bliteral)
					}),
				}
			}).unwrap_or_else(|| none.clone()));
		}

		let array_name = Ident::new(format!("TOKEN_MAP_{0}", name_suffix).as_str(),
			Span::call_site());
		let array_ts = Group::new(Delimiter::Bracket, inside_array);

		ts.extend(quote! {
			static #array_name : [::core::option::Option<&'static ::ascii::AsciiStr>; 256]
				= #array_ts;
		});
	}
}