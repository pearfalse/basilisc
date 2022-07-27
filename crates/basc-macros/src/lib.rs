extern crate proc_macro;
use proc_macro2::*;

#[proc_macro]
pub fn declare_token_data(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	tokenise2(input.into()).unwrap_or_else(syn::Error::into_compile_error)
		.into()
}

fn tokenise2(input: TokenStream) -> syn::Result<TokenStream> {
	Err(syn::Error::new(Span::call_site(), "not implemented"))
}


fn build_basic_tokens<I: core::iter::ExactSizeIterator<Item = &'static str>>(
	tokens: I) -> TokenStream {
	todo!()
}