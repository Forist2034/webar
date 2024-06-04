use proc_macro::TokenStream;

mod serialize;

#[proc_macro_derive(Serialize)]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    serialize::derive_serialize(syn::parse_macro_input!(input)).into()
}
