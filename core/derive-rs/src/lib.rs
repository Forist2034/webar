extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

mod codec;

#[proc_macro_derive(ToCbor, attributes(codec))]
pub fn derive_to_cbor(input: TokenStream) -> TokenStream {
    codec::derive_to_cbor(
        &quote!(webar_core),
        &quote!(webar_core::codec::cbor),
        parse_macro_input!(input),
    )
    .into()
}

#[proc_macro_derive(FromCbor, attributes(codec))]
pub fn derive_from_cbor(input: TokenStream) -> TokenStream {
    codec::derive_from_cbor(
        &quote!(webar_core),
        &quote!(webar_core::codec::cbor),
        parse_macro_input!(input),
    )
    .into()
}

#[proc_macro_derive(CborCodec, attributes(codec))]
pub fn derive_cbor_codec(input: TokenStream) -> TokenStream {
    codec::derive_cbor_codec(
        &quote!(webar_core),
        &quote!(webar_core::codec::cbor),
        parse_macro_input!(input),
    )
    .into()
}
