use darling::{FromAttributes, FromMeta};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse_quote, punctuated::Punctuated, Attribute, DeriveInput, Expr, Ident, Path, PathArguments,
    PathSegment,
};

mod cbor {
    pub mod decode;
    pub mod encode;
}
mod generics;
mod rename;

#[derive(Clone, Copy, FromMeta)]
enum Rename {
    SnakeCase,
}

#[derive(FromAttributes)]
#[darling(attributes(codec))]
struct FieldOptions {
    #[darling(default)]
    rename: Option<String>,
}

#[derive(FromAttributes)]
#[darling(attributes(codec))]
struct StructOptions {
    #[darling(default)]
    transparent: bool,
}

#[derive(Default, FromAttributes)]
#[darling(attributes(codec))]
struct VariantOptions {
    #[darling(default)]
    rename: Option<String>,
}

#[derive(FromAttributes)]
#[darling(attributes(codec))]
struct EnumOptions {
    #[darling(default)]
    rename_variants: Option<Rename>,
}

fn sort_fields(fields: &syn::FieldsNamed) -> Vec<(String, &Ident)> {
    let mut ret = fields
        .named
        .iter()
        .map(|f| {
            let opt = FieldOptions::from_attributes(&f.attrs).unwrap();
            let name = f.ident.as_ref().unwrap();
            (opt.rename.unwrap_or_else(|| name.to_string()), name)
        })
        .collect::<Vec<_>>();
    ret.sort_by(|l, r| l.0.cmp(&r.0));
    ret
}
fn self_type_name() -> Expr {
    parse_quote! {
        internal::std::any::type_name::<Self>()
    }
}
fn auto_derive_attr() -> Attribute {
    Attribute {
        pound_token: Default::default(),
        style: syn::AttrStyle::Outer,
        bracket_token: Default::default(),
        meta: syn::Meta::Path(Path {
            leading_colon: None,
            segments: {
                let mut ret = Punctuated::new();
                ret.push(PathSegment {
                    ident: Ident::new("automatically_derived", proc_macro2::Span::call_site()),
                    arguments: PathArguments::None,
                });
                ret
            },
        }),
    }
}

fn wrap<T: quote::ToTokens>(
    crate_name: &TokenStream,
    parent_mod: &TokenStream,
    inputs: &[T],
) -> TokenStream {
    struct Many<'a, T>(&'a [T]);
    impl<'a, T: quote::ToTokens> quote::ToTokens for Many<'a, T> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            for i in self.0 {
                quote::ToTokens::to_tokens(i, tokens)
            }
        }
    }
    let t = Many(inputs);

    quote! {
        const _ : () =  {
            extern crate #crate_name;

            use #parent_mod::internal;

            #t
        };
    }
}

// pass crate_name and parent_mod so that macro can be reexported

pub fn derive_from_cbor(
    crate_name: &TokenStream,
    parent_mod: &TokenStream,
    input: DeriveInput,
) -> TokenStream {
    wrap(
        crate_name,
        parent_mod,
        &[cbor::decode::from_cbor_impl(input)],
    )
}

pub fn derive_to_cbor(
    crate_name: &TokenStream,
    parent_mod: &TokenStream,
    input: DeriveInput,
) -> TokenStream {
    wrap(crate_name, parent_mod, &[cbor::encode::to_cbor_impl(input)])
}

pub fn derive_cbor_codec(
    crate_name: &TokenStream,
    parent_mod: &TokenStream,
    input: DeriveInput,
) -> TokenStream {
    wrap(
        crate_name,
        parent_mod,
        &[
            cbor::encode::to_cbor_impl(input.clone()),
            cbor::decode::from_cbor_impl(input),
        ],
    )
}
