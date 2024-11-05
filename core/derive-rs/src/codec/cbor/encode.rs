use darling::FromAttributes;
use proc_macro2::Span;
use quote::format_ident;
use syn::{
    parse_quote, punctuated::Punctuated, Arm, Block, Data, DataEnum, DataStruct, DeriveInput, Expr,
    ExprBlock, ExprMatch, FieldPat, Fields, FieldsNamed, FieldsUnnamed, Generics, Ident, ImplItem,
    ImplItemFn, Index, ItemImpl, Member, Pat, PatIdent, PatStruct, PatTupleStruct, Stmt,
    TraitBound,
};

use crate::codec::{
    auto_derive_attr, generics, rename::camel_to_snake, self_type_name, sort_fields, EnumOptions,
    Rename, StructOptions, VariantOptions,
};

fn derive_struct(encoder: &Ident, fields: FieldsNamed) -> Block {
    let fields = sort_fields(&fields);
    let mut stmts = Vec::new();
    let len = fields.len();
    let ser = Ident::new("__struct_enc", Span::call_site());
    let type_name = self_type_name();
    stmts.push(parse_quote! {
        let mut #ser = internal::encoding::Encoder::encode_struct(
            #encoder,
            #type_name,
            #len
        )?;
    });
    stmts.extend(fields.into_iter().map(|(name, field)| {
        parse_quote! {
            internal::encoding::StructEncoder::encode_field(&mut #ser, #name, &self.#field)?;
        }
    }));
    stmts.push(Stmt::Expr(
        parse_quote! {
            internal::encoding::StructEncoder::end(#ser)
        },
        None,
    ));
    Block {
        brace_token: Default::default(),
        stmts,
    }
}

fn derive_tuple_struct(encoder: &Ident, fields: FieldsUnnamed) -> Block {
    let mut stmts = Vec::new();
    let len = fields.unnamed.len();
    let ser = Ident::new("__struct_enc", Span::call_site());
    let type_name = self_type_name();
    stmts.push(parse_quote! {
        let mut #ser = internal::encoding::Encoder::encode_tuple_struct(
            #encoder,
            #type_name,
            #len
        )?;
    });
    for i in 0..fields.unnamed.len() {
        let idx = Index {
            index: i as u32,
            span: Span::call_site(),
        };
        stmts.push(parse_quote! {
            internal::encoding::TupleStructEncoder::encode_field(&mut #ser, &self.#idx)?;
        });
    }
    stmts.push(Stmt::Expr(
        parse_quote! {
            internal::encoding::TupleStructEncoder::end(#ser)
        },
        None,
    ));
    Block {
        brace_token: Default::default(),
        stmts,
    }
}

fn derive_struct_transparent(s: DataStruct, encoder: &Ident) -> Block {
    match s.fields {
        Fields::Named(n) if n.named.len() == 1 => {
            let name = n.named[0].ident.as_ref().unwrap();
            Block {
                brace_token: Default::default(),
                stmts: Vec::from([Stmt::Expr(
                    parse_quote! {
                        internal::encoding::ToCbor::encode(&self.#name, #encoder)
                    },
                    None,
                )]),
            }
        }
        Fields::Unnamed(u) if u.unnamed.len() == 1 => Block {
            brace_token: Default::default(),
            stmts: Vec::from([Stmt::Expr(
                parse_quote! {
                    internal::encoding::ToCbor::encode(&self.0,#encoder)
                },
                None,
            )]),
        },
        Fields::Unit => panic!("Transparent derive on unit struct is not allowed"),
        _ => panic!("Transparent derive on struct with more than one field is not supported"),
    }
}

fn derive_struct_variant(
    encoder: &Ident,
    type_name: &Ident,
    ident: &Ident,
    variant_name: &str,
    fields: FieldsNamed,
) -> (Pat, Expr) {
    let fields = sort_fields(&fields);
    let len = fields.len();
    let ser = Ident::new("__enum_enc", Span::call_site());
    let mut pat = Punctuated::new();
    let mut stmts = Vec::new();
    stmts.push(parse_quote! {
        let mut #ser = internal::encoding::Encoder::encode_struct_variant(
            #encoder,
            #type_name,
            #variant_name,
            #len
        )?;
    });
    for (name, field) in fields {
        let f = format_ident!("__f_{field}");
        stmts.push(parse_quote! {
            internal::encoding::StructVariantEncoder::encode_field(
                &mut #ser,
                #name,
                #f
            )?;
        });
        pat.push(FieldPat {
            attrs: Vec::new(),
            member: Member::Named(field.clone()),
            colon_token: Some(Default::default()),
            pat: Box::new(Pat::Ident(PatIdent {
                attrs: Vec::new(),
                by_ref: None,
                mutability: None,
                ident: f,
                subpat: None,
            })),
        });
    }
    stmts.push(Stmt::Expr(
        parse_quote! {
            internal::encoding::StructVariantEncoder::end(#ser)
        },
        None,
    ));

    (
        Pat::Struct(PatStruct {
            attrs: Vec::new(),
            qself: None,
            path: parse_quote! {Self::#ident},
            brace_token: Default::default(),
            fields: pat,
            rest: None,
        }),
        Expr::Block(ExprBlock {
            attrs: Vec::new(),
            label: None,
            block: Block {
                brace_token: Default::default(),
                stmts,
            },
        }),
    )
}

fn derive_tuple_variant(
    encoder: &Ident,
    type_name: &Ident,
    ident: &Ident,
    variant_name: &str,
    fields: FieldsUnnamed,
) -> (Pat, Expr) {
    let len = fields.unnamed.len();
    let ser = Ident::new("__enum_enc", Span::call_site());
    let mut pat = Punctuated::new();
    let mut stmts = Vec::new();
    stmts.push(parse_quote! {
        let mut #ser = internal::encoding::Encoder::encode_tuple_variant(
            #encoder,
            #type_name,
            #variant_name,
            #len
        )?;
    });
    for i in 0..len {
        let f = format_ident!("__field_{i}");
        stmts.push(parse_quote! {
            internal::encoding::TupleVariantEncoder::encode_field(
                &mut #ser,
                #f
            )?;
        });
        pat.push(Pat::Ident(PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident: f,
            subpat: None,
        }));
    }
    stmts.push(Stmt::Expr(
        parse_quote! {
            internal::encoding::TupleVariantEncoder::end(#ser)
        },
        None,
    ));

    (
        Pat::TupleStruct(PatTupleStruct {
            attrs: Vec::new(),
            qself: None,
            path: parse_quote! {Self::#ident},
            paren_token: Default::default(),
            elems: pat,
        }),
        Expr::Block(ExprBlock {
            attrs: Vec::new(),
            label: None,
            block: Block {
                brace_token: Default::default(),
                stmts,
            },
        }),
    )
}

fn derive_enum(encoder: &Ident, enum_opt: EnumOptions, input: DataEnum) -> Block {
    let type_name = Ident::new("__type", Span::call_site());
    let mut arms = Vec::new();
    for var in input.variants {
        let var_opt = VariantOptions::from_attributes(&var.attrs).unwrap();
        let ident = var.ident;
        let variant_name = var_opt.rename.unwrap_or_else(|| {
            match enum_opt
                .rename_variants
                .expect("When rename_variants is missing, all variants must have rename")
            {
                Rename::SnakeCase => camel_to_snake(&ident.to_string()),
            }
        });
        let ((pat, expr), comma) = match var.fields {
            Fields::Unit => (
                (
                    parse_quote! { Self::#ident },
                    parse_quote! {
                        internal::encoding::Encoder::encode_unit_variant(
                            #encoder,
                            #type_name,
                            #variant_name
                        )
                    },
                ),
                Some(Default::default()),
            ),
            Fields::Named(n) => (
                derive_struct_variant(encoder, &type_name, &ident, &variant_name, n),
                None,
            ),
            Fields::Unnamed(u) => (
                derive_tuple_variant(encoder, &type_name, &ident, &variant_name, u),
                None,
            ),
        };
        arms.push(Arm {
            attrs: Vec::new(),
            pat,
            guard: None,
            fat_arrow_token: Default::default(),
            body: Box::new(expr),
            comma,
        });
    }
    Block {
        brace_token: Default::default(),
        stmts: Vec::from([
            {
                let expr = self_type_name();
                parse_quote! {
                    let #type_name = #expr;
                }
            },
            Stmt::Expr(
                Expr::Match(ExprMatch {
                    attrs: Vec::new(),
                    match_token: Default::default(),
                    expr: Box::new(parse_quote!(&self)),
                    brace_token: Default::default(),
                    arms,
                }),
                None,
            ),
        ]),
    }
}

pub fn to_cbor_impl(input: DeriveInput) -> ItemImpl {
    let encoder = Ident::new("__encoder", Span::call_site());
    let body = match input.data {
        Data::Struct(s) => {
            let opt = StructOptions::from_attributes(&input.attrs).unwrap();
            if opt.transparent {
                derive_struct_transparent(s, &encoder)
            } else {
                match s.fields {
                    Fields::Named(n) => derive_struct(&encoder, n),
                    Fields::Unnamed(u) => derive_tuple_struct(&encoder, u),
                    Fields::Unit => panic!("Unit struct is not supported"),
                }
            }
        }
        Data::Enum(e) => derive_enum(
            &encoder,
            EnumOptions::from_attributes(&input.attrs).unwrap(),
            e,
        ),
        Data::Union(_) => panic!("Derive union is not supported"),
    };
    ItemImpl {
        attrs: Vec::from([auto_derive_attr()]),
        defaultness: None,
        unsafety: None,
        impl_token: Default::default(),
        self_ty: Box::new(generics::instantiate_type(
            input.ident,
            input.generics.params.clone(),
        )),
        generics: {
            let (params, clause) = generics::generics_bounds(
                &syn::TypeParamBound::Trait(TraitBound {
                    paren_token: None,
                    modifier: syn::TraitBoundModifier::None,
                    lifetimes: None,
                    path: parse_quote!(internal::encoding::ToCbor),
                }),
                input.generics,
            );
            Generics {
                lt_token: None,
                params,
                gt_token: None,
                where_clause: clause,
            }
        },
        trait_: Some((
            None,
            parse_quote!(internal::encoding::ToCbor),
            Default::default(),
        )),
        brace_token: Default::default(),
        items: Vec::from([ImplItem::Fn(ImplItemFn {
            attrs: Vec::new(),
            vis: syn::Visibility::Inherited,
            defaultness: None,
            sig: parse_quote! {
                fn encode<'__enc, __W: internal::encoding::Write>(
                    &self,
                    #encoder: internal::encoding::Encoder<'__enc, __W>
                ) -> internal::std::result::Result<
                    (),
                    internal::encoding::Error<__W::Error>
                >
            },
            block: body,
        })]),
    }
}
