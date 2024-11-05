use std::iter::once;

use darling::FromAttributes;
use proc_macro2::Span;
use syn::{
    parse_quote, punctuated::Punctuated, Arm, Block, Data, DataEnum, DataStruct, DeriveInput, Expr,
    ExprCall, ExprMatch, ExprPath, ExprStruct, FieldValue, Fields, FieldsNamed, FieldsUnnamed,
    GenericParam, Generics, Ident, ImplItem, ImplItemFn, ItemImpl, Member, Pat, PatIdent, Path,
    Stmt, TraitBound, TraitBoundModifier, TypeParam, TypeParamBound,
};

use crate::codec::{
    auto_derive_attr, generics, rename::camel_to_snake, self_type_name, sort_fields, EnumOptions,
    Rename, StructOptions, VariantOptions,
};

fn tuple_variant(name: Path, expr: impl IntoIterator<Item = Expr>) -> Expr {
    Expr::Call(ExprCall {
        attrs: Vec::new(),
        func: Box::new(Expr::Path(ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: name,
        })),
        paren_token: Default::default(),
        args: expr.into_iter().collect(),
    })
}
fn ok(expr: Expr) -> Expr {
    tuple_variant(parse_quote!(internal::std::result::Result::Ok), once(expr))
}
fn err(expr: Expr) -> Expr {
    tuple_variant(parse_quote!(internal::std::result::Result::Err), once(expr))
}

fn derive_struct(decoder: &Ident, fields: FieldsNamed) -> Block {
    let type_name = self_type_name();
    let dec = Ident::new("__struct_dec", Span::call_site());
    let len = fields.named.len();
    let fields = sort_fields(&fields);
    Block {
        brace_token: Default::default(),
        stmts: Vec::from([
            parse_quote! {
                let mut #dec = internal::decoding::Decoder::decode_struct_len(
                    #decoder,
                    #type_name,
                    #len
                )?;
            },
            Stmt::Expr(
                ok(Expr::Struct(ExprStruct {
                    attrs: Vec::new(),
                    qself: None,
                    path: parse_quote!(Self),
                    brace_token: Default::default(),
                    fields: fields
                        .into_iter()
                        .map(|(field_name, field)| FieldValue {
                            attrs: Vec::new(),
                            member: Member::Named(field.clone()),
                            colon_token: Some(Default::default()),
                            expr: parse_quote! {
                                internal::decoding::StructDecoder::next_field_of(
                                    &mut #dec,
                                    #field_name
                                )?
                            },
                        })
                        .collect(),
                    dot2_token: None,
                    rest: None,
                })),
                None,
            ),
        ]),
    }
}
fn derive_tuple_struct(decoder: &Ident, fields: FieldsUnnamed) -> Block {
    let type_name = self_type_name();
    let dec = Ident::new("__struct_dec", Span::call_site());
    let len = fields.unnamed.len();
    Block {
        brace_token: Default::default(),
        stmts: Vec::from([
            parse_quote! {
                let mut #dec = internal::decoding::Decoder::decode_tuple_struct_len(
                    #decoder,
                    #type_name,
                    #len
                )?;
            },
            Stmt::Expr(
                ok(Expr::Call(ExprCall {
                    attrs: Vec::new(),
                    func: parse_quote!(Self),
                    paren_token: Default::default(),
                    args: (0..len)
                        .map(|_| -> Expr {
                            parse_quote! {
                                internal::decoding::TupleStructDecoder::next_field(&mut #dec)?
                            }
                        })
                        .collect(),
                })),
                None,
            ),
        ]),
    }
}
fn derive_struct_transparent(decoder: &Ident, s: DataStruct) -> Block {
    match s.fields {
        Fields::Unit => panic!("Transparent derive for unit struct is not allowed"),
        Fields::Unnamed(u) if u.unnamed.len() == 1 => Block {
            brace_token: Default::default(),
            stmts: Vec::from([Stmt::Expr(
                parse_quote! {
                    internal::decoding::FromCbor::decode(#decoder).map(Self)
                },
                None,
            )]),
        },
        Fields::Named(n) if n.named.len() == 1 => {
            let name = n.named[0].ident.as_ref().unwrap();
            Block {
                brace_token: Default::default(),
                stmts: Vec::from([Stmt::Expr(
                    parse_quote! {
                        internal::decoding::FromCbor::decode(#decoder).map(|v| Self {
                            #name: v
                        })
                    },
                    None,
                )]),
            }
        }
        _ => panic!("Transparent deriving on struct with more than one field is not supported"),
    }
}

fn enum_size_mismatch(
    type_name: &Ident,
    variant_name: &str,
    expected: usize,
    pat: impl FnOnce(&Ident) -> Pat,
) -> Arm {
    let actual_len = Ident::new("__len", Span::call_site());
    Arm {
        attrs: Vec::new(),
        pat: pat(&actual_len),
        guard: None,
        fat_arrow_token: Default::default(),
        body: Box::new(err(parse_quote! {
            internal::decoding::Error::variant_size_mismatch(
                #type_name,
                #variant_name,
                #actual_len,
                #expected
            )
        })),
        comma: Some(Default::default()),
    }
}
fn derive_tuple_enum(
    type_name: &Ident,
    variant: &Ident,
    variant_name: &str,
    fields: FieldsUnnamed,
) -> (Arm, Arm) {
    let len = fields.unnamed.len();
    (
        {
            let dec = Ident::new("__variant_dec", Span::call_site());
            Arm {
                attrs: Vec::new(),
                pat: parse_quote!(internal::decoding::Enum::Tuple(#variant_name, #len, mut #dec)),
                guard: None,
                fat_arrow_token: Default::default(),
                body: Box::new(ok(tuple_variant(
                    parse_quote!(Self::#variant),
                    (0..len).map(|_| {
                        parse_quote! {
                            internal::decoding::TupleVariantDecoder::next_field(&mut #dec)?
                        }
                    }),
                ))),
                comma: Some(Default::default()),
            }
        },
        enum_size_mismatch(type_name, variant_name, len, |actual_len| {
            parse_quote! {
                internal::decoding::Enum::Tuple(#variant_name, #actual_len, _)
            }
        }),
    )
}
fn derive_struct_enum(
    type_name: &Ident,
    variant: &Ident,
    variant_name: &str,
    fields: FieldsNamed,
) -> (Arm, Arm) {
    let len = fields.named.len();
    (
        {
            let dec = Ident::new("__variant_dec", Span::call_site());
            Arm {
                attrs: Vec::new(),
                pat: parse_quote! {
                    internal::decoding::Enum::Struct(#variant_name, #len, mut #dec)
                },
                guard: None,
                fat_arrow_token: Default::default(),
                body: Box::new(ok(Expr::Struct(ExprStruct {
                    attrs: Vec::new(),
                    qself: None,
                    path: parse_quote!(Self::#variant),
                    brace_token: Default::default(),
                    fields: sort_fields(&fields)
                        .into_iter()
                        .map(|(name, field)| FieldValue {
                            attrs: Vec::new(),
                            member: Member::Named(field.clone()),
                            colon_token: Some(Default::default()),
                            expr: parse_quote! {
                                internal::decoding::StructVariantDecoder::next_field_of(
                                    &mut #dec,
                                    #name
                                )?
                            },
                        })
                        .collect(),
                    dot2_token: Default::default(),
                    rest: None,
                }))),
                comma: Some(Default::default()),
            }
        },
        enum_size_mismatch(type_name, variant_name, len, |actual_len| {
            parse_quote! {
                internal::decoding::Enum::Struct(#variant_name, #actual_len, _)
            }
        }),
    )
}

fn derive_enum(decoder: &Ident, enum_opt: EnumOptions, e: DataEnum) -> Block {
    let variants =
        e.variants
            .into_iter()
            .map(|v| {
                let opt = VariantOptions::from_attributes(&v.attrs).unwrap();
                let name = opt.rename.unwrap_or_else(|| {
                    match enum_opt.rename_variants.expect(
                        "When rename_variants is missing, all variants must have rename attr",
                    ) {
                        Rename::SnakeCase => camel_to_snake(&v.ident.to_string()),
                    }
                });
                (v, name)
            })
            .collect::<Vec<_>>();
    let name_len = variants.iter().map(|v| v.1.len()).max().unwrap_or(0);
    let mut arms = Vec::new();
    let type_name = Ident::new("__type", Span::call_site());
    let name_buf = Ident::new("__variant", Span::call_site());

    for (variant, variant_name) in variants {
        match variant.fields {
            Fields::Unit => {
                let ident = variant.ident;
                arms.push(Arm {
                    attrs: Vec::new(),
                    pat: parse_quote!(internal::decoding::Enum::Unit(#variant_name)),
                    guard: None,
                    fat_arrow_token: Default::default(),
                    body: Box::new(ok(parse_quote!(Self::#ident))),
                    comma: Some(Default::default()),
                })
            }
            Fields::Unnamed(u) => {
                let (suc, fail) = derive_tuple_enum(&type_name, &variant.ident, &variant_name, u);
                arms.push(suc);
                arms.push(fail);
            }
            Fields::Named(n) => {
                let (suc, fail) = derive_struct_enum(&type_name, &variant.ident, &variant_name, n);
                arms.push(suc);
                arms.push(fail);
            }
        }
    }

    {
        let v = Ident::new("e", Span::call_site());
        let error = parse_quote! {
            internal::decoding::Error::unknown_enum_variant(#type_name, #v)
        };
        arms.push(Arm {
            attrs: Vec::new(),
            pat: Pat::Ident(PatIdent {
                attrs: Vec::new(),
                by_ref: None,
                mutability: None,
                ident: v,
                subpat: None,
            }),
            guard: None,
            fat_arrow_token: Default::default(),
            body: Box::new(err(error)),
            comma: Some(Default::default()),
        })
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
            parse_quote! {
                let mut #name_buf = [0u8; #name_len];
            },
            Stmt::Expr(
                Expr::Match(ExprMatch {
                    attrs: Vec::new(),
                    match_token: Default::default(),
                    expr: parse_quote! {
                        internal::decoding::Decoder::decode_enum(
                            #decoder,
                            #type_name,
                            &mut #name_buf
                        )?
                    },
                    brace_token: Default::default(),
                    arms,
                }),
                None,
            ),
        ]),
    }
}

pub fn from_cbor_impl(input: DeriveInput) -> ItemImpl {
    let decoder = Ident::new("__decoder", Span::call_site());
    let body = match input.data {
        Data::Struct(s) => {
            let opt = StructOptions::from_attributes(&input.attrs).unwrap();
            if opt.transparent {
                derive_struct_transparent(&decoder, s)
            } else {
                match s.fields {
                    Fields::Unit => panic!("Deriving on unit struct is not supported"),
                    Fields::Unnamed(u) => derive_tuple_struct(&decoder, u),
                    Fields::Named(n) => derive_struct(&decoder, n),
                }
            }
        }
        Data::Enum(e) => derive_enum(
            &decoder,
            EnumOptions::from_attributes(&input.attrs).unwrap(),
            e,
        ),
        Data::Union(_) => panic!("Deriving on union is not supported"),
    };

    let reader = Ident::new("__Reader", Span::call_site());

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
            let (mut params, where_clause) = generics::generics_bounds(
                &TypeParamBound::Trait(TraitBound {
                    paren_token: None,
                    modifier: TraitBoundModifier::None,
                    lifetimes: None,
                    path: parse_quote! {
                        internal::decoding::FromCbor<#reader>
                    },
                }),
                input.generics,
            );
            params.push(GenericParam::Type(TypeParam {
                attrs: Vec::new(),
                ident: reader.clone(),
                colon_token: Some(Default::default()),
                bounds: {
                    let mut ret = Punctuated::new();
                    ret.push(TypeParamBound::Trait(TraitBound {
                        paren_token: None,
                        modifier: TraitBoundModifier::None,
                        lifetimes: None,
                        path: parse_quote! {
                            internal::decoding::Read
                        },
                    }));
                    ret
                },
                eq_token: None,
                default: None,
            }));
            Generics {
                lt_token: Some(Default::default()),
                params,
                gt_token: Some(Default::default()),
                where_clause,
            }
        },
        trait_: Some((
            None,
            parse_quote!(internal::decoding::FromCbor<#reader>),
            Default::default(),
        )),
        brace_token: Default::default(),
        items: Vec::from([ImplItem::Fn(ImplItemFn {
            attrs: Vec::new(),
            vis: syn::Visibility::Inherited,
            defaultness: None,
            sig: parse_quote! {
                fn decode<'__dec>(
                    #decoder : internal::decoding::Decoder<#reader>
                ) -> internal::std::result::Result<
                    Self,
                    internal::decoding::Error<#reader::Error>
                >
            },
            block: body,
        })]),
    }
}
