use darling::{FromAttributes, FromMeta};
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    parse_quote, punctuated::Punctuated, token::Comma, AngleBracketedGenericArguments, Arm,
    AttrStyle, Attribute, Block, ConstParam, Data, DataEnum, DeriveInput, Expr, ExprBlock,
    ExprMatch, ExprPath, FieldPat, Fields, FieldsNamed, FieldsUnnamed, GenericArgument,
    GenericParam, Generics, ImplItem, ImplItemFn, Index, ItemImpl, LifetimeParam, Member, Meta,
    Pat, PatIdent, PatStruct, PatTupleStruct, Path, PathArguments, PathSegment, PredicateLifetime,
    PredicateType, Stmt, TraitBound, TraitBoundModifier, Type, TypeParam, TypeParamBound, TypePath,
    WhereClause, WherePredicate,
};

#[derive(Default, FromMeta)]
struct Bound {
    #[darling(default)]
    serialize: Option<String>,
}

#[derive(FromAttributes)]
#[darling(attributes(serde))]
struct Options {
    #[darling(default)]
    transparent: bool,
    #[darling(default)]
    bound: Bound,
}

#[derive(FromAttributes)]
#[darling(attributes(serde))]
struct BodyOptions {
    rename: Option<String>,
}

#[derive(FromAttributes)]
#[darling(attributes(serde))]
struct VariantOptions {
    rename: String,
}

fn derive_struct(serializer: &Ident, data: FieldsNamed) -> Block {
    let fields = {
        let mut ret = data
            .named
            .iter()
            .map(|f| {
                let opt = BodyOptions::from_attributes(&f.attrs).unwrap();
                let name = f.ident.as_ref().unwrap();
                (opt.rename.unwrap_or_else(|| name.to_string()), name)
            })
            .collect::<Vec<_>>();
        ret.sort_by(|v1, v2| v1.0.cmp(&v2.0));
        ret
    };
    let ser = Ident::new("__struct_ser", Span::call_site());
    let mut stmts = Vec::new();
    let len = fields.len();
    stmts.push(parse_quote! {
        let mut #ser = webar_data::ser::Serializer::serialize_struct(#serializer, #len)?;
    });
    stmts.extend(fields.into_iter().map(|(name, field)| {
        Stmt::Expr(
            parse_quote! {
                webar_data::ser::SerializeStruct::serialize_field(&mut #ser, #name, &self.#field)?
            },
            Some(Default::default()),
        )
    }));
    stmts.push(Stmt::Expr(
        parse_quote!(webar_data::ser::SerializeStruct::end(#ser)),
        None,
    ));
    Block {
        brace_token: Default::default(),
        stmts,
    }
}

fn derive_tuple_struct(serializer: &Ident, data: FieldsUnnamed) -> Block {
    let ser = Ident::new("__struct_str", Span::call_site());
    let len = data.unnamed.len();
    let mut stmts = Vec::new();
    stmts.push(parse_quote! {
        let mut #ser = webar_data::ser::Serializer::serialize_tuple_struct(#serializer, #len)?;
    });
    for (idx, _) in data.unnamed.iter().enumerate() {
        let idx = Index {
            index: idx as u32,
            span: Span::call_site(),
        };
        stmts.push(Stmt::Expr(
            parse_quote! {
                webar_data::ser::SerializeTupleStruct::serialize_field(&mut #ser, &self.#idx)?
            },
            Some(Default::default()),
        ));
    }
    stmts.push(Stmt::Expr(
        parse_quote! { webar_data::ser::SerializeTupleStruct::end(#ser) },
        None,
    ));
    Block {
        brace_token: Default::default(),
        stmts,
    }
}

fn derive_struct_variant(serializer: &Ident, ident: Ident, name: &str, f: FieldsNamed) -> Arm {
    let fields = {
        let mut ret = f
            .named
            .iter()
            .map(|f| {
                let opt = BodyOptions::from_attributes(&f.attrs).unwrap();
                let ident = f.ident.as_ref().unwrap();
                (opt.rename.unwrap_or_else(|| ident.to_string()), ident)
            })
            .collect::<Vec<_>>();
        ret.sort_by(|l, r| l.0.cmp(&r.0));
        ret
    };
    let mut pat = Punctuated::new();
    let ser = Ident::new("__variant_ser", Span::call_site());
    let len = f.named.len();
    let mut stmts = Vec::new();
    stmts.push(
        parse_quote! {
            let mut #ser = webar_data::ser::Serializer::serialize_struct_variant(#serializer, #name, #len)?;
        }
    );
    for (name, field) in fields {
        pat.push(FieldPat {
            attrs: Vec::new(),
            member: Member::Named(field.clone()),
            colon_token: None,
            pat: Box::new(Pat::Ident(PatIdent {
                attrs: Vec::new(),
                by_ref: None,
                mutability: None,
                ident: field.clone(),
                subpat: None,
            })),
        });
        stmts.push(Stmt::Expr(
            parse_quote! {
                webar_data::ser::SerializeStructVariant::serialize_field(&mut #ser, #name, #field)?
            },
            Some(Default::default()),
        ));
    }
    stmts.push(Stmt::Expr(
        parse_quote! { webar_data::ser::SerializeStructVariant::end(#ser) },
        None,
    ));
    Arm {
        attrs: Vec::new(),
        pat: Pat::Struct(PatStruct {
            attrs: Vec::new(),
            qself: None,
            path: parse_quote!(Self::#ident),
            brace_token: Default::default(),
            fields: pat,
            rest: None,
        }),
        guard: None,
        fat_arrow_token: Default::default(),
        body: Box::new(Expr::Block(ExprBlock {
            attrs: Vec::new(),
            label: None,
            block: Block {
                brace_token: Default::default(),
                stmts,
            },
        })),
        comma: None,
    }
}

fn derive_tuple_variant(serializer: &Ident, ident: Ident, name: &str, f: FieldsUnnamed) -> Arm {
    let mut pat = Punctuated::new();
    let ser = Ident::new("__variant_ser", Span::call_site());
    let len = f.unnamed.len();
    let mut stmts = Vec::new();
    stmts.push(
        parse_quote! {
            let mut #ser = webar_data::ser::Serializer::serialize_tuple_variant(#serializer, #name, #len)?;
        }
    );
    for (idx, _) in f.unnamed.iter().enumerate() {
        let field = Ident::new(&format!("__field_{idx}"), Span::call_site());
        stmts.push(Stmt::Expr(
            parse_quote! {
                webar_data::ser::SerializeTupleVariant::serialize_field(&mut #ser, #field)?
            },
            Some(Default::default()),
        ));
        pat.push(Pat::Ident(PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident: field,
            subpat: None,
        }));
    }
    stmts.push(Stmt::Expr(
        parse_quote! {webar_data::ser::SerializeTupleVariant::end(#ser)},
        None,
    ));
    Arm {
        attrs: Vec::new(),
        pat: Pat::TupleStruct(PatTupleStruct {
            attrs: Vec::new(),
            qself: None,
            path: parse_quote!(Self::#ident),
            paren_token: Default::default(),
            elems: pat,
        }),
        guard: None,
        fat_arrow_token: Default::default(),
        body: Box::new(Expr::Block(ExprBlock {
            attrs: Vec::new(),
            label: None,
            block: Block {
                brace_token: Default::default(),
                stmts,
            },
        })),
        comma: None,
    }
}

fn derive_enum(serializer: &Ident, data: DataEnum) -> Block {
    let mut arms = Vec::new();
    for v in data.variants {
        let opt = VariantOptions::from_attributes(&v.attrs).unwrap();
        let name = opt.rename;
        arms.push(match v.fields {
            Fields::Named(n) => derive_struct_variant(serializer, v.ident, &name, n),
            Fields::Unnamed(u) if u.unnamed.len() == 1 => {
                let ident = v.ident;
                let field = Ident::new("f", Span::call_site());
                Arm {
                    attrs: Vec::new(),
                    pat: Pat::TupleStruct(PatTupleStruct {
                        attrs: Vec::new(),
                        qself: None,
                        path: parse_quote!(Self::#ident),
                        paren_token: Default::default(),
                        elems: {
                            let mut ret = Punctuated::new();
                            ret.push(Pat::Ident(PatIdent {
                                attrs: Vec::new(),
                                by_ref: None,
                                mutability: None,
                                ident: field.clone(),
                                subpat: None,
                            }));
                            ret
                        },
                    }),
                    guard: None,
                    fat_arrow_token: Default::default(),
                    body: Box::new(parse_quote! {
                        webar_data::ser::Serializer::serialize_newtype_variant(#serializer, #name, #field)
                    }),
                    comma: Some(Default::default()),
                }
            }
            Fields::Unnamed(u) => derive_tuple_variant(serializer, v.ident, &name, u),
            Fields::Unit => {
                let ident = v.ident;
                Arm {
                    attrs: Vec::new(),
                    pat: Pat::Path(parse_quote!(Self::#ident)),
                    guard: None,
                    fat_arrow_token: Default::default(),
                    body: Box::new(parse_quote! {
                        webar_data::ser::Serializer::serialize_unit_variant(#serializer, #name)
                    }),
                    comma: Some(Default::default()),
                }
            }
        });
    }
    Block {
        brace_token: Default::default(),
        stmts: Vec::from([Stmt::Expr(
            Expr::Match(ExprMatch {
                attrs: Vec::new(),
                match_token: Default::default(),
                expr: Box::new(parse_quote!(&self)),
                brace_token: Default::default(),
                arms,
            }),
            None,
        )]),
    }
}

fn derive_transparent(serializer: &Ident, data: Data) -> Block {
    match data {
        Data::Struct(f) if f.fields.len() == 1 => match f.fields {
            Fields::Named(fs) => {
                let name = fs.named[0].ident.as_ref().unwrap();
                Block {
                    brace_token: Default::default(),
                    stmts: Vec::from([Stmt::Expr(
                        parse_quote! {
                            webar_data::ser::Serialize::serialize(&self.#name, #serializer)
                        },
                        None,
                    )]),
                }
            }
            Fields::Unnamed(_) => Block {
                brace_token: Default::default(),
                stmts: Vec::from([Stmt::Expr(
                    parse_quote! {
                        webar_data::ser::Serialize::serialize(&self.0, #serializer)
                    },
                    None,
                )]),
            },
            Fields::Unit => panic!("transparent deserialize unit struct is not supported"),
        },
        Data::Struct(_) => panic!("transparent deserialize require strust has only one field"),
        Data::Enum(_) => {
            panic!("transparent deserialize enum is not supported")
        }
        Data::Union(_) => panic!("union is not supported"),
    }
}

fn ident_path(ident: Ident) -> Path {
    Path {
        leading_colon: None,
        segments: {
            let mut ret = Punctuated::new();
            ret.push(PathSegment {
                ident,
                arguments: PathArguments::None,
            });
            ret
        },
    }
}

fn infer_impl_generics(
    orig_param: Punctuated<GenericParam, Comma>,
    orig_clause: Option<WhereClause>,
) -> (
    Punctuated<GenericParam, Comma>,
    Punctuated<WherePredicate, Comma>,
) {
    let mut params = Punctuated::new();
    let mut predicates = Punctuated::new();
    for t in orig_param {
        match t {
            GenericParam::Const(c) => params.push(GenericParam::Const(ConstParam {
                attrs: Vec::new(),
                const_token: Default::default(),
                ident: c.ident,
                colon_token: Default::default(),
                ty: c.ty,
                eq_token: None,
                default: None,
            })),
            GenericParam::Lifetime(l) => {
                if !l.bounds.is_empty() {
                    predicates.push(WherePredicate::Lifetime(PredicateLifetime {
                        lifetime: l.lifetime.clone(),
                        colon_token: Default::default(),
                        bounds: l.bounds,
                    }));
                }
                params.push(GenericParam::Lifetime(LifetimeParam {
                    attrs: Vec::new(),
                    lifetime: l.lifetime,
                    colon_token: None,
                    bounds: Punctuated::new(),
                }));
            }
            GenericParam::Type(t) => {
                predicates.push(WherePredicate::Type(PredicateType {
                    lifetimes: None,
                    bounded_ty: Type::Path(TypePath {
                        qself: None,
                        path: ident_path(t.ident.clone()),
                    }),
                    colon_token: Default::default(),
                    bounds: {
                        let mut ret = t.bounds.clone();
                        ret.push(TypeParamBound::Trait(TraitBound {
                            paren_token: None,
                            modifier: TraitBoundModifier::None,
                            lifetimes: None,
                            path: parse_quote!(webar_data::ser::Serialize),
                        }));
                        ret
                    },
                }));
                params.push(GenericParam::Type(TypeParam {
                    attrs: Vec::new(),
                    ident: t.ident,
                    colon_token: None,
                    bounds: Punctuated::new(),
                    eq_token: None,
                    default: None,
                }));
            }
        }
    }
    if let Some(w) = orig_clause {
        predicates.extend(w.predicates);
    }
    (params, predicates)
}

fn clear_param_bounds(params: Punctuated<GenericParam, Comma>) -> Punctuated<GenericParam, Comma> {
    params
        .into_iter()
        .map(|t| match t {
            GenericParam::Const(c) => GenericParam::Const(ConstParam {
                attrs: Vec::new(),
                const_token: c.const_token,
                ident: c.ident,
                colon_token: c.colon_token,
                ty: c.ty,
                eq_token: None,
                default: None,
            }),
            GenericParam::Lifetime(l) => GenericParam::Lifetime(LifetimeParam {
                attrs: Vec::new(),
                lifetime: l.lifetime,
                colon_token: l.colon_token,
                bounds: Punctuated::new(),
            }),
            GenericParam::Type(t) => GenericParam::Type(TypeParam {
                attrs: Vec::new(),
                ident: t.ident,
                colon_token: t.colon_token,
                bounds: Punctuated::new(),
                eq_token: None,
                default: None,
            }),
        })
        .collect()
}

fn impl_generics(opt: Options, orig: Generics) -> Generics {
    let (params, predicates) = match opt.bound.serialize {
        Some(b) => (clear_param_bounds(orig.params), {
            let mut ret = Punctuated::new();
            for s in b.split(',') {
                ret.push(syn::parse_str(s).unwrap());
            }
            ret
        }),
        None => infer_impl_generics(orig.params, orig.where_clause),
    };
    Generics {
        lt_token: Some(Default::default()),
        params,
        gt_token: Some(Default::default()),
        where_clause: Some(WhereClause {
            where_token: Default::default(),
            predicates,
        }),
    }
}

fn instantiate_type(g: Punctuated<GenericParam, Comma>, ident: Ident) -> Type {
    let mut segments = Punctuated::new();
    segments.push(PathSegment {
        ident,
        arguments: if g.is_empty() {
            PathArguments::None
        } else {
            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token: Default::default(),
                args: g
                    .into_iter()
                    .map(|p| match p {
                        GenericParam::Const(c) => GenericArgument::Const(Expr::Path(ExprPath {
                            attrs: Vec::new(),
                            qself: None,
                            path: ident_path(c.ident),
                        })),
                        GenericParam::Lifetime(l) => GenericArgument::Lifetime(l.lifetime.clone()),
                        GenericParam::Type(t) => GenericArgument::Type(Type::Path(TypePath {
                            qself: None,
                            path: ident_path(t.ident),
                        })),
                    })
                    .collect(),
                gt_token: Default::default(),
            })
        },
    });
    Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments,
        },
    })
}

pub fn derive_serialize(input: DeriveInput) -> TokenStream {
    let options = Options::from_attributes(&input.attrs).unwrap();
    let serializer = Ident::new("serializer", Span::call_site());
    let body = if options.transparent {
        derive_transparent(&serializer, input.data)
    } else {
        match input.data {
            Data::Enum(e) => derive_enum(&serializer, e),
            Data::Struct(s) => match s.fields {
                Fields::Named(n) => derive_struct(&serializer, n),
                Fields::Unnamed(u) => derive_tuple_struct(&serializer, u),
                Fields::Unit => panic!("Unit struct is not supported"),
            },
            Data::Union(_) => {
                panic!("Union is not supported")
            }
        }
    };
    let imp = ItemImpl {
        attrs: Vec::from([Attribute {
            pound_token: Default::default(),
            style: AttrStyle::Outer,
            bracket_token: Default::default(),
            meta: Meta::Path(ident_path(Ident::new(
                "automatically_derived",
                Span::call_site(),
            ))),
        }]),
        defaultness: None,
        unsafety: None,
        impl_token: Default::default(),
        self_ty: Box::new(instantiate_type(
            input.generics.params.clone(),
            input.ident.clone(),
        )),
        generics: impl_generics(options, input.generics),
        trait_: Some((
            None,
            parse_quote!(webar_data::ser::Serialize),
            Default::default(),
        )),
        brace_token: Default::default(),
        items: Vec::from([ImplItem::Fn(ImplItemFn {
            attrs: Vec::new(),
            vis: syn::Visibility::Inherited,
            defaultness: None,
            sig: parse_quote! {
                fn serialize<__S: webar_data::ser::Serializer>(&self, #serializer: __S) ->
                    webar_data::__internal::Result<__S::Ok,__S::Error>
            },
            block: body,
        })]),
    };
    quote! {
        const _ : () = {
            extern crate webar_data;
            #imp
        };
    }
}
