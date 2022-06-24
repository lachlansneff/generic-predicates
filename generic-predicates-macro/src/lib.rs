use std::mem;

use proc_macro::TokenStream;
use proc_macro2::{Punct, Spacing, Span, TokenTree};
use quote::quote;
use syn::{
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream},
    parse2, parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::{self, Async, Comma, Const, Gt, Lt, Paren, Unsafe, Where},
    Abi, Attribute, Block, BoundLifetimes, ConstParam, Error, Expr, FnArg, GenericParam, Ident,
    Lifetime, LifetimeDef, LitStr, Pat, PatType, PredicateLifetime, PredicateType, ReturnType,
    Token, Type, TypeParam, TypeParamBound, Variadic, Visibility,
};

struct PredicateConst {
    expr: Expr,
    message: Option<LitStr>,
}

struct MyPredicateType {
    pub lifetimes: Option<BoundLifetimes>,
    pub bounded_ty: Type,
    pub colon_token: Token![:],
    pub bounds: Punctuated<TypeParamBound, Token![+]>,
}

impl From<MyPredicateType> for PredicateType {
    fn from(this: MyPredicateType) -> Self {
        Self {
            lifetimes: this.lifetimes,
            bounded_ty: this.bounded_ty,
            colon_token: this.colon_token,
            bounds: this.bounds,
        }
    }
}

impl Parse for MyPredicateType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            lifetimes: input.parse()?,
            bounded_ty: input.parse()?,
            colon_token: input.parse()?,
            bounds: {
                let mut bounds = Punctuated::new();
                loop {
                    if input.is_empty()
                        || input.peek(token::Brace)
                        || input.peek(Token![,])
                        || input.peek(Token![;])
                        || input.peek(Token![:]) && !input.peek(Token![::])
                        || input.peek(Token![=])
                    {
                        break;
                    }
                    let value = input.parse()?;
                    bounds.push_value(value);
                    if !input.peek(Token![+]) {
                        break;
                    }
                    let punct = input.parse()?;
                    bounds.push_punct(punct);
                }
                bounds
            },
        })
    }
}

enum WherePredicate {
    Type(PredicateType),
    Lifetime(PredicateLifetime),
    Const(PredicateConst),
}

impl From<WherePredicate> for syn::WherePredicate {
    fn from(this: WherePredicate) -> Self {
        match this {
            WherePredicate::Type(x) => syn::WherePredicate::Type(x),
            WherePredicate::Lifetime(x) => syn::WherePredicate::Lifetime(x),
            WherePredicate::Const(_) => unreachable!(),
        }
    }
}

impl Parse for WherePredicate {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        println!("{:?}", input.to_string());
        if input.peek(Lifetime) && input.peek2(Token![:]) {
            Ok(WherePredicate::Lifetime(PredicateLifetime {
                lifetime: input.parse()?,
                colon_token: input.parse()?,
                bounds: {
                    let mut bounds = Punctuated::new();
                    loop {
                        if input.is_empty()
                            || input.peek(token::Brace)
                            || input.peek(Token![,])
                            || input.peek(Token![;])
                            || input.peek(Token![:])
                            || input.peek(Token![=])
                        {
                            break;
                        }
                        let value = input.parse()?;
                        bounds.push_value(value);
                        if !input.peek(Token![+]) {
                            break;
                        }
                        let punct = input.parse()?;
                        bounds.push_punct(punct);
                    }
                    bounds
                },
            }))
        } else if input.peek(Paren) {
            let content;
            let _: Paren = parenthesized!(content in input);
            let expr = content.parse()?;
            let _: Token![,] = content.parse()?;
            Ok(WherePredicate::Const(PredicateConst {
                expr,
                message: content.parse()?,
            }))
        } else {
            Ok(WherePredicate::Type(PredicateType {
                lifetimes: input.parse()?,
                bounded_ty: input.parse()?,
                colon_token: input.parse()?,
                bounds: {
                    let mut bounds = Punctuated::new();
                    loop {
                        if input.is_empty()
                            || input.peek(token::Brace)
                            || input.peek(Token![,])
                            || input.peek(Token![;])
                            || input.peek(Token![:]) && !input.peek(Token![::])
                            || input.peek(Token![=])
                        {
                            break;
                        }
                        let value = input.parse()?;
                        bounds.push_value(value);
                        if !input.peek(Token![+]) {
                            break;
                        }
                        let punct = input.parse()?;
                        bounds.push_punct(punct);
                    }
                    bounds
                },
            }))
        }
    }
}

struct WhereClause {
    pub where_token: Where,
    pub predicates: Punctuated<WherePredicate, Comma>,
}

impl From<WhereClause> for syn::WhereClause {
    fn from(this: WhereClause) -> Self {
        Self {
            where_token: this.where_token,
            predicates: this
                .predicates
                .into_iter()
                .map(<WherePredicate as Into<syn::WherePredicate>>::into)
                .collect(),
        }
    }
}

impl Parse for WhereClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(WhereClause {
            where_token: input.parse()?,
            predicates: {
                let mut predicates = Punctuated::new();
                loop {
                    if input.is_empty()
                        || input.peek(token::Brace)
                        || input.peek(Token![,])
                        || input.peek(Token![;])
                        || input.peek(Token![:]) && !input.peek(Token![::])
                        || input.peek(Token![=])
                    {
                        break;
                    }
                    let value = input.parse()?;
                    predicates.push_value(value);
                    if !input.peek(Token![,]) {
                        break;
                    }
                    let punct = input.parse()?;
                    predicates.push_punct(punct);
                }
                predicates
            },
        })
    }
}

#[derive(Default)]
struct Generics {
    pub lt_token: Option<Lt>,
    pub params: Punctuated<GenericParam, Comma>,
    pub gt_token: Option<Gt>,
    pub where_clause: Option<WhereClause>,
}

impl From<Generics> for syn::Generics {
    fn from(this: Generics) -> Self {
        Self {
            lt_token: this.lt_token,
            params: this.params,
            gt_token: this.gt_token,
            where_clause: this.where_clause.map(Into::into),
        }
    }
}

impl Parse for Generics {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if !input.peek(Token![<]) {
            return Ok(Generics::default());
        }

        let lt_token: Token![<] = input.parse()?;

        let mut params = Punctuated::new();
        loop {
            if input.peek(Token![>]) {
                break;
            }

            let attrs = input.call(Attribute::parse_outer)?;
            let lookahead = input.lookahead1();
            if lookahead.peek(Lifetime) {
                params.push_value(GenericParam::Lifetime(LifetimeDef {
                    attrs,
                    ..input.parse()?
                }));
            } else if lookahead.peek(Ident) {
                params.push_value(GenericParam::Type(TypeParam {
                    attrs,
                    ..input.parse()?
                }));
            } else if lookahead.peek(Token![const]) {
                params.push_value(GenericParam::Const(ConstParam {
                    attrs,
                    ..input.parse()?
                }));
            } else if input.peek(Token![_]) {
                params.push_value(GenericParam::Type(TypeParam {
                    attrs,
                    ident: input.call(Ident::parse_any)?,
                    colon_token: None,
                    bounds: Punctuated::new(),
                    eq_token: None,
                    default: None,
                }));
            } else {
                return Err(lookahead.error());
            }

            if input.peek(Token![>]) {
                break;
            }
            let punct = input.parse()?;
            params.push_punct(punct);
        }

        let gt_token: Token![>] = input.parse()?;

        Ok(Generics {
            lt_token: Some(lt_token),
            params,
            gt_token: Some(gt_token),
            where_clause: None,
        })
    }
}

struct Signature {
    pub constness: Option<Const>,
    pub asyncness: Option<Async>,
    pub unsafety: Option<Unsafe>,
    pub abi: Option<Abi>,
    pub fn_token: Token![fn],
    pub ident: Ident,
    pub generics: Generics,
    pub paren_token: Paren,
    pub inputs: Punctuated<FnArg, Comma>,
    pub variadic: Option<Variadic>,
    pub output: ReturnType,
}

impl From<Signature> for syn::Signature {
    fn from(this: Signature) -> Self {
        Self {
            constness: this.constness,
            asyncness: this.asyncness,
            unsafety: this.unsafety,
            abi: this.abi,
            fn_token: this.fn_token,
            ident: this.ident,
            generics: this.generics.into(),
            paren_token: this.paren_token,
            inputs: this.inputs,
            variadic: this.variadic,
            output: this.output,
        }
    }
}

impl Parse for Signature {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let constness: Option<Token![const]> = input.parse()?;
        let asyncness: Option<Token![async]> = input.parse()?;
        let unsafety: Option<Token![unsafe]> = input.parse()?;
        let abi: Option<Abi> = input.parse()?;
        let fn_token: Token![fn] = input.parse()?;
        let ident: Ident = input.parse()?;
        let mut generics: Generics = input.parse()?;

        let content;
        let paren_token = parenthesized!(content in input);
        let mut inputs = parse_fn_args(&content)?;
        let variadic = pop_variadic(&mut inputs);

        let output: ReturnType = input.parse()?;
        generics.where_clause = Some(input.parse()?);

        Ok(Signature {
            constness,
            asyncness,
            unsafety,
            abi,
            fn_token,
            ident,
            generics,
            paren_token,
            inputs,
            variadic,
            output,
        })
    }
}

fn parse_fn_args(input: ParseStream) -> syn::Result<Punctuated<FnArg, Token![,]>> {
    let mut args = Punctuated::new();
    let mut has_receiver = false;

    while !input.is_empty() {
        let attrs = input.call(Attribute::parse_outer)?;

        let arg = if let Some(dots) = input.parse::<Option<Token![...]>>()? {
            FnArg::Typed(PatType {
                attrs,
                pat: Box::new(Pat::Verbatim(variadic_to_tokens(&dots))),
                colon_token: Token![:](dots.spans[0]),
                ty: Box::new(Type::Verbatim(variadic_to_tokens(&dots))),
            })
        } else {
            let mut arg: FnArg = input.parse()?;
            match &mut arg {
                FnArg::Receiver(receiver) if has_receiver => {
                    return Err(Error::new(
                        receiver.self_token.span,
                        "unexpected second method receiver",
                    ));
                }
                FnArg::Receiver(receiver) if !args.is_empty() => {
                    return Err(Error::new(
                        receiver.self_token.span,
                        "unexpected method receiver",
                    ));
                }
                FnArg::Receiver(receiver) => {
                    has_receiver = true;
                    receiver.attrs = attrs;
                }
                FnArg::Typed(arg) => arg.attrs = attrs,
            }
            arg
        };
        args.push_value(arg);

        if input.is_empty() {
            break;
        }

        let comma: Token![,] = input.parse()?;
        args.push_punct(comma);
    }

    Ok(args)
}

fn pop_variadic(args: &mut Punctuated<FnArg, Token![,]>) -> Option<Variadic> {
    let trailing_punct = args.trailing_punct();

    let last = match args.last_mut()? {
        FnArg::Typed(last) => last,
        _ => return None,
    };

    let ty = match last.ty.as_ref() {
        Type::Verbatim(ty) => ty,
        _ => return None,
    };

    let mut variadic = Variadic {
        attrs: Vec::new(),
        dots: parse2(ty.clone()).ok()?,
    };

    if let Pat::Verbatim(pat) = last.pat.as_ref() {
        if pat.to_string() == "..." && !trailing_punct {
            variadic.attrs = mem::replace(&mut last.attrs, Vec::new());
            args.pop();
        }
    }

    Some(variadic)
}

fn variadic_to_tokens(dots: &Token![...]) -> proc_macro2::TokenStream {
    proc_macro2::TokenStream::from_iter(vec![
        TokenTree::Punct({
            let mut dot = Punct::new('.', Spacing::Joint);
            dot.set_span(dots.spans[0]);
            dot
        }),
        TokenTree::Punct({
            let mut dot = Punct::new('.', Spacing::Joint);
            dot.set_span(dots.spans[1]);
            dot
        }),
        TokenTree::Punct({
            let mut dot = Punct::new('.', Spacing::Alone);
            dot.set_span(dots.spans[2]);
            dot
        }),
    ])
}

struct ItemFn {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub sig: Signature,
    pub block: Box<Block>,
}

impl From<ItemFn> for syn::ItemFn {
    fn from(this: ItemFn) -> Self {
        Self {
            attrs: this.attrs,
            vis: this.vis,
            sig: this.sig.into(),
            block: this.block,
        }
    }
}

impl Parse for ItemFn {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            attrs: input.call(Attribute::parse_outer)?,
            vis: input.parse()?,
            sig: input.parse()?,
            block: input.parse()?,
        })
    }
}

#[proc_macro]
pub fn generic_predicates(item: TokenStream) -> TokenStream {
    let mut func = parse_macro_input!(item as ItemFn);

    if let Some(where_clause) = &mut func.sig.generics.where_clause {
        for predicate in where_clause.predicates.iter_mut() {
            let (expr, msg) = match predicate {
                WherePredicate::Const(PredicateConst { expr, message, .. }) => (&*expr, &*message),
                _ => continue,
            };

            let msg = msg
                .as_ref()
                .cloned()
                .unwrap_or(LitStr::new("predicate was false", Span::call_site()));

            let my_pred: MyPredicateType = parse_quote! {
                [(); ::generic_predicates::__private::assertion(#expr, #msg)]:
            };

            *predicate = WherePredicate::Type(my_pred.into());
        }
    }

    let syn_func: syn::ItemFn = func.into();

    quote! {
        #syn_func
    }
    .into()
}
