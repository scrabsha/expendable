use std::marker::PhantomData;

use proc_macro2::TokenStream as GenericTokenStream;
use syn::parse::{Parse, ParseStream};

use crate::{
    Error, InvocationContext,
    token_tree::{ParseMode, TokenTree},
};

pub(crate) fn check_expansion(
    expansion: Vec<TokenTree>,
    ctx: InvocationContext,
) -> Result<(), Error> {
    let check_fn = match ctx {
        InvocationContext::Expr => check_expr,
        InvocationContext::Item => check_item,
        InvocationContext::Pat => check_pat,
        InvocationContext::Stmt => check_stmt,
        InvocationContext::Ty => check_ty,
    };

    let stream = TokenTree::into_generic(expansion, ParseMode::Transcriber);

    check_fn(stream).map_err(|_| todo!())
}

fn check_expr(expansion: GenericTokenStream) -> syn::Result<()> {
    syn::parse2::<Expr>(expansion).map(drop)
}

fn check_item(expansion: GenericTokenStream) -> syn::Result<()> {
    syn::parse2::<Item>(expansion).map(drop)
}

fn check_pat(expansion: GenericTokenStream) -> syn::Result<()> {
    syn::parse2::<Pat>(expansion).map(drop)
}

fn check_stmt(expansion: GenericTokenStream) -> syn::Result<()> {
    syn::parse2::<Stmt>(expansion).map(drop)
}

fn check_ty(expansion: GenericTokenStream) -> syn::Result<()> {
    syn::parse2::<Type>(expansion).map(drop)
}

struct Expr;

impl Parse for Expr {
    fn parse(input: ParseStream) -> syn::Result<Expr> {
        syn::Expr::parse(input).map(|_| Expr)
    }
}

struct Item;

impl Parse for Item {
    fn parse(input: ParseStream) -> syn::Result<Item> {
        Multiple::<syn::Item>::parse(input).map(|_| Item)
    }
}

struct Pat;

impl Parse for Pat {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        syn::Pat::parse_multi_with_leading_vert(input).map(|_| Pat)
    }
}

struct Stmt;

impl Parse for Stmt {
    fn parse(input: ParseStream) -> syn::Result<Stmt> {
        Multiple::<syn::Stmt>::parse(input).map(|_| Stmt)
    }
}

struct Type;

impl Parse for Type {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        syn::Type::parse(input).map(|_| Type)
    }
}

struct Multiple<T>(PhantomData<T>);

impl<T> Parse for Multiple<T>
where
    T: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        while !input.is_empty() {
            T::parse(input)?;
        }

        Ok(Multiple(PhantomData))
    }
}
