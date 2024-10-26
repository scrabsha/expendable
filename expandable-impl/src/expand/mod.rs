mod repetition_groups;

use proc_macro2::TokenStream as GenericTokenStream;
use syn::{Expr, Item, Stmt, Type, parse::Parse};

use crate::{
    Error, InvocationContext,
    expand::repetition_groups::RepetitionGroups,
    token_tree::{ParseMode, TokenTree},
};

#[expect(dead_code)]
pub(crate) fn check_macro_arm(
    matcher: Vec<TokenTree>,
    transcriber: Vec<TokenTree>,
    ctx: InvocationContext,
) -> Result<(), Error> {
    let relevant_expansions = expand(matcher, transcriber);

    for expansion in relevant_expansions {
        check_expansion(expansion, ctx)?;
    }

    Ok(())
}

fn expand(matcher: Vec<TokenTree>, transcriber: Vec<TokenTree>) -> Vec<Vec<TokenTree>> {
    let _ = RepetitionGroups::new(&matcher, &transcriber);
    todo!();
}

fn check_expansion(expansion: Vec<TokenTree>, ctx: InvocationContext) -> Result<(), Error> {
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
    // TODO: it's actually 0 or more items.
    syn::parse2::<Item>(expansion).map(drop)
}

fn check_pat(expansion: GenericTokenStream) -> syn::Result<()> {
    syn::parse2::<Pat>(expansion).map(drop)
}

fn check_stmt(expansion: GenericTokenStream) -> syn::Result<()> {
    // TODO: it's actually 0 or more statements.
    syn::parse2::<Stmt>(expansion).map(drop)
}

fn check_ty(expansion: GenericTokenStream) -> syn::Result<()> {
    syn::parse2::<Type>(expansion).map(drop)
}

struct Pat(());

impl Parse for Pat {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        syn::Pat::parse_multi_with_leading_vert(input)
            .map(drop)
            .map(Pat)
    }
}
