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
