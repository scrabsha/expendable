mod repetition_groups;

use crate::{Error, expand::repetition_groups::RepetitionGroups, token_tree::TokenTree};

#[expect(dead_code)]
pub(crate) fn check_macro_arm(
    matcher: Vec<TokenTree>,
    transcriber: Vec<TokenTree>,
) -> Result<(), Error> {
    let _relevant_expansions = expand(matcher, transcriber);

    Ok(())
}

fn expand(matcher: Vec<TokenTree>, transcriber: Vec<TokenTree>) -> Vec<Vec<TokenTree>> {
    let _ = RepetitionGroups::new(&matcher, &transcriber);
    todo!();
}
