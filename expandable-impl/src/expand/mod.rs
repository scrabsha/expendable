mod repetition_groups;

use crate::token_tree::TokenTree;
use crate::expand::repetition_groups::RepetitionGroups;

#[expect(dead_code)]
pub(crate) fn expand(matcher: Vec<TokenTree>, transcriber: Vec<TokenTree>) -> Vec<Vec<TokenTree>> {
    let _ = RepetitionGroups::new(&matcher, &transcriber);
    todo!();
}
