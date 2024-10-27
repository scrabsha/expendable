use crate::{Error, token_tree::TokenTree};

pub(super) fn validate(_matcher: &[TokenTree], _transcriber: &[TokenTree]) -> Result<(), Error> {
    Ok(())
}
