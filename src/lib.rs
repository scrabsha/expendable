//! <div class="title-block" style="text-align: center;" align="center">
//! <h1><code>expandable</code></h1>
//! An opinionated attribute-macro based <code>macro_rules!</code> expansion
//! checker.
//! </div>
//!
//! <br />
//! <br />
#![doc = include_str!("../doc/00-top-image.md")]
//!
#![doc = include_str!("../doc/01-textbook-example.md")]
//! Luckily for us, this crate provides the [`expandable::expr`] macro, that
//! checks that the macro expands to a valid expression. Let's use it on
//! `js_concat`:
//!
//! ```rust,compile_fail
//! #[expandable::expr]
//! macro_rules! my_vec {
//!   ($($t:expr),*) => {{
//!       let mut buffer = Vec::new();
//!
//!       $(
//!           buffer->push($t);
//!       )*
//!
//!       buffer
//!   }};
//! }
//! ```
//!
//! This emits the following error [^error-message]:
//! ```none
#![doc = include_str!("../tests/ui/fail/my_vec.stderr_nightly")]
//! ```
//! 
//! [^error-message]: The Rust grammar is not fully implemented at the moment,
//!     leading to incomplete "expected xxx" list this will be fixed before the
//!     first non-alpha release of this crate.
//!
//! ## Expansion context
//!
//! Macros can expand to different things depending on where they are called.
//! As a result, `expandable` must know what the macro expands to. To do so,
//! multiple macros are available:
//! - Macros that expand to expressions are checked by [`expandable::expr`],
//! - Macros that expand to items are checked by [`expandable::item`],
//! - Macros that expand to patterns are checked by [`expandable::pat`],
//! - Macros that expand to statements are checked by [`expandable::stmt`],
//! - Macros that expand to types are checked by [`expandable::ty`],
//!
//! [`expandable::expr`]: macro@expr
//! [`expandable::item`]: macro@item
//! [`expandable::pat`]: macro@pat
//! [`expandable::stmt`]: macro@stmt
//! [`expandable::ty`]: macro@ty
#![doc = include_str!("../doc/02-what-can-it-detect.md")]
//!
#![doc = include_str!("../doc/03-opinionated.md")]
//!
#![doc = include_str!("../doc/98-msrv.md")]
//!
#![doc = include_str!("../doc/99-license.md")]

extern crate proc_macro;

use std::collections::BTreeSet;

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Span, TokenTree};
use syn_shim::ItemMacroRules;

mod syn_shim;

macro_rules! attribute_macro {
    ($name:ident => $variant:ident) => {
        #[doc = concat!("Checks that a macro expands to a valid ", stringify!($name), ".")]
        ///
        /// *Refer to the [crate-level documentation][crate] for more.*
        #[proc_macro_attribute]
        pub fn $name(_: TokenStream1, item: TokenStream1) -> TokenStream1 {
            expandable_inner(expandable_impl::InvocationContext::$variant, item)
        }
    };
}

attribute_macro!(expr => Expr);
attribute_macro!(item => Item);
attribute_macro!(pat => Pat);
attribute_macro!(stmt => Stmt);
attribute_macro!(ty => Ty);

fn expandable_inner(ctx: expandable_impl::InvocationContext, item: TokenStream1) -> TokenStream1 {
    let mut item_ = item.clone();

    let macro_ = match syn::parse2::<ItemMacroRules>(item.into()) {
        Ok(macro_) => macro_,
        Err(e) => return e.to_compile_error().into(),
    };

    if let Err(e) = expandable_impl::check_macro(ctx, macro_.tokens) {
        item_.extend(TokenStream1::from(mk_error_msg(e).into_compile_error()));
        return item_;
    }

    item_
}

fn mk_error_msg(error: expandable_impl::Error) -> syn::Error {
    let (message, span) = match error {
        expandable_impl::Error::ParsingFailed { where_, .. } => (
            "Failed to parse `macro_rules` body".to_string(),
            Some(where_),
        ),

        expandable_impl::Error::UnexpectedEnd { last_token, .. } => {
            ("Unexpected end of macro invocation".to_string(), last_token)
        }

        expandable_impl::Error::InvalidProducedAst { span, expected, .. } => {
            let expected = rassemble_expected_descrs(expected);
            (
                format!("Potentially invalid expansion. Expected {expected}."),
                Some(span),
            )
        }

        expandable_impl::Error::UnboundMetavariable { name, where_, .. } => {
            (format!("Unbound metavariable `{name}`"), Some(where_))
        }

        // expandable_impl::Error::InvalidRepetitionNesting {
        //     metavariable_name,
        //     usage_span,
        //     expected_nesting,
        //     got_nesting,
        //     ..
        // } => {
        //     let expected_nesting = pp_repetition_ops(&expected_nesting);
        //     let got_nesting = pp_repetition_ops(&got_nesting);

        //     (
        //         format!(
        //             "the repetition used for `{metavariable_name}` ({got_nesting}) is different \
        //              from how it is matched ({expected_nesting})."
        //         ),
        //         Some(usage_span),
        //     )
        // }
        _ => (
            "`expandable` returned an error the expandable macro does not handle (yet)".to_string(),
            None,
        ),
    };

    let span = span.unwrap_or_else(Span::call_site);
    syn::Error::new(span, message)
}

const MAX_EXPECTED_NUM: usize = 6;

fn rassemble_expected_descrs(expected: Vec<TokenTree>) -> String {
    // This is a slow path - we are allowed to allocate as much as required.
    // `BtreeSet`s have set properties (no duplicate), and they give us a cool
    // ordering.

    #[expect(unused_mut)]
    let (mut possible_fragments, mut rest) = (BTreeSet::<String>::new(), BTreeSet::new());

    #[expect(unused)]
    for expected in expected {
        todo!();
    }

    let mut expected = possible_fragments.into_iter().collect::<Vec<_>>();
    expected.extend(rest);

    let expected_len = expected.len();

    let (expected, or_n_others) = if expected_len > MAX_EXPECTED_NUM {
        let to_print = expected[0..MAX_EXPECTED_NUM].join(", ");
        let n = expected_len - MAX_EXPECTED_NUM;
        let n_others = format!(" or {n} others");

        (to_print, n_others)
    } else {
        let to_print = expected.join(", ");
        let n_others = String::new();

        (to_print, n_others)
    };

    format!("{expected}{or_n_others}")
}

#[cfg(test)]
#[test]
fn ui() {
    setup_channel_dependant_stderrs();

    let t = trybuild::TestCases::new();
    t.pass("tests/ui/pass/*.rs");
    t.compile_fail("tests/ui/fail/*.rs");

    drop(t);

    cleanup_channel_dependant_stderrs();
}

#[cfg(test)]
const CHANNEL_DEPENDANT_STDERRS: [&str; 2] = ["bad_range_pattern", "my_vec"];

#[cfg(test)]
fn setup_channel_dependant_stderrs() {
    // The error messages of `expandable` depend on the channel that is used
    // when compiling the crate, as we use the nightly-only `Span::join` method.
    //
    // We circumvent this by checking against a different stderr depending on
    // the channel that is used when the macro is compiled.

    use std::fs;

    for (original, link) in channel_dependant_stderrs() {
        let _ = fs::remove_file(&link);
        fs::hard_link(original, link).unwrap();
    }
}

#[cfg(test)]
fn cleanup_channel_dependant_stderrs() {
    use std::fs;

    for (_, link) in channel_dependant_stderrs() {
        fs::remove_file(link).unwrap();
    }
}

#[cfg(test)]
fn channel_dependant_stderrs() -> Vec<(String, String)> {
    use rustc_version::Channel;

    CHANNEL_DEPENDANT_STDERRS
        .into_iter()
        .map(|unstable_stderr| {
            let channel = rustc_version::version_meta().unwrap().channel;
            let suffix = match channel {
                Channel::Nightly | Channel::Dev => "nightly",
                Channel::Beta | Channel::Stable => "stable",
            };

            let in_path = format!("tests/ui/fail/{unstable_stderr}.stderr_{suffix}");
            let out_path = format!("tests/ui/fail/{unstable_stderr}.stderr");

            (in_path, out_path)
        })
        .collect()
}
