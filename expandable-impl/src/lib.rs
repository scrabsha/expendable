#![deny(missing_debug_implementations)]
#![warn(
    missing_docs,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss,
    clippy::cast_lossless,
    clippy::cast_possible_wrap,
    clippy::clear_with_drain,
    clippy::dbg_macro,
    clippy::deref_by_slicing,
    clippy::doc_link_with_quotes,
    clippy::doc_markdown,
    clippy::explicit_deref_methods,
    clippy::get_unwrap,
    clippy::impl_trait_in_params,
    clippy::inefficient_to_string,
    clippy::redundant_else,
    clippy::semicolon_if_nothing_returned,
    clippy::should_panic_without_expect,
    clippy::string_add,
    clippy::string_to_string,
    clippy::used_underscore_binding,
    clippy::wildcard_imports
)]

//! <div class="title-block" style="text-align: center;" align="center">
//! <h1><code>expandable-impl</code></h1>
//! An opinionated, runtime-agnostic <code>macro_rules!</code> expansion
//! checker. </div>
//!
//! <br />
//! <br />
#![doc = include_str!("../doc/00-top-image.md")]
//!
#![doc = include_str!("../doc/01-textbook-example.md")]
//! Luckily for us, this crate provides the [`check_macro`] function, that
//! (drumroll) checks that a macro is valid. It takes as argument the context
//! in which the macro will be called and the content of the macro definition.
//! Let's use it on `js_concat`:
//!
//! ```
//! use expandable_impl::{InvocationContext, quote};
//!
//! let err = expandable_impl::check_macro(InvocationContext::Item, quote! {
//!     (@left:expr, @right:expr) => {
//!        @left ++ @right
//!     };
//! })
//! .unwrap_err();
//!
//! assert!(matches!(
//!     err,
//!     expandable_impl::Error::InvalidProducedAst { .. }
//! ));
//! ```
//!
//! ## Expansion context
//!
//! Macros can expand to different things depending on where they are called.
//! As a result, `expandable-impl` must know what the macro expands to. This is
//! represented by the [`InvocationContext`] enum.
//!
//! ## Runtime-agnostic?
//!
//! This crate does not depend on any "compiler-specific" data structure. It
//! may be embedded anywhere. [`expandable`] is a crate that provides the
//! features defined in this crate as a set of `proc_macro`. Feel free to
//! embed this crate in your analysis tool!
//!
//! [`expandable`]: https://crates.io/crates/expandable
#![doc = include_str!("../doc/02-what-can-it-detect.md")]
//!
#![doc = include_str!("../doc/03-opinionated.md")]
//!
#![doc = include_str!("../doc/98-msrv.md")]
//!
#![doc = include_str!("../doc/99-license.md")]

use std::{marker::Copy, str::FromStr};

pub use error::{Error, MacroRuleNode};
pub use proc_macro2::TokenStream;

mod error;
mod expand;
pub mod token_tree;

/// The whole point.
///
/// This functions takes all the tokens that have been passed to the macro
/// invocation and performs all the checks that have been implemented in this
/// crate.
#[expect(unused_variables)]
pub fn check_macro(ctx: InvocationContext, input: TokenStream) -> Result<(), Error> {
    todo!()
}

/// The contexts in which a macro can be called.
///
/// All macros can't be called in all contexts. For instance, a macro that
/// expands to a pattern may not be called where an expression is expected.
/// This type allows the [`check_macro`] function to know the context the macro
/// will be invoked in.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InvocationContext {
    /// The macro expands to an expression.
    Expr,
    /// The macro expands to any number of item.
    Item,
    /// The macro expands to a pattern.
    Pat,
    /// The macro expands to any number of statement.
    Stmt,
    /// The macro expands to a type.
    Ty,
}

/// A specific kind of fragment.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum FragmentKind {
    /// A block (`block`).
    Block,
    /// An expression (`expr`).
    Expr,
    /// An identifier or a raw identifier (`ident`).
    Ident,
    /// An item (`item`).
    Item,
    /// A lifetime (`lifetime`).
    Lifetime,
    /// An attribute content (`meta`)
    Meta,
    /// A pattern (including alternative) (`pat`).
    Pat,
    /// A path (`path`).
    Path,
    /// A pattern (excluding alternative) (`pat_param`).
    PatParam,
    /// A statement (`stmt`).
    Stmt,
    /// A token tree (`tt`).
    Tt,
    /// A type (`ty`).
    Ty,
    /// A visibility (`vis`).
    Vis,
}

impl FromStr for FragmentKind {
    type Err = ();

    fn from_str(s: &str) -> Result<FragmentKind, ()> {
        Ok(match s {
            "block" => FragmentKind::Block,
            "expr" => FragmentKind::Expr,
            "ident" => FragmentKind::Ident,
            "item" => FragmentKind::Item,
            "lifetime" => FragmentKind::Lifetime,
            "meta" => FragmentKind::Meta,
            "pat" => FragmentKind::Pat,
            "path" => FragmentKind::Path,
            "pat_param" => FragmentKind::PatParam,
            "stmt" => FragmentKind::Stmt,
            "tt" => FragmentKind::Tt,
            "ty" => FragmentKind::Ty,
            "vis" => FragmentKind::Vis,

            _ => return Err(()),
        })
    }
}

impl FragmentKind {
    fn to_str(self) -> &'static str {
        match self {
            FragmentKind::Block => "block",
            FragmentKind::Expr => "expr",
            FragmentKind::Ident => "ident",
            FragmentKind::Item => "item",
            FragmentKind::Lifetime => "lifetime",
            FragmentKind::Meta => "meta",
            FragmentKind::Pat => "pat",
            FragmentKind::Path => "path",
            FragmentKind::PatParam => "pat_param",
            FragmentKind::Stmt => "stmt",
            FragmentKind::Tt => "tt",
            FragmentKind::Ty => "ty",
            FragmentKind::Vis => "vis",
        }
    }
}
