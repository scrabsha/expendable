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
use grammar::DynamicState;
pub use grammar::TokenDescription;

#[macro_use]
mod macros;
mod error;
mod expand;
mod expansion;
mod grammar;
mod list;
mod matcher;
mod repetition_stack;
pub mod token_tree;

#[doc(hidden)]
pub mod span;
mod substitution;

/// The whole point.
///
/// This functions takes all the tokens that have been passed to the macro
/// invocation and performs all the checks that have been implemented in this
/// crate.
pub fn check_macro<Span>(
    ctx: InvocationContext,
    input: Vec<TokenTree<Span>>,
) -> Result<(), Error<Span>>
where
    Span: Copy + 'static,
{
    let mut iter = input.into_iter();

    while let Some(head) = iter.next() {
        let TokenTreeKind::Parenthesed(matcher) = head.kind else {
            return Err(Error::ParsingFailed {
                what: vec![MacroRuleNode::Matcher],
                where_: head.span,
            });
        };

        let matcher = matcher::TokenTree::from_generic(matcher)?;
        let matcher = matcher::Matcher::from_generic(&matcher)?;

        let Some(token) = iter.next() else {
            return Err(Error::UnexpectedEnd {
                last_token: Some(head.span),
            });
        };

        let TokenTreeKind::Terminal(Terminal::FatArrow) = token.kind else {
            return Err(Error::ParsingFailed {
                what: vec![MacroRuleNode::Terminal(Terminal::FatArrow)],
                where_: token.span,
            });
        };

        let Some(token) = iter.next() else {
            return Err(Error::UnexpectedEnd {
                last_token: Some(token.span),
            });
        };

        let TokenTreeKind::CurlyBraced(substitution) = token.kind else {
            return Err(Error::ParsingFailed {
                what: vec![MacroRuleNode::Transcriber],
                where_: token.span,
            });
        };

        let substitution = substitution::TokenTree::from_generic(substitution)?;
        repetition_stack::check(&matcher, &substitution)?;

        expansion::check_arm(ctx.to_state(), matcher, &substitution, token.span)?;

        if let Some(semi) = iter.next() {
            let TokenTreeKind::Terminal(Terminal::Semicolon) = semi.kind else {
                return Err(Error::ParsingFailed {
                    what: vec![MacroRuleNode::Terminal(Terminal::Semicolon)],
                    where_: semi.span,
                });
            };
        }
    }

    Ok(())
}

pub(crate) trait Spannable<Span> {
    type Output;

    fn with_span(self, span: Span) -> Self::Output;
}

/// An untyped tree of tokens.
///
/// This type allows the end-user to represent the tokens that is passed in the
/// macro invocation. It is not _exactly_ the same as [`proc_macro::TokenTree`],
/// as the tokens are grouped differently [^1]. Writing a
/// "[`proc_macro::TokenTree`] to [`TokenTree`]" should not be too hard, but is
/// not the scope of this crate.
///
/// [^1]: For instance, `+=` is represented as a single token in declarative
/// macros but as "`+` followed by `=`" in procedural macros
/// ([ref][declarative-macro-tokens-and-procedural-macro-tokens]).
///
/// [`proc_macro::TokenTree`]:
///     https://doc.rust-lang.org/proc_macro/enum.TokenTree.html
/// [declarative-macro-tokens-and-procedural-macro-tokens]:
///     https://doc.rust-lang.org/reference/procedural-macros.html#declarative-macro-tokens-and-procedural-macro-tokens
#[derive(Clone, Debug, PartialEq)]
pub struct TokenTree<Span> {
    /// What kind of token tree is this?
    pub kind: TokenTreeKind<Span>,
    /// Its position in the input code (useful for error message generation).
    pub span: Span,
}

#[doc(hidden)]
#[allow(non_snake_case, unused)]
impl<Span> TokenTree<Span> {
    pub fn terminal(span: Span, t: Terminal) -> TokenTree<Span> {
        TokenTree {
            kind: TokenTreeKind::Terminal(t),
            span,
        }
    }

    pub fn parenthesed(span: Span, i: Vec<TokenTree<Span>>) -> TokenTree<Span> {
        TokenTree {
            kind: TokenTreeKind::Parenthesed(i),
            span,
        }
    }

    pub fn curly_braced(span: Span, i: Vec<TokenTree<Span>>) -> TokenTree<Span> {
        TokenTree {
            kind: TokenTreeKind::CurlyBraced(i),
            span,
        }
    }

    pub fn bracketed(span: Span, i: Vec<TokenTree<Span>>) -> TokenTree<Span> {
        TokenTree {
            kind: TokenTreeKind::Bracketed(i),
            span,
        }
    }
}

/// Represents the different types of token tree.
#[derive(Clone, Debug, PartialEq)]
pub enum TokenTreeKind<Span> {
    /// A terminal (ie: a leaf tree node).
    Terminal(Terminal),
    /// A sequence of [`TokenTree`] that is delimited by parenthesis.
    Parenthesed(Vec<TokenTree<Span>>),
    /// A sequence of [`TokenTree`] that is delimited by curly brackets.
    CurlyBraced(Vec<TokenTree<Span>>),
    /// A sequence of [`TokenTree`] that is delimited by brackets.
    Bracketed(Vec<TokenTree<Span>>),
}

impl_spannable!(TokenTreeKind<Span> => TokenTree);

/// A terminal symbol.
///
/// # Multi-character operators
///
/// Multi-character operators (`+=`, `->`, ...) must _not_ be split in multiple
/// [`Terminal`]. Any use of the [`check_macro`] function that does not respect
/// this invariant will is subject to unexpected results.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Terminal {
    /// An identifier (`foo`, `bar`).
    Ident(String),

    // Keywords
    /// The `as` keyword.
    As,
    /// The `async` keyword.
    Async,
    /// The `await` keyword.
    Await,
    /// The `break` keyword.
    Break,
    /// The `const` keyword.
    Const,
    /// The `continue` keyword.
    Continue,
    /// The `crate` keyword.
    Crate,
    /// The `dyn` keyword.
    Dyn,
    /// The `else` keyword.
    Else,
    /// The `enum` keyword.
    Enum,
    /// The `extern` keyword.
    Extern,
    /// The `false` keyword.
    False,
    /// The `fn` keyword.
    Fn,
    /// The `for` keyword.
    For,
    /// The `if` keyword.
    If,
    /// The `impl` keyword.
    Impl,
    /// The `in` keyword.
    In,
    /// The `let` keyword.
    Let,
    /// The `loop` keyword.
    Loop,
    /// The `match` keyword.
    Match,
    /// The `mod` keyword.
    Mod,
    /// The `move` keyword.
    Move,
    /// The `mut` keyword.
    Mut,
    /// The `pub` keyword.
    Pub,
    /// The `ref` keyword.
    Ref,
    /// The `return` keyword.
    Return,
    /// The `self` keyword.
    Self_,
    /// The `Self` keyword.
    SelfUpper,
    /// The `static` keyword.
    Static,
    /// The `struct` keyword.
    Struct,
    /// The `super` keyword.
    Super,
    /// The `trait` keyword.
    Trait,
    /// The `true` keyword.
    True,
    /// The `type` keyword.
    Type,
    /// The `union` keyword.
    Union,
    /// The `unsafe` keyword.
    Unsafe,
    /// The `use` keyword.
    Use,
    /// The `where` keyword.
    Where,
    /// The `while` keyword.
    While,
    /// The `abstract` keyword.
    Abstract,
    /// The `become` keyword.
    Become,
    /// The `box` keyword.
    Box,
    /// The `do` keyword.
    Do,
    /// The `final` keyword.
    Final,
    /// The `macro` keyword.
    Macro,
    /// The `override` keyword.
    Override,
    /// The `priv` keyword.
    Priv,
    /// The `try` keyword.
    Try,
    /// The `typeof` keyword.
    Typeof,
    /// The `unsized` keyword.
    Unsized,
    /// The `virtual` keyword.
    Virtual,
    /// The `yield` keyword.
    Yield,

    // Literals
    /// A literal (`42`, `"foo"`).
    ///
    /// We use textual representation of literals because we don't want to deal
    /// with the parsing of literals.
    // TODO: it may be appropriate to actually parse these literals :thinking:.
    Literal(String),

    // Punctuates
    /// A plus (`+`).
    Plus,
    /// A minus (`-`).
    Minus,
    /// A star (`*`).
    Star,
    /// A slash (`/`).
    Slash,
    /// A percent (`%`).
    Percent,
    /// A caret (`^`).
    Caret,
    /// A not (`!`).
    Not,
    /// An and (`&`).
    And,
    /// An or (`|`).
    Or,
    /// Lazy boolean and (`&&`).
    AndAnd,
    /// Lazy boolean or (`||`).
    OrOr,
    /// A shift left (`<<`).
    Shl,
    /// A shift right (`>>`).
    Shr,
    /// A plus-equals (`+=`).
    PlusEquals,
    /// A minus-equals (`-=`).
    MinusEquals,
    /// A star-equals (`*=`).
    StarEquals,
    /// A slash-equals (`/=`).
    SlashEquals,
    /// A percent-equals (`%=`).
    PercentEquals,
    /// A caret-equal (`^=`).
    CaretEquals,
    /// An and-equals (`&=`).
    AndEquals,
    /// An or-equals (`|=`).
    OrEquals,
    /// A shift-left-equals (`<<=`).
    ShlEquals,
    /// A shift-right-equals (`>>=`).
    ShrEquals,
    /// An equal (`=`).
    Equals,
    /// An equals equals (`==`).
    EqualsEquals,
    /// A not-equal (`!=`).
    NotEquals,
    /// A greater than (`>`).
    GreaterThan,
    /// A less than (`<`).
    LessThan,
    /// A greater than equals (`>=`).
    GreaterThanEquals,
    /// A less than equals (`<=`).
    LessThanEquals,
    /// An at (`@`).
    At,
    /// An underscore (`_`).
    Underscore,
    /// A dot (`.`).
    Dot,
    /// A dot dot (`..`).
    DotDot,
    /// A dot dot dot (`...`).
    DotDotDot,
    /// A dot dot equals (`..=`).
    DotDotEquals,
    /// A comma (`,`).
    Comma,
    /// A semicolon (`;`).
    Semicolon,
    /// A colon (`:`).
    Colon,
    /// A colon colon (`::`).
    ColonColon,
    /// A right arrow (`->`).
    RightArrow,
    /// A fat arrow (`=>`).
    FatArrow,
    /// A pound (`#`).
    Pound,
    /// A dollar sign (`$`).
    Dollar,
    /// A question mark (`?`).
    QuestionMark,
}

impl_spannable!(Terminal => TokenTree);

impl<Span> From<Terminal> for TokenTreeKind<Span> {
    fn from(value: Terminal) -> TokenTreeKind<Span> {
        TokenTreeKind::Terminal(value)
    }
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

impl InvocationContext {
    fn to_state<T>(self) -> DynamicState<T>
    where
        T: Copy,
    {
        match self {
            InvocationContext::Expr => DynamicState::expr(),
            InvocationContext::Item => DynamicState::item(),
            InvocationContext::Pat => DynamicState::pat(),
            InvocationContext::Stmt => DynamicState::stmt(),
            InvocationContext::Ty => DynamicState::ty(),
        }
    }
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

impl FromStr for InvocationContext {
    type Err = ();

    fn from_str(s: &str) -> Result<InvocationContext, ()> {
        Ok(match s {
            "item" => InvocationContext::Item,
            "expr" => InvocationContext::Expr,

            _ => return Err(()),
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct RepetitionQuantifier<Span> {
    kind: RepetitionQuantifierKind,
    span: Span,
}

#[cfg(test)]
#[allow(non_snake_case, unused)]
impl RepetitionQuantifier<()> {
    fn ZeroOrOne() -> RepetitionQuantifier<()> {
        RepetitionQuantifier {
            kind: RepetitionQuantifierKind::ZeroOrOne,
            span: (),
        }
    }

    fn ZeroOrMore() -> RepetitionQuantifier<()> {
        RepetitionQuantifier {
            kind: RepetitionQuantifierKind::ZeroOrMore,
            span: (),
        }
    }

    fn OneOrMore() -> RepetitionQuantifier<()> {
        RepetitionQuantifier {
            kind: RepetitionQuantifierKind::OneOrMore,
            span: (),
        }
    }
}

/// Denotes how much times a repetition shall be repeated.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum RepetitionQuantifierKind {
    /// Zero or one repetitions (`?` operator).
    ZeroOrOne,
    /// Zero or more repetitions (`*` operator).
    ZeroOrMore,
    /// One or more repetitions (`+` operator).
    OneOrMore,
}

impl_spannable!(RepetitionQuantifierKind => RepetitionQuantifier);

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! check_macro_test {
        (
            $( #[ $meta:meta ] )*
            $test_name:ident {
                #[$kind:ident]
                {
                    $( $tt:tt )*
                }
            }
        ) => {
            $( #[ $meta ] )*
            #[test]
            fn $test_name() {
                let tokens = quote! { $( $tt )* };
                let ctxt = stringify!($kind).parse::<InvocationContext>().expect("Failed to parse `InvocationContext`");

                check_macro(ctxt, tokens).expect("Macro check returned error");
            }
        };
    }

    check_macro_test! {
        single_arm {
            #[expr]
            {
                () => { a }
            }
        }
    }

    check_macro_test! {
        accepts_final_semi {
            #[expr]
            {
                () => { a };
            }
        }
    }

    check_macro_test! {
        multiple_arms {
            #[expr]
            {
                () => { a };
                (()) => { b };
            }
        }
    }

    check_macro_test! {
        #[should_panic = "Macro check returned error: InvalidProducedAst { span: 2, expected: [Return, Break, Ident, Self_, SelfUpper, Super, Crate, Fragment(Ident), ColonColon, LessThan, Literal, Fragment(Expr), If, LParen, LBracket, LBrace, Loop, While, For, Match], counter_example: [] }"]
        empty_expr_is_not_an_expr {
            #[expr]
            {
                () => {}
            }
        }
    }

    check_macro_test! {
        just_a_simple_if {
            #[expr]
            {
                () => { if a { a } }
            }
        }
    }

    check_macro_test! {
        if_with_else {
            #[expr]
            {
                () => { if a { a } else { a } }
            }
        }
    }

    check_macro_test! {
        fn_call_1 {
            #[expr]
            {
                () => { a(b) };
            }
        }
    }

    check_macro_test! {
        fn_call_2 {
            #[expr]
            {
                () => { a(b, c) };
            }
        }
    }

    check_macro_test! {
        fn_call_3 {
            #[expr]
            {
                () => { a(b, c, d) };
            }
        }
    }

    check_macro_test! {
        fn_call_4 {
            #[expr]
            {
                () => {
                    a(
                        b,
                        c,
                        d,
                    )
                };
            }
        }
    }

    check_macro_test! {
        fn_call_5 {
            #[expr]
            {
                () => {
                    a(
                        b + c,
                        if d { e },
                        if f { g } else { h }
                    )
                };
            }
        }
    }

    check_macro_test! {
        fn_call_6 {
            #[expr]
            {
                () => {
                    a(
                        b + c,
                        if d { e },
                        if f { g } else { h },
                    )
                };
            }
        }
    }

    check_macro_test! {
        array_0 {
            #[expr]
            {
                () => { [] };
            }
        }
    }

    check_macro_test! {
        array_1 {
            #[expr]
            {
                () => { [a] };
            }
        }
    }

    check_macro_test! {
        array_2 {
            #[expr]
            {
                () => { [a, b] };
            }
        }
    }

    check_macro_test! {
        array_2_ {
            #[expr]
            {
                () => { [a, b,] };
            }
        }
    }

    check_macro_test! {
        array_with_fragments {
            #[expr]
            {
                (#a:expr, #b:expr, #c:ident, #d:ident) => { [#a, #b, #c, #d] };
            }
        }
    }

    check_macro_test! {
        array_repeat {
            #[expr]
            {
                ( #( #a:expr )* ) => {
                    [ #( #a, )* ]
                };
            }
        }
    }

    check_macro_test! {
        path_repeat {
            #[expr]
            {
                () => {
                    #( :: a )+
                }
            }
        }
    }

    check_macro_test! {
        path_repeat_from_fragment {
            #[expr]
            {
                ( #( #id:ident )+ ) => {
                    #( :: #id )+
                }
            }
        }
    }
}
