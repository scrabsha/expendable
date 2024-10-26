// Architectural invariant: this module contains types that are useful for error
// reporting and nothing else.

use crate::token_tree::{Span, TokenTree};

/// An error that is generated when checking an incorrect macro.
///
/// This enum allows crate users to handle and report errors detected by
/// [`check_macro`].
///
/// Everything in this enum in marked as `non_exhaustive`, in order to partially
/// mitigate future variant additions.
///
/// [`check_macro`]: crate::check_macro
#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    /// Generated when one of the matcher or transcriber does not parse.
    ///
    /// The Rust compiler is likely to emit an error anyway. Below is an
    /// example of code that triggers this error:
    ///
    /// ```rust,compile_fail
    /// macro_rules! blatant_error {
    ///     () => =>;
    ///     //    |
    ///     //    error: macro rhs must be delimited.
    /// }
    /// ```
    ///
    /// This prevents us from doing any analyses.
    #[non_exhaustive]
    ParsingFailed {
        /// What was expected.
        what: Vec<MacroRuleNode>,
        /// Where it was expected.
        where_: Span,
    },

    /// Generated when the macro arms don't parse.
    ///
    /// The Rust compiler is likely to emit an error anyway. Below is an
    /// example of code that triggers this error:
    ///
    /// ```rust,compile_fail
    /// macro_rules! blatant_error {
    ///     () => =>;
    ///     //    |
    ///     //    error: macro rhs must be delimited.
    /// }
    /// ```
    ///
    /// This prevents us from doing any analyses.
    #[non_exhaustive]
    SynError {
        /// The exact error that `syn` returned to us.
        error: syn::Error,
    },

    /// An EOF was reached when it was not expected.
    #[non_exhaustive]
    UnexpectedEnd {
        /// The position the parser was at when it reached EOF.
        last_token: Option<Span>,
    },

    /// The macro may expand to invalid AST.
    ///
    /// This variant is very minimal for now. We may add more information to
    /// it in the future. Please open an issue if you have opinions on what
    /// to add!
    #[non_exhaustive]
    InvalidProducedAst {
        /// Where the error happens.
        span: Span,
        /// What tokens are expected here.
        expected: Vec<TokenTree>,
        /// A possible expansion of the macro that exhibits a parsing error.
        ///
        /// The expansion may contain fragments.
        counter_example: Vec<TokenTree>,
    },

    /// A macro expansion refers to a metavariable that is not defined in the
    /// macro match arm.
    ///
    /// If you ever hit this error on a macro that works, then please file an
    /// issue.
    #[non_exhaustive]
    UnboundMetavariable {
        /// The name of the metavariable that was used.
        name: String,
        /// Where it was used.
        where_: Span,
    },

    /// A metavariable is defined at a lower depth than it is used at. At any
    /// given repetition depth, it is only possible to use metavariables
    /// defined at the same or higher depth.
    #[non_exhaustive]
    MetavariableDefinedAtLowerDepth {
        /// The name of the metavariable that was used.
        name: String,
        /// Where it was defined.
        definition_span: Span,
        /// Where it was used.
        usage_span: Span,
        /// The depth at which the metavariable was defined.
        definition_depth: usize,
        /// The depth at which the metavariable was used.
        usage_depth: usize,
    },

    /// A repetition group (nor other repetitions nested into it) doesn't refer
    /// to any metavariable defined at the same depth in the match arm. This
    /// means it's not possible to determine how much that repetition should
    /// be repeated.
    #[non_exhaustive]
    RepetitionWithoutMetavariables {
        /// Where the repetition is defined.
        span: Span,
    },

    /*
    /// A variable is being repeated with a sequence of operator that does not
    /// match the one used when the variable was declared.
    ///
    /// # Example
    ///
    /// ```rust,compile_fail
    /// macro_rules! subtraction {
    ///     ( $( $first:literal $( - $then:literal )* )? ) => {
    ///         $first $( - $then )*
    ///     };
    /// }
    ///
    /// subtraction!(101 - 42);
    /// ```
    ///
    /// In this example, the repetition nesting of the matched metavariable
    /// `then` is `?*`, while the nesting of the metavariable indication
    /// `then` is `*`.
    ///
    /// This variant represents both the case where the amount of repetitions
    /// does not match (which is an error) and the case where the repetition
    /// operators used do not match (which is allowed, but can lead to confusing
    /// errors).
    InvalidRepetitionNesting {
        /// The name of the metavariable.
        metavariable_name: String,
        /// Where the metavariable was declared.
        decl_span: Span,
        /// Where the metavariable was used with an incorrect nesting.
        usage_span: Span,
        /// The nesting used when the metavariable was declared.
        expected_nesting: Vec<RepetitionQuantifierKind>,
        /// The nesting encountered when the metavariable was used.
        got_nesting: Vec<RepetitionQuantifierKind>,
    },
    */
    /// When an invalid repetition separator is being used.
    ///
    /// This corresponds to the following situation:
    ///
    /// ```rust,compile_fail
    /// $( /* tokens */ )()*
    /// ```
    ///
    /// The only valid repetition separators are punctuations, idents and
    /// literals.
    InvalidSeparator {
        /// The tree that is being wrongly repeated.
        tree: TokenTree,
    },
}

impl From<syn::Error> for Error {
    fn from(error: syn::Error) -> Error {
        Error::SynError { error }
    }
}

/// Various nodes that can be expected in a `macro_rules!` invocation.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum MacroRuleNode {
    /// A matcher (everything that comes _before_ the `=>` of a macro rule.
    Matcher,
    /// A transcriber (everything that comes _after_ the `=>` of a macro rule.
    Transcriber,
    /// A repetition.
    Repetition,
    /// A fragment name.
    FragmentName,
    /// A fragment type specifier (`ident`, `expr`, ...).
    FragmentSpecifier,
    /// A meta variable match, such as `$a:ident`.
    MetaVariableMatch,
    /// A repetition quantifier (`?`, `*`, `+`).
    RepetitionQuantifier,
    /// A repetition separator (the `,` in `$( $expr ),*`).
    RepetitionSeparator,
    /// Any terminal.
    Terminal(TokenTree),
}
