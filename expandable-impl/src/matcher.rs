// Architecture invariant: this module contains constructs that allow to
// extract information out of macro matchers.

use std::{
    collections::HashMap,
    fmt::{self, Debug},
};

use crate::{
    FragmentKind, RepetitionQuantifier, RepetitionQuantifierKind, Spannable, Terminal,
    TokenTree as GenericTokenTree, TokenTreeKind as GenericTokenTreeKind,
    error::{Error, MacroRuleNode},
    list::LameLinkedList,
};

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct TokenTree<Span> {
    kind: TokenTreeKind<Span>,
    span: Span,
}

#[cfg(test)]
#[allow(non_snake_case, unused)]
impl TokenTree<()> {
    fn Terminal(t: Terminal) -> TokenTree<()> {
        TokenTreeKind::Terminal(t).with_span(())
    }

    fn Parenthesed(i: Vec<TokenTree<()>>) -> TokenTree<()> {
        TokenTreeKind::Parenthesed(i).with_span(())
    }

    fn CurlyBraced(i: Vec<TokenTree<()>>) -> TokenTree<()> {
        TokenTreeKind::CurlyBraced(i).with_span(())
    }

    fn Binding(name: String, kind: FragmentKind) -> TokenTree<()> {
        TokenTreeKind::Binding { name, kind }.with_span(())
    }

    fn Repetition(
        inner: Vec<TokenTree<()>>,
        separator: Option<TokenTree<()>>,
        quantifier: RepetitionQuantifier<()>,
    ) -> TokenTree<()> {
        let separator = separator.map(Box::new);
        TokenTreeKind::Repetition {
            inner,
            separator,
            quantifier,
        }
        .with_span(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum TokenTreeKind<Span> {
    Terminal(Terminal),
    Parenthesed(Vec<TokenTree<Span>>),
    CurlyBraced(Vec<TokenTree<Span>>),
    Bracketed(Vec<TokenTree<Span>>),
    Binding {
        name: String,
        kind: FragmentKind,
    },
    Repetition {
        inner: Vec<TokenTree<Span>>,
        separator: Option<Box<TokenTree<Span>>>,
        quantifier: RepetitionQuantifier<Span>,
    },
}

impl_spannable!(TokenTreeKind<Span> => TokenTree);

impl<Span> TokenTree<Span>
where
    Span: Copy,
{
    pub(crate) fn from_generic(
        generic: Vec<GenericTokenTree<Span>>,
    ) -> Result<Vec<TokenTree<Span>>, Error<Span>> {
        let mut out = Vec::with_capacity(generic.len());
        let mut iter = generic.into_iter();

        while let Some(token) = iter.next() {
            let token = match token.kind {
                // $
                GenericTokenTreeKind::Terminal(Terminal::Dollar) => {
                    let Some(after_dollar) = iter.next() else {
                        return Err(Error::UnexpectedEnd {
                            last_token: Some(token.span),
                        });
                    };

                    match after_dollar.kind {
                        // $ident
                        GenericTokenTreeKind::Terminal(Terminal::Ident(id)) => {
                            Self::parse_fragment(&mut iter, id, after_dollar.span)
                        }

                        // $(...)
                        GenericTokenTreeKind::Parenthesed(inner) => {
                            Self::parse_repetition(&mut iter, inner, after_dollar.span)
                        }

                        GenericTokenTreeKind::Terminal(_)
                        | GenericTokenTreeKind::CurlyBraced(_)
                        | GenericTokenTreeKind::Bracketed(_) => {
                            return Err(Error::ParsingFailed {
                                what: vec![
                                    MacroRuleNode::Repetition,
                                    MacroRuleNode::MetaVariableMatch,
                                ],
                                where_: after_dollar.span,
                            });
                        }
                    }?
                }

                GenericTokenTreeKind::Terminal(t) => {
                    TokenTreeKind::Terminal(t).with_span(token.span)
                }

                GenericTokenTreeKind::Parenthesed(inner) => {
                    TokenTreeKind::Parenthesed(TokenTree::from_generic(inner)?)
                        .with_span(token.span)
                }

                GenericTokenTreeKind::CurlyBraced(inner) => {
                    TokenTreeKind::CurlyBraced(TokenTree::from_generic(inner)?)
                        .with_span(token.span)
                }

                GenericTokenTreeKind::Bracketed(inner) => {
                    TokenTreeKind::Bracketed(TokenTree::from_generic(inner)?).with_span(token.span)
                }
            };

            out.push(token);
        }

        Ok(out)
    }

    fn parse_fragment(
        iter: &mut impl Iterator<Item = GenericTokenTree<Span>>,
        name: String,
        // TODO
        span: Span,
    ) -> Result<TokenTree<Span>, Error<Span>> {
        // $ident

        let Some(token) = iter.next() else {
            return Err(Error::UnexpectedEnd {
                last_token: Some(span),
            });
        };

        let GenericTokenTreeKind::Terminal(Terminal::Colon) = token.kind else {
            return Err(Error::ParsingFailed {
                what: vec![MacroRuleNode::FragmentName],
                where_: token.span,
            });
        };
        // TODO: we want to be able to expand the span somehow.
        let span = token.span;

        // $ident:

        let Some(token) = iter.next() else {
            return Err(Error::UnexpectedEnd {
                last_token: Some(span),
            });
        };

        let GenericTokenTreeKind::Terminal(Terminal::Ident(kind)) = token.kind else {
            return Err(Error::ParsingFailed {
                what: vec![MacroRuleNode::FragmentSpecifier],
                where_: token.span,
            });
        };

        let Ok(kind) = kind.parse() else {
            return Err(Error::ParsingFailed {
                what: vec![MacroRuleNode::FragmentSpecifier],
                where_: token.span,
            });
        };

        // $ident:kind

        Ok(TokenTreeKind::Binding { name, kind }.with_span(span))
    }

    fn parse_repetition(
        iter: &mut impl Iterator<Item = GenericTokenTree<Span>>,
        inner: Vec<GenericTokenTree<Span>>,
        span: Span,
    ) -> Result<TokenTree<Span>, Error<Span>> {
        let inner = TokenTree::from_generic(inner)?;

        let Some(token) = iter.next() else {
            return Err(Error::UnexpectedEnd {
                last_token: Some(span),
            });
        };

        let (separator, quantifier) = match token.kind {
            GenericTokenTreeKind::Terminal(Terminal::QuestionMark) => (
                None,
                RepetitionQuantifierKind::ZeroOrOne.with_span(token.span),
            ),

            GenericTokenTreeKind::Terminal(Terminal::Star) => (
                None,
                RepetitionQuantifierKind::ZeroOrMore.with_span(token.span),
            ),

            GenericTokenTreeKind::Terminal(Terminal::Plus) => (
                None,
                RepetitionQuantifierKind::OneOrMore.with_span(token.span),
            ),

            GenericTokenTreeKind::Terminal(sep) => {
                let sep = TokenTreeKind::Terminal(sep).with_span(token.span);

                let Some(token) = iter.next() else {
                    return Err(Error::UnexpectedEnd {
                        last_token: Some(token.span),
                    });
                };

                let quantifier = match token.kind {
                    GenericTokenTreeKind::Terminal(Terminal::QuestionMark) => {
                        RepetitionQuantifierKind::ZeroOrOne
                    }
                    GenericTokenTreeKind::Terminal(Terminal::Star) => {
                        RepetitionQuantifierKind::ZeroOrMore
                    }
                    GenericTokenTreeKind::Terminal(Terminal::Plus) => {
                        RepetitionQuantifierKind::OneOrMore
                    }

                    _ => {
                        return Err(Error::ParsingFailed {
                            what: vec![MacroRuleNode::RepetitionQuantifier],
                            where_: token.span,
                        });
                    }
                }
                .with_span(token.span);

                (Some(Box::new(sep)), quantifier)
            }

            GenericTokenTreeKind::Parenthesed(_)
            | GenericTokenTreeKind::CurlyBraced(_)
            | GenericTokenTreeKind::Bracketed(_) => {
                return Err(Error::ParsingFailed {
                    what: vec![
                        MacroRuleNode::RepetitionSeparator,
                        MacroRuleNode::RepetitionQuantifier,
                    ],
                    where_: token.span,
                });
            }
        };

        Ok(TokenTree {
            kind: TokenTreeKind::Repetition {
                inner,
                separator,
                quantifier,
            },
            span,
        })
    }
}

#[derive(Clone, PartialEq)]
pub(crate) struct Matcher<Span> {
    pub(crate) bindings: HashMap<String, BindingData<Span>>,
}

impl<Span> Debug for Matcher<Span>
where
    Span: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // HashMaps are not ordered. As we need a deterministic debug output,
        // we have to make a shiny debug impl.
        struct Helper<'a, Span>(Vec<(&'a String, &'a BindingData<Span>)>);

        impl<Span> Debug for Helper<'_, Span>
        where
            Span: Debug,
        {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_map().entries(self.0.iter().copied()).finish()
            }
        }

        let mut ordered_keys = self.bindings.iter().collect::<Vec<_>>();
        ordered_keys.sort_by_key(|&(name, _)| name);

        f.debug_struct("Matcher")
            .field("bindings", &Helper(ordered_keys))
            .finish()
    }
}

impl<Span> Matcher<Span>
where
    Span: Copy,
{
    pub(crate) fn from_generic(tokens: &[TokenTree<Span>]) -> Result<Matcher<Span>, Error<Span>> {
        let mut bindings = HashMap::new();

        fn visit<Span>(
            bindings: &mut HashMap<String, BindingData<Span>>,
            stack: &LameLinkedList<RepetitionQuantifierKind>,
            token: &TokenTree<Span>,
        ) where
            Span: Copy,
        {
            match &token.kind {
                TokenTreeKind::Terminal(_) => {}

                TokenTreeKind::Binding { name, kind } => {
                    let kind = *kind;
                    let repetition_stack = stack.to_vec();
                    let data = BindingData {
                        kind,
                        span: token.span,
                        repetition_stack,
                    };

                    // TODO: properly check that there is no other binding with
                    // that name.
                    let prev = bindings.insert(name.clone(), data);
                    assert!(prev.is_none());
                }

                TokenTreeKind::Repetition {
                    inner, quantifier, ..
                } => {
                    let stack = LameLinkedList::Cons(quantifier.kind, stack);
                    inner.iter().for_each(|tt| visit(bindings, &stack, tt));
                }

                TokenTreeKind::Parenthesed(inner)
                | TokenTreeKind::CurlyBraced(inner)
                | TokenTreeKind::Bracketed(inner) => {
                    inner.iter().for_each(|tt| visit(bindings, stack, tt));
                }
            }
        }

        let stack = LameLinkedList::Nil;
        tokens
            .iter()
            .for_each(|tt| visit(&mut bindings, &stack, tt));

        Ok(Matcher { bindings })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct BindingData<Span> {
    pub(crate) kind: FragmentKind,
    pub(crate) span: Span,
    pub(crate) repetition_stack: Vec<RepetitionQuantifierKind>,
}

#[cfg(test)]
mod local_tree_to_matcher {
    use super::*;

    macro_rules! matcher_conv_test {
        ($test_name:ident {
            { $( $left:tt )* }, $right:expr $(,)?
        }) => {
            #[test]
            fn $test_name() {
                let tokens = quote! {
                    $( $left )*
                };

                let tokens = TokenTree::from_generic(tokens).expect("Failed to convert to `matcher::TokenTree`");
                let rslt = Matcher::from_generic(&tokens);

                let expected = $right;

                expected.assert_debug_eq(&rslt);
            }
        };
    }

    matcher_conv_test! {
        without_bindings {
            { a b c + d },
            expect_test::expect![[r#"
                Ok(
                    Matcher {
                        bindings: {},
                    },
                )
            "#]]
        }
    }

    matcher_conv_test! {
        multiple_bindings {
            { #a: ident + #b: expr },
            expect_test::expect![[r#"
                Ok(
                    Matcher {
                        bindings: {
                            "a": BindingData {
                                kind: Ident,
                                span: 2,
                                repetition_stack: [],
                            },
                            "b": BindingData {
                                kind: Expr,
                                span: 7,
                                repetition_stack: [],
                            },
                        },
                    },
                )
            "#]],
        }
    }

    matcher_conv_test! {
        nesting {
            { (((#a: ident)))},
            expect_test::expect![[r#"
                Ok(
                    Matcher {
                        bindings: {
                            "a": BindingData {
                                kind: Ident,
                                span: 5,
                                repetition_stack: [],
                            },
                        },
                    },
                )
            "#]],
        }
    }

    matcher_conv_test! {
        handles_repetitions {
            { #( #a:ident )* },
            expect_test::expect![[r#"
                Ok(
                    Matcher {
                        bindings: {
                            "a": BindingData {
                                kind: Ident,
                                span: 4,
                                repetition_stack: [
                                    ZeroOrMore,
                                ],
                            },
                        },
                    },
                )
            "#]],
        }
    }
}
