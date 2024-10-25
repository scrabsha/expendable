#![allow(missing_docs)] // TODO: write docs

use std::collections::HashMap;

use proc_macro2::{
    Delimiter, Group as GenericGroup, Ident, Literal, Punct, Span,
    TokenStream as GenericTokenStream, TokenTree as GenericTokenTree,
};

use crate::{Error, FragmentKind, MacroRuleNode};

#[derive(Debug)]
pub enum TokenTree {
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    Group(Group),
    Metavariable(Metavariable),
    Repetition(Repetition),
}

#[derive(Debug)]
pub struct Group {
    pub content: Vec<TokenTree>,
    pub delimiter: Delimiter,
    pub span: Span,
}

#[derive(Debug)]
pub struct Repetition {
    pub id: RepetitionId,
    pub content: Vec<TokenTree>,
    pub separator: Separator,
    pub count: RepetitionCount,
    pub span: Span,
}

#[derive(Debug)]
pub enum Separator {
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    None,
}

impl Separator {
    pub(crate) fn to_token_tree(self) -> Option<GenericTokenTree> {
        match self {
            Separator::Ident(ident) => Some(GenericTokenTree::Ident(ident)),
            Separator::Punct(punct) => Some(GenericTokenTree::Punct(punct)),
            Separator::Literal(literal) => Some(GenericTokenTree::Literal(literal)),
            Separator::None => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RepetitionId(usize);

#[derive(Debug)]
pub enum RepetitionCount {
    AtMostOne,
    ZeroOrMore,
    OneOrMore,
}

#[derive(Debug)]
pub struct Metavariable {
    pub name: Ident,
    pub kind: FragmentKind,
    pub span: Span,
}

pub(crate) struct ParseCtxt {
    counter: usize,
    mode: ParseMode,
    metavariables: HashMap<Ident, FragmentKind>,
}

impl ParseCtxt {
    pub(crate) fn matcher() -> ParseCtxt {
        let mode = ParseMode::Matcher;
        ParseCtxt {
            counter: 0,
            mode,
            metavariables: HashMap::new(),
        }
    }

    pub(crate) fn turn_into_transcriber(&mut self) {
        self.mode = ParseMode::Transcriber;
    }

    fn with_mode(mode: ParseMode) -> ParseCtxt {
        ParseCtxt {
            counter: 0,
            mode,
            metavariables: HashMap::new(),
        }
    }

    fn id(&mut self) -> RepetitionId {
        let id = RepetitionId(self.counter);
        self.counter += 1;

        id
    }
}

enum ParseMode {
    Matcher,
    Transcriber,
}

impl TokenTree {
    pub(crate) fn from_generic(
        ctx: &mut ParseCtxt,
        tokens: GenericTokenStream,
    ) -> Result<Vec<TokenTree>, Error> {
        let mut iter = tokens.into_iter();
        Self::parse_seq(ctx, &mut iter)
    }

    fn parse_seq(
        ctx: &mut ParseCtxt,
        iter: &mut impl Iterator<Item = GenericTokenTree>,
    ) -> Result<Vec<TokenTree>, Error> {
        let mut out = Vec::new();

        while let Some(token) = iter.next() {
            let token = match token {
                // $
                GenericTokenTree::Punct(punct) if punct.as_char() == '$' => {
                    let Some(after_dollar) = iter.next() else {
                        return Err(Error::UnexpectedEnd {
                            last_token: Some(punct.span()),
                        });
                    };

                    match after_dollar {
                        // $ident
                        GenericTokenTree::Ident(ident) => Self::parse_fragment(ctx, iter, ident),

                        // $(...)
                        GenericTokenTree::Group(group)
                            if group.delimiter() == Delimiter::Parenthesis =>
                        {
                            Self::parse_repetition(ctx, iter, group)
                        }

                        anything => {
                            return Err(Error::ParsingFailed {
                                what: vec![
                                    MacroRuleNode::Repetition,
                                    MacroRuleNode::MetaVariableMatch,
                                ],
                                where_: anything.span(),
                            });
                        }
                    }?
                }

                GenericTokenTree::Ident(ident) => TokenTree::Ident(ident),

                GenericTokenTree::Punct(punct) => TokenTree::Punct(punct),

                GenericTokenTree::Literal(literal) => TokenTree::Literal(literal),

                GenericTokenTree::Group(group) => TokenTree::Group(Self::parse_group(ctx, group)?),
            };

            out.push(token);
        }

        Ok(out)
    }

    fn parse_fragment(
        ctx: &mut ParseCtxt,
        iter: &mut impl Iterator<Item = GenericTokenTree>,
        name: Ident,
        // TODO
    ) -> Result<TokenTree, Error> {
        let span = name.span();
        // $ident

        let (kind, span) = match ctx.mode {
            // $ident:kind
            ParseMode::Matcher => {
                // :
                let Some(token) = iter.next() else {
                    return Err(Error::UnexpectedEnd {
                        last_token: Some(span),
                    });
                };
                let GenericTokenTree::Punct(token) = token else {
                    return Err(Error::ParsingFailed {
                        what: vec![MacroRuleNode::FragmentName],
                        where_: token.span(),
                    });
                };

                if token.as_char() != ':' {
                    return Err(Error::ParsingFailed {
                        what: vec![MacroRuleNode::FragmentName],
                        where_: token.span(),
                    });
                }
                // TODO: we want to be able to expand the span somehow.
                let span = token.span();

                // $ident:

                let Some(token) = iter.next() else {
                    return Err(Error::UnexpectedEnd {
                        last_token: Some(span),
                    });
                };

                let span = token.span();
                let GenericTokenTree::Ident(ident) = token else {
                    return Err(Error::ParsingFailed {
                        what: vec![MacroRuleNode::FragmentSpecifier],
                        where_: token.span(),
                    });
                };

                let Ok(kind) = ident.to_string().parse() else {
                    return Err(Error::ParsingFailed {
                        what: vec![MacroRuleNode::FragmentSpecifier],
                        where_: span,
                    });
                };

                let prev = ctx.metavariables.insert(name.clone(), kind);

                if prev.is_some() {
                    todo!("Redefinition of metavariable")
                }

                (kind, span)
            }

            // $ident (kind obtained from the symbol table).
            ParseMode::Transcriber => {
                let span = name.span();
                let kind =
                    *ctx.metavariables
                        .get(&name)
                        .ok_or_else(|| Error::UnboundMetavariable {
                            name: name.to_string(),
                            where_: span,
                        })?;

                (kind, span)
            }
        };

        Ok(TokenTree::Metavariable(Metavariable { name, kind, span }))
    }

    fn parse_repetition(
        ctx: &mut ParseCtxt,
        iter: &mut impl Iterator<Item = GenericTokenTree>,
        group: GenericGroup,
    ) -> Result<TokenTree, Error> {
        let span = group.span();

        let id = ctx.id();

        let content = TokenTree::parse_group(ctx, group)?.content;

        let mut try_parse_quantifier = || {
            let Some(token) = iter.next() else {
                return Err(Error::UnexpectedEnd {
                    last_token: Some(span),
                });
            };

            match token {
                GenericTokenTree::Punct(punct) if punct.as_char() == '?' => {
                    Ok(Ok((RepetitionCount::AtMostOne, punct.span())))
                }

                GenericTokenTree::Punct(punct) if punct.as_char() == '*' => {
                    Ok(Ok((RepetitionCount::ZeroOrMore, punct.span())))
                }

                GenericTokenTree::Punct(punct) if punct.as_char() == '+' => {
                    Ok(Ok((RepetitionCount::OneOrMore, punct.span())))
                }

                GenericTokenTree::Ident(ident) => Ok(Err(Separator::Ident(ident))),

                GenericTokenTree::Literal(literal) => Ok(Err(Separator::Literal(literal))),

                GenericTokenTree::Punct(punct) => Ok(Err(Separator::Punct(punct))),

                GenericTokenTree::Group(_) => Err(Error::ParsingFailed {
                    what: vec![
                        MacroRuleNode::RepetitionSeparator,
                        MacroRuleNode::RepetitionQuantifier,
                    ],
                    where_: token.span(),
                }),
            }
        };

        let (separator, count) = {
            match try_parse_quantifier()? {
                Ok((count, _)) => (Separator::None, count),

                Err(separator) => match try_parse_quantifier()? {
                    Ok((count, _)) => (separator, count),

                    Err(separator) => {
                        let tree = match separator.to_token_tree() {
                            Some(tree) => tree,
                            None => unreachable!(),
                        };
                        return Err(Error::InvalidSeparator { tree });
                    }
                },
            }
        };

        Ok(TokenTree::Repetition(Repetition {
            id,
            content,
            separator,
            count,
            span,
        }))
    }

    fn parse_group(ctx: &mut ParseCtxt, group: GenericGroup) -> Result<Group, Error> {
        let mut iter = group.stream().into_iter();

        let content = Self::parse_seq(ctx, &mut iter)?;
        let delimiter = group.delimiter();
        let span = group.span();

        Ok(Group {
            content,
            delimiter,
            span,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test {
        (fn $name:ident() { ($($matcher:tt)*) => {$($transcriber:tt)*}, $expected:expr $(,)? }) => {
            #[test]
            fn $name() {
                let matcher = ::quote::quote! { $($matcher)* };
                let transcriber = ::quote::quote! { $($transcriber)* };

                let mut ctx = ParseCtxt::matcher();

                let matcher = TokenTree::from_generic(&mut ctx, matcher);

                let matcher = match matcher {
                    Ok(matcher) => matcher,
                    Err(err) => {
                        let expected = $expected;
                        expected.assert_debug_eq(&err);
                        return;
                    }
                };

                ctx.turn_into_transcriber();

                let transcriber = TokenTree::from_generic(&mut ctx, transcriber);
                let transcriber = match transcriber {
                    Ok(transcriber) => transcriber,
                    Err(err) => {
                        let expected = $expected;
                        expected.assert_debug_eq(&err);
                        return;
                    }
                };

                let expected = $expected;
                expected.assert_debug_eq(&(matcher, transcriber));
            }
        };
    }

    test! {
        fn both_empty() {
            () => {},
            expect_test::expect![[r#"
                (
                    [],
                    [],
                )
            "#]],
        }
    }

    test! {
        fn simple_sequence() {
            (a 1 b 2 ; + @) => { a 1 b 2 ; + @ },
            expect_test::expect![[r#"
                (
                    [
                        Ident(
                            Ident {
                                sym: a,
                            },
                        ),
                        Literal(
                            Literal {
                                lit: 1,
                                span: bytes(1..2),
                            },
                        ),
                        Ident(
                            Ident {
                                sym: b,
                            },
                        ),
                        Literal(
                            Literal {
                                lit: 2,
                                span: bytes(3..4),
                            },
                        ),
                        Punct(
                            Punct {
                                char: ';',
                                spacing: Alone,
                            },
                        ),
                        Punct(
                            Punct {
                                char: '+',
                                spacing: Alone,
                            },
                        ),
                        Punct(
                            Punct {
                                char: '@',
                                spacing: Alone,
                            },
                        ),
                    ],
                    [
                        Ident(
                            Ident {
                                sym: a,
                            },
                        ),
                        Literal(
                            Literal {
                                lit: 1,
                                span: bytes(5..6),
                            },
                        ),
                        Ident(
                            Ident {
                                sym: b,
                            },
                        ),
                        Literal(
                            Literal {
                                lit: 2,
                                span: bytes(7..8),
                            },
                        ),
                        Punct(
                            Punct {
                                char: ';',
                                spacing: Alone,
                            },
                        ),
                        Punct(
                            Punct {
                                char: '+',
                                spacing: Alone,
                            },
                        ),
                        Punct(
                            Punct {
                                char: '@',
                                spacing: Alone,
                            },
                        ),
                    ],
                )
            "#]],
        }
    }

    test! {
        fn super_simple_repetition() {
            ( $( $a:ident )* ) => { $( $a )* },
            expect_test::expect![[r#"
                (
                    [
                        Repetition(
                            Repetition {
                                id: RepetitionId(
                                    0,
                                ),
                                content: [
                                    Metavariable(
                                        Metavariable {
                                            name: Ident {
                                                sym: a,
                                            },
                                            kind: Ident,
                                            span: bytes(0..0),
                                        },
                                    ),
                                ],
                                separator: None,
                                count: ZeroOrMore,
                                span: bytes(0..0),
                            },
                        ),
                    ],
                    [
                        Repetition(
                            Repetition {
                                id: RepetitionId(
                                    1,
                                ),
                                content: [
                                    Metavariable(
                                        Metavariable {
                                            name: Ident {
                                                sym: a,
                                            },
                                            kind: Ident,
                                            span: bytes(0..0),
                                        },
                                    ),
                                ],
                                separator: None,
                                count: ZeroOrMore,
                                span: bytes(0..0),
                            },
                        ),
                    ],
                )
            "#]],
        }
    }

    test! {
        fn random_complicated_test() {
            ($($foo:ident $($bar:ident)*)*) => { $($($foo $bar)*)* },
            expect_test::expect![[r#"
                (
                    [
                        Repetition(
                            Repetition {
                                id: RepetitionId(
                                    0,
                                ),
                                content: [
                                    Metavariable(
                                        Metavariable {
                                            name: Ident {
                                                sym: foo,
                                            },
                                            kind: Ident,
                                            span: bytes(0..0),
                                        },
                                    ),
                                    Repetition(
                                        Repetition {
                                            id: RepetitionId(
                                                1,
                                            ),
                                            content: [
                                                Metavariable(
                                                    Metavariable {
                                                        name: Ident {
                                                            sym: bar,
                                                        },
                                                        kind: Ident,
                                                        span: bytes(0..0),
                                                    },
                                                ),
                                            ],
                                            separator: None,
                                            count: ZeroOrMore,
                                            span: bytes(0..0),
                                        },
                                    ),
                                ],
                                separator: None,
                                count: ZeroOrMore,
                                span: bytes(0..0),
                            },
                        ),
                    ],
                    [
                        Repetition(
                            Repetition {
                                id: RepetitionId(
                                    2,
                                ),
                                content: [
                                    Repetition(
                                        Repetition {
                                            id: RepetitionId(
                                                3,
                                            ),
                                            content: [
                                                Metavariable(
                                                    Metavariable {
                                                        name: Ident {
                                                            sym: foo,
                                                        },
                                                        kind: Ident,
                                                        span: bytes(0..0),
                                                    },
                                                ),
                                                Metavariable(
                                                    Metavariable {
                                                        name: Ident {
                                                            sym: bar,
                                                        },
                                                        kind: Ident,
                                                        span: bytes(0..0),
                                                    },
                                                ),
                                            ],
                                            separator: None,
                                            count: ZeroOrMore,
                                            span: bytes(0..0),
                                        },
                                    ),
                                ],
                                separator: None,
                                count: ZeroOrMore,
                                span: bytes(0..0),
                            },
                        ),
                    ],
                )
            "#]],
        }
    }

    test! {
        fn group_nesting() {
            ([ bracket ] { brace } ( parethensis )) => {( parenthesis ) { brace } [ bracket ]},
            expect_test::expect![[r#"
                (
                    [
                        Group(
                            Group {
                                content: [
                                    Ident(
                                        Ident {
                                            sym: bracket,
                                        },
                                    ),
                                ],
                                delimiter: Bracket,
                                span: bytes(0..0),
                            },
                        ),
                        Group(
                            Group {
                                content: [
                                    Ident(
                                        Ident {
                                            sym: brace,
                                        },
                                    ),
                                ],
                                delimiter: Brace,
                                span: bytes(0..0),
                            },
                        ),
                        Group(
                            Group {
                                content: [
                                    Ident(
                                        Ident {
                                            sym: parethensis,
                                        },
                                    ),
                                ],
                                delimiter: Parenthesis,
                                span: bytes(0..0),
                            },
                        ),
                    ],
                    [
                        Group(
                            Group {
                                content: [
                                    Ident(
                                        Ident {
                                            sym: parenthesis,
                                        },
                                    ),
                                ],
                                delimiter: Parenthesis,
                                span: bytes(0..0),
                            },
                        ),
                        Group(
                            Group {
                                content: [
                                    Ident(
                                        Ident {
                                            sym: brace,
                                        },
                                    ),
                                ],
                                delimiter: Brace,
                                span: bytes(0..0),
                            },
                        ),
                        Group(
                            Group {
                                content: [
                                    Ident(
                                        Ident {
                                            sym: bracket,
                                        },
                                    ),
                                ],
                                delimiter: Bracket,
                                span: bytes(0..0),
                            },
                        ),
                    ],
                )
            "#]]
        }
    }
}
