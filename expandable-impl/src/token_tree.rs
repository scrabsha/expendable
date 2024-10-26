#![allow(missing_docs)] // TODO: write docs

use std::{
    collections::HashMap,
    fmt::{self, Debug},
    iter,
};

use proc_macro2::{
    Delimiter, Group as GenericGroup, Ident, Literal, Punct, Spacing, Span as GenericSpan,
    TokenStream as GenericTokenStream, TokenTree as GenericTokenTree,
};

use crate::{Error, FragmentKind, MacroRuleNode};

#[derive(Clone)]
pub enum TokenTree {
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    Group(Group),
    Metavariable(Metavariable),
    Repetition(Repetition),
}

impl Debug for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Ident(ident) => DebugHelper(ident).fmt(f),
            TokenTree::Punct(punct) => DebugHelper(punct).fmt(f),
            TokenTree::Literal(literal) => DebugHelper(literal).fmt(f),
            TokenTree::Group(group) => group.fmt(f),
            TokenTree::Metavariable(metavariable) => metavariable.fmt(f),
            TokenTree::Repetition(repetition) => repetition.fmt(f),
        }
    }
}

struct DebugHelper<T>(T);

impl Debug for DebugHelper<&Ident> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ident")
            .field("sym", &self.0.to_string())
            .field("span", &Span(self.0.span()))
            .finish()
    }
}

impl Debug for DebugHelper<&Punct> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("punct")
            .field("ch", &self.0.as_char())
            .field("spacing", &self.0.spacing())
            .field("span", &Span(self.0.span()))
            .finish()
    }
}

impl Debug for DebugHelper<&Literal> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Literal")
            .field("lit", &self.0.to_string())
            .field("span", &Span(self.0.span()))
            .finish()
    }
}

#[derive(Clone)]
pub struct Group {
    pub content: Vec<TokenTree>,
    pub delimiter: Delimiter,
    pub span: Span,
}

impl Debug for Group {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Group")
            .field("delimiter", &self.delimiter)
            .field("stream", &self.content)
            .field("span", &self.span)
            .finish()
    }
}

#[derive(Clone)]
pub struct Repetition {
    pub id: RepetitionId,
    pub dollar: Span,
    pub paren: Span,
    pub content: Vec<TokenTree>,
    pub separator: Separator,
    pub count: RepetitionCount,
    pub count_span: Span,
    pub span: Span,
}

impl Debug for Repetition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Repetition")
            .field("id", &self.id)
            .field("dollar", &self.dollar)
            .field("paren", &self.paren)
            .field("stream", &self.content)
            .field("separator", &self.separator)
            .field("count", &self.count)
            .field("count_span", &self.count_span)
            .field("span", &self.span)
            .finish()
    }
}

#[derive(Clone)]
pub enum Separator {
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    None,
}

impl Debug for Separator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Separator::Ident(ident) => DebugHelper(ident).fmt(f),
            Separator::Punct(punct) => DebugHelper(punct).fmt(f),
            Separator::Literal(literal) => DebugHelper(literal).fmt(f),
            Separator::None => write!(f, "None"),
        }
    }
}

impl Separator {
    pub(crate) fn span(&self) -> Option<Span> {
        match self {
            Separator::Ident(ident) => Some(ident.span().into()),
            Separator::Punct(punct) => Some(punct.span().into()),
            Separator::Literal(literal) => Some(literal.span().into()),
            Separator::None => None,
        }
    }

    pub(crate) fn into_token_tree(self) -> Option<GenericTokenTree> {
        match self {
            Separator::Ident(ident) => Some(GenericTokenTree::Ident(ident)),
            Separator::Punct(punct) => Some(GenericTokenTree::Punct(punct)),
            Separator::Literal(literal) => Some(GenericTokenTree::Literal(literal)),
            Separator::None => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RepetitionId(usize);

impl fmt::Debug for RepetitionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "repetition {}", self.0)
    }
}

#[derive(Clone, Copy)]
pub enum RepetitionCount {
    AtMostOne,
    ZeroOrMore,
    OneOrMore,
}

impl Debug for RepetitionCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            RepetitionCount::AtMostOne => "AtMostOne (?)",
            RepetitionCount::ZeroOrMore => "ZeroOrMore (*)",
            RepetitionCount::OneOrMore => "OneOrMore (+)",
        };

        write!(f, "{string}")
    }
}

impl RepetitionCount {
    fn as_char(self) -> char {
        match self {
            RepetitionCount::AtMostOne => '?',
            RepetitionCount::ZeroOrMore => '*',
            RepetitionCount::OneOrMore => '+',
        }
    }
}

#[derive(Clone)]
pub struct Metavariable {
    pub dollar: Span,
    pub name: Ident,
    pub kind: FragmentKind,
    // None when parsing a transcriber :3
    pub matcher_spans: Option<(/* : */ Span, /* kind */ Span)>,
    pub span: Span,
}

impl Debug for Metavariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Metavariable")
            .field("dollar", &self.dollar)
            .field("name", &DebugHelper(&self.name))
            .field("kind", &self.kind)
            .field("matcher_spans", &self.matcher_spans)
            .field("span", &self.span)
            .finish()
    }
}

#[derive(Copy, Clone)]
pub struct Span(GenericSpan);

impl Span {
    pub fn join<I: Into<Span>>(&self, other: I) -> Span {
        // This is best-effort, because in a proc macro context on stable compilers the
        // join method is not available (as it is unstable).
        Span(self.0.join(other.into().0).unwrap_or(self.0))
    }
}

impl From<GenericSpan> for Span {
    fn from(inner: GenericSpan) -> Self {
        Span(inner)
    }
}

impl From<Span> for GenericSpan {
    fn from(val: Span) -> GenericSpan {
        val.0
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(text) = self.0.source_text() {
            write!(f, "span( {text} )")
        } else {
            write!(f, "no source-map available (span: {:?})", &self.0)
        }
    }
}

pub(crate) struct ParseCtxt {
    counter: usize,
    mode: ParseMode,
    metavariables: HashMap<Ident, FragmentKind>,
}

impl ParseCtxt {
    #[cfg_attr(not(test), expect(dead_code))] // TODO: use it
    pub(crate) fn matcher() -> ParseCtxt {
        let mode = ParseMode::Matcher;
        ParseCtxt {
            counter: 0,
            mode,
            metavariables: HashMap::new(),
        }
    }

    #[cfg_attr(not(test), expect(dead_code))] // TODO: use it
    pub(crate) fn turn_into_transcriber(&mut self) {
        self.mode = ParseMode::Transcriber;
    }

    fn id(&mut self) -> RepetitionId {
        let id = RepetitionId(self.counter);
        self.counter += 1;

        id
    }
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum ParseMode {
    Matcher,
    Transcriber,
}

impl TokenTree {
    #[cfg_attr(not(test), expect(dead_code))] // TODO: use it
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
                        GenericTokenTree::Ident(ident) => {
                            Self::parse_fragment(ctx, iter, punct.span().into(), ident)
                        }

                        // $(...)
                        GenericTokenTree::Group(group)
                            if group.delimiter() == Delimiter::Parenthesis =>
                        {
                            Self::parse_repetition(
                                ctx,
                                punct.span().into(),
                                group.span().into(),
                                iter,
                                group,
                            )
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
        dollar: Span,
        name: Ident,
        // TODO
    ) -> Result<TokenTree, Error> {
        let mut span = dollar.join(name.span());
        // $ident

        let (kind, matcher_spans) = match ctx.mode {
            // $ident:kind
            ParseMode::Matcher => {
                // :
                let Some(token) = iter.next() else {
                    return Err(Error::UnexpectedEnd {
                        last_token: Some(span.into()),
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

                let colon_span = token.span();
                span = span.join(colon_span);

                // $ident:

                let Some(token) = iter.next() else {
                    return Err(Error::UnexpectedEnd {
                        last_token: Some(colon_span),
                    });
                };

                span = span.join(token.span());

                let GenericTokenTree::Ident(ident) = token else {
                    return Err(Error::ParsingFailed {
                        what: vec![MacroRuleNode::FragmentSpecifier],
                        where_: token.span(),
                    });
                };

                let Ok(kind) = ident.to_string().parse() else {
                    return Err(Error::ParsingFailed {
                        what: vec![MacroRuleNode::FragmentSpecifier],
                        where_: span.into(),
                    });
                };

                let kind_span = ident.span();

                let prev = ctx.metavariables.insert(name.clone(), kind);

                if prev.is_some() {
                    todo!("Redefinition of metavariable")
                }

                (kind, Some((colon_span.into(), kind_span.into())))
            }

            // $ident (kind obtained from the symbol table).
            ParseMode::Transcriber => {
                let kind =
                    *ctx.metavariables
                        .get(&name)
                        .ok_or_else(|| Error::UnboundMetavariable {
                            name: name.to_string(),
                            where_: span.into(),
                        })?;

                (kind, None)
            }
        };

        Ok(TokenTree::Metavariable(Metavariable {
            dollar,
            name,
            kind,
            matcher_spans,
            span,
        }))
    }

    fn parse_repetition(
        ctx: &mut ParseCtxt,
        dollar: Span,
        paren: Span,
        iter: &mut impl Iterator<Item = GenericTokenTree>,
        group: GenericGroup,
    ) -> Result<TokenTree, Error> {
        let mut span = dollar.join(group.span());

        let id = ctx.id();

        let content = TokenTree::parse_group(ctx, group)?.content;

        let mut try_parse_quantifier = || -> Result<_, Error> {
            let Some(token) = iter.next() else {
                return Err(Error::UnexpectedEnd {
                    last_token: Some(span.into()),
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

        let (separator, count, count_span) = {
            match try_parse_quantifier()? {
                Ok((count, count_span)) => (Separator::None, count, count_span),

                Err(separator) => match try_parse_quantifier()? {
                    Ok((count, count_span)) => (separator, count, count_span),

                    Err(separator) => {
                        let tree = match separator.into_token_tree() {
                            Some(tree) => tree,
                            None => unreachable!(),
                        };
                        return Err(Error::InvalidSeparator { tree });
                    }
                },
            }
        };

        if let Some(separator_span) = separator.span() {
            span = span.join(separator_span);
        }
        span = span.join(count_span);

        Ok(TokenTree::Repetition(Repetition {
            id,
            dollar,
            paren,
            content,
            separator,
            count,
            count_span: count_span.into(),
            span,
        }))
    }

    fn parse_group(ctx: &mut ParseCtxt, group: GenericGroup) -> Result<Group, Error> {
        let mut iter = group.stream().into_iter();

        let content = Self::parse_seq(ctx, &mut iter)?;
        let delimiter = group.delimiter();
        let span = group.span().into();

        Ok(Group {
            content,
            delimiter,
            span,
        })
    }

    #[cfg_attr(not(test), expect(dead_code))] // TODO: use it
    pub(crate) fn into_generic(stream: Vec<TokenTree>, mode: ParseMode) -> GenericTokenStream {
        stream
            .into_iter()
            .fold(GenericTokenStream::new(), |mut stream, tree| {
                tree.into_generic_tree(mode, &mut stream);
                stream
            })
    }

    fn into_generic_tree(self, mode: ParseMode, tokens: &mut GenericTokenStream) {
        match self {
            TokenTree::Ident(ident) => {
                tokens.extend(iter::once(GenericTokenTree::Ident(ident)));
            }

            TokenTree::Punct(punct) => {
                tokens.extend(iter::once(GenericTokenTree::Punct(punct)));
            }

            TokenTree::Literal(literal) => {
                tokens.extend(iter::once(GenericTokenTree::Literal(literal)));
            }

            TokenTree::Group(group) => {
                let stream = group.content.into_iter().fold(
                    GenericTokenStream::new(),
                    |mut stream, element| {
                        element.into_generic_tree(mode, &mut stream);
                        stream
                    },
                );

                let group = GenericGroup::new(group.delimiter, stream);
                let tree = GenericTokenTree::Group(group);

                tokens.extend(iter::once(tree));
            }

            TokenTree::Metavariable(metavariable) if mode == ParseMode::Matcher => {
                let mut dollar = Punct::new('$', Spacing::Alone);
                dollar.set_span(metavariable.dollar.into());
                let dollar = GenericTokenTree::Punct(dollar);

                let name = GenericTokenTree::Ident(metavariable.name);

                let (colon_span, kind_span) = metavariable
                    .matcher_spans
                    .expect("Attempt to convert a transcriber stream into a matcher stream");

                let mut colon = Punct::new(':', Spacing::Alone);
                colon.set_span(colon_span.into());
                let colon = GenericTokenTree::Punct(colon);

                let kind = Ident::new(metavariable.kind.to_str(), kind_span.into());
                let kind = GenericTokenTree::Ident(kind);

                tokens.extend([dollar, name, colon, kind]);
            }

            TokenTree::Metavariable(metavariable) => {
                let mut dollar = Punct::new('$', Spacing::Alone);
                dollar.set_span(metavariable.dollar.into());
                let dollar = GenericTokenTree::Punct(dollar);

                let name = GenericTokenTree::Ident(metavariable.name);

                assert!(
                    metavariable.matcher_spans.is_none(),
                    "Attempt to convert a matcher stream into a transcriber stream"
                );

                tokens.extend([dollar, name]);
            }

            TokenTree::Repetition(repetition) => {
                // $
                let mut dollar = Punct::new('$', Spacing::Alone);
                dollar.set_span(repetition.dollar.into());
                let dollar = GenericTokenTree::Punct(dollar);

                // (inner)
                let stream = repetition.content.into_iter().fold(
                    GenericTokenStream::new(),
                    |mut stream, tree| {
                        tree.into_generic_tree(mode, &mut stream);
                        stream
                    },
                );
                let mut group = GenericGroup::new(Delimiter::Parenthesis, stream);
                group.set_span(repetition.paren.into());
                let group = GenericTokenTree::Group(group);

                tokens.extend([dollar, group]);

                // sep
                if let Some(mut tree) = repetition.separator.into_token_tree() {
                    if let GenericTokenTree::Punct(ref mut punct) = tree {
                        // Set appropriate spacing here - we know the next token
                        // going to be a punct as well!
                        let ch = punct.as_char();
                        *punct = Punct::new(ch, Spacing::Joint);
                    }
                    tokens.extend(iter::once(tree));
                }

                // count
                let count = repetition.count.as_char();
                let mut count = Punct::new(count, Spacing::Alone);
                count.set_span(repetition.count_span.into());
                let count = GenericTokenTree::Punct(count);

                tokens.extend(iter::once(count));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test {
        (fn $name:ident() { ($($matcher:tt)*) => {$($transcriber:tt)*}, $expected:expr $(,)? }) => {
            #[test]
            fn $name() {
                let matcher = stringify!( $($matcher)* );
                let transcriber = stringify!( $($transcriber)* );

                let matcher = matcher.parse().unwrap();
                let transcriber = transcriber.parse().unwrap();

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

                let initial_matcher = TokenTree::into_generic(matcher.clone(), ParseMode::Matcher);
                let initial_transcriber = TokenTree::into_generic(transcriber.clone(), ParseMode::Transcriber);

                let expected = $expected;
                expected.assert_debug_eq(&(matcher, transcriber, initial_matcher, initial_transcriber));
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
                    TokenStream [],
                    TokenStream [],
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
                        Ident {
                            sym: "a",
                            span: span( a ),
                        },
                        Literal {
                            lit: "1",
                            span: span( 1 ),
                        },
                        Ident {
                            sym: "b",
                            span: span( b ),
                        },
                        Literal {
                            lit: "2",
                            span: span( 2 ),
                        },
                        punct {
                            ch: ';',
                            spacing: Alone,
                            span: span( ; ),
                        },
                        punct {
                            ch: '+',
                            spacing: Alone,
                            span: span( + ),
                        },
                        punct {
                            ch: '@',
                            spacing: Alone,
                            span: span( @ ),
                        },
                    ],
                    [
                        Ident {
                            sym: "a",
                            span: span( a ),
                        },
                        Literal {
                            lit: "1",
                            span: span( 1 ),
                        },
                        Ident {
                            sym: "b",
                            span: span( b ),
                        },
                        Literal {
                            lit: "2",
                            span: span( 2 ),
                        },
                        punct {
                            ch: ';',
                            spacing: Alone,
                            span: span( ; ),
                        },
                        punct {
                            ch: '+',
                            spacing: Alone,
                            span: span( + ),
                        },
                        punct {
                            ch: '@',
                            spacing: Alone,
                            span: span( @ ),
                        },
                    ],
                    TokenStream [
                        Ident {
                            sym: a,
                            span: bytes(1..2),
                        },
                        Literal {
                            lit: 1,
                            span: bytes(3..4),
                        },
                        Ident {
                            sym: b,
                            span: bytes(5..6),
                        },
                        Literal {
                            lit: 2,
                            span: bytes(7..8),
                        },
                        Punct {
                            char: ';',
                            spacing: Alone,
                            span: bytes(8..9),
                        },
                        Punct {
                            char: '+',
                            spacing: Alone,
                            span: bytes(10..11),
                        },
                        Punct {
                            char: '@',
                            spacing: Alone,
                            span: bytes(12..13),
                        },
                    ],
                    TokenStream [
                        Ident {
                            sym: a,
                            span: bytes(14..15),
                        },
                        Literal {
                            lit: 1,
                            span: bytes(16..17),
                        },
                        Ident {
                            sym: b,
                            span: bytes(18..19),
                        },
                        Literal {
                            lit: 2,
                            span: bytes(20..21),
                        },
                        Punct {
                            char: ';',
                            spacing: Alone,
                            span: bytes(21..22),
                        },
                        Punct {
                            char: '+',
                            spacing: Alone,
                            span: bytes(23..24),
                        },
                        Punct {
                            char: '@',
                            spacing: Alone,
                            span: bytes(25..26),
                        },
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
                        Repetition {
                            id: repetition 0,
                            dollar: span( $ ),
                            paren: span( ($a:ident) ),
                            stream: [
                                Metavariable {
                                    dollar: span( $ ),
                                    name: Ident {
                                        sym: "a",
                                        span: span( a ),
                                    },
                                    kind: Ident,
                                    matcher_spans: Some(
                                        (
                                            span( : ),
                                            span( ident ),
                                        ),
                                    ),
                                    span: span( $a:ident ),
                                },
                            ],
                            separator: None,
                            count: ZeroOrMore (*),
                            count_span: span( * ),
                            span: span( $($a:ident)* ),
                        },
                    ],
                    [
                        Repetition {
                            id: repetition 1,
                            dollar: span( $ ),
                            paren: span( ($a) ),
                            stream: [
                                Metavariable {
                                    dollar: span( $ ),
                                    name: Ident {
                                        sym: "a",
                                        span: span( a ),
                                    },
                                    kind: Ident,
                                    matcher_spans: None,
                                    span: span( $a ),
                                },
                            ],
                            separator: None,
                            count: ZeroOrMore (*),
                            count_span: span( * ),
                            span: span( $($a)* ),
                        },
                    ],
                    TokenStream [
                        Punct {
                            char: '$',
                            spacing: Alone,
                            span: bytes(1..2),
                        },
                        Group {
                            delimiter: Parenthesis,
                            stream: TokenStream [
                                Punct {
                                    char: '$',
                                    spacing: Alone,
                                    span: bytes(3..4),
                                },
                                Ident {
                                    sym: a,
                                    span: bytes(4..5),
                                },
                                Punct {
                                    char: ':',
                                    spacing: Alone,
                                    span: bytes(5..6),
                                },
                                Ident {
                                    sym: ident,
                                    span: bytes(6..11),
                                },
                            ],
                            span: bytes(2..12),
                        },
                        Punct {
                            char: '*',
                            spacing: Alone,
                            span: bytes(12..13),
                        },
                    ],
                    TokenStream [
                        Punct {
                            char: '$',
                            spacing: Alone,
                            span: bytes(14..15),
                        },
                        Group {
                            delimiter: Parenthesis,
                            stream: TokenStream [
                                Punct {
                                    char: '$',
                                    spacing: Alone,
                                    span: bytes(16..17),
                                },
                                Ident {
                                    sym: a,
                                    span: bytes(17..18),
                                },
                            ],
                            span: bytes(15..19),
                        },
                        Punct {
                            char: '*',
                            spacing: Alone,
                            span: bytes(19..20),
                        },
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
                        Repetition {
                            id: repetition 0,
                            dollar: span( $ ),
                            paren: span( ($foo:ident $($bar:ident)*) ),
                            stream: [
                                Metavariable {
                                    dollar: span( $ ),
                                    name: Ident {
                                        sym: "foo",
                                        span: span( foo ),
                                    },
                                    kind: Ident,
                                    matcher_spans: Some(
                                        (
                                            span( : ),
                                            span( ident ),
                                        ),
                                    ),
                                    span: span( $foo:ident ),
                                },
                                Repetition {
                                    id: repetition 1,
                                    dollar: span( $ ),
                                    paren: span( ($bar:ident) ),
                                    stream: [
                                        Metavariable {
                                            dollar: span( $ ),
                                            name: Ident {
                                                sym: "bar",
                                                span: span( bar ),
                                            },
                                            kind: Ident,
                                            matcher_spans: Some(
                                                (
                                                    span( : ),
                                                    span( ident ),
                                                ),
                                            ),
                                            span: span( $bar:ident ),
                                        },
                                    ],
                                    separator: None,
                                    count: ZeroOrMore (*),
                                    count_span: span( * ),
                                    span: span( $($bar:ident)* ),
                                },
                            ],
                            separator: None,
                            count: ZeroOrMore (*),
                            count_span: span( * ),
                            span: span( $($foo:ident $($bar:ident)*)* ),
                        },
                    ],
                    [
                        Repetition {
                            id: repetition 2,
                            dollar: span( $ ),
                            paren: span( ($($foo $bar)*) ),
                            stream: [
                                Repetition {
                                    id: repetition 3,
                                    dollar: span( $ ),
                                    paren: span( ($foo $bar) ),
                                    stream: [
                                        Metavariable {
                                            dollar: span( $ ),
                                            name: Ident {
                                                sym: "foo",
                                                span: span( foo ),
                                            },
                                            kind: Ident,
                                            matcher_spans: None,
                                            span: span( $foo ),
                                        },
                                        Metavariable {
                                            dollar: span( $ ),
                                            name: Ident {
                                                sym: "bar",
                                                span: span( bar ),
                                            },
                                            kind: Ident,
                                            matcher_spans: None,
                                            span: span( $bar ),
                                        },
                                    ],
                                    separator: None,
                                    count: ZeroOrMore (*),
                                    count_span: span( * ),
                                    span: span( $($foo $bar)* ),
                                },
                            ],
                            separator: None,
                            count: ZeroOrMore (*),
                            count_span: span( * ),
                            span: span( $($($foo $bar)*)* ),
                        },
                    ],
                    TokenStream [
                        Punct {
                            char: '$',
                            spacing: Alone,
                            span: bytes(1..2),
                        },
                        Group {
                            delimiter: Parenthesis,
                            stream: TokenStream [
                                Punct {
                                    char: '$',
                                    spacing: Alone,
                                    span: bytes(3..4),
                                },
                                Ident {
                                    sym: foo,
                                    span: bytes(4..7),
                                },
                                Punct {
                                    char: ':',
                                    spacing: Alone,
                                    span: bytes(7..8),
                                },
                                Ident {
                                    sym: ident,
                                    span: bytes(8..13),
                                },
                                Punct {
                                    char: '$',
                                    spacing: Alone,
                                    span: bytes(14..15),
                                },
                                Group {
                                    delimiter: Parenthesis,
                                    stream: TokenStream [
                                        Punct {
                                            char: '$',
                                            spacing: Alone,
                                            span: bytes(16..17),
                                        },
                                        Ident {
                                            sym: bar,
                                            span: bytes(17..20),
                                        },
                                        Punct {
                                            char: ':',
                                            spacing: Alone,
                                            span: bytes(20..21),
                                        },
                                        Ident {
                                            sym: ident,
                                            span: bytes(21..26),
                                        },
                                    ],
                                    span: bytes(15..27),
                                },
                                Punct {
                                    char: '*',
                                    spacing: Alone,
                                    span: bytes(27..28),
                                },
                            ],
                            span: bytes(2..29),
                        },
                        Punct {
                            char: '*',
                            spacing: Alone,
                            span: bytes(29..30),
                        },
                    ],
                    TokenStream [
                        Punct {
                            char: '$',
                            spacing: Alone,
                            span: bytes(31..32),
                        },
                        Group {
                            delimiter: Parenthesis,
                            stream: TokenStream [
                                Punct {
                                    char: '$',
                                    spacing: Alone,
                                    span: bytes(33..34),
                                },
                                Group {
                                    delimiter: Parenthesis,
                                    stream: TokenStream [
                                        Punct {
                                            char: '$',
                                            spacing: Alone,
                                            span: bytes(35..36),
                                        },
                                        Ident {
                                            sym: foo,
                                            span: bytes(36..39),
                                        },
                                        Punct {
                                            char: '$',
                                            spacing: Alone,
                                            span: bytes(40..41),
                                        },
                                        Ident {
                                            sym: bar,
                                            span: bytes(41..44),
                                        },
                                    ],
                                    span: bytes(34..45),
                                },
                                Punct {
                                    char: '*',
                                    spacing: Alone,
                                    span: bytes(45..46),
                                },
                            ],
                            span: bytes(32..47),
                        },
                        Punct {
                            char: '*',
                            spacing: Alone,
                            span: bytes(47..48),
                        },
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
                        Group {
                            delimiter: Bracket,
                            stream: [
                                Ident {
                                    sym: "bracket",
                                    span: span( bracket ),
                                },
                            ],
                            span: span( [bracket] ),
                        },
                        Group {
                            delimiter: Brace,
                            stream: [
                                Ident {
                                    sym: "brace",
                                    span: span( brace ),
                                },
                            ],
                            span: span( { brace } ),
                        },
                        Group {
                            delimiter: Parenthesis,
                            stream: [
                                Ident {
                                    sym: "parethensis",
                                    span: span( parethensis ),
                                },
                            ],
                            span: span( (parethensis) ),
                        },
                    ],
                    [
                        Group {
                            delimiter: Parenthesis,
                            stream: [
                                Ident {
                                    sym: "parenthesis",
                                    span: span( parenthesis ),
                                },
                            ],
                            span: span( (parenthesis) ),
                        },
                        Group {
                            delimiter: Brace,
                            stream: [
                                Ident {
                                    sym: "brace",
                                    span: span( brace ),
                                },
                            ],
                            span: span( { brace } ),
                        },
                        Group {
                            delimiter: Bracket,
                            stream: [
                                Ident {
                                    sym: "bracket",
                                    span: span( bracket ),
                                },
                            ],
                            span: span( [bracket] ),
                        },
                    ],
                    TokenStream [
                        Group {
                            delimiter: Bracket,
                            stream: TokenStream [
                                Ident {
                                    sym: bracket,
                                    span: bytes(2..9),
                                },
                            ],
                        },
                        Group {
                            delimiter: Brace,
                            stream: TokenStream [
                                Ident {
                                    sym: brace,
                                    span: bytes(13..18),
                                },
                            ],
                        },
                        Group {
                            delimiter: Parenthesis,
                            stream: TokenStream [
                                Ident {
                                    sym: parethensis,
                                    span: bytes(22..33),
                                },
                            ],
                        },
                    ],
                    TokenStream [
                        Group {
                            delimiter: Parenthesis,
                            stream: TokenStream [
                                Ident {
                                    sym: parenthesis,
                                    span: bytes(36..47),
                                },
                            ],
                        },
                        Group {
                            delimiter: Brace,
                            stream: TokenStream [
                                Ident {
                                    sym: brace,
                                    span: bytes(51..56),
                                },
                            ],
                        },
                        Group {
                            delimiter: Bracket,
                            stream: TokenStream [
                                Ident {
                                    sym: bracket,
                                    span: bytes(60..67),
                                },
                            ],
                        },
                    ],
                )
            "#]]
        }
    }

    test! {
        fn sasha_plz_fix_this_span() {
            (a) => { a },
            expect_test::expect![[r#"
                (
                    [
                        Ident {
                            sym: "a",
                            span: span( a ),
                        },
                    ],
                    [
                        Ident {
                            sym: "a",
                            span: span( a ),
                        },
                    ],
                    TokenStream [
                        Ident {
                            sym: a,
                            span: bytes(1..2),
                        },
                    ],
                    TokenStream [
                        Ident {
                            sym: a,
                            span: bytes(3..4),
                        },
                    ],
                )
            "#]],
        }
    }
}
