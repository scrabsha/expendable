#![allow(missing_docs)] // TODO: write docs

use proc_macro2::{Ident, Literal, Punct, Spacing, Span, TokenTree as GenericTokenTree};

use crate::FragmentKind;

#[derive(Debug)]
pub enum TokenTree {
    Ident(proc_macro2::Ident),
    Punct(proc_macro2::Punct),
    Literal(proc_macro2::Literal),
    Group(Group),
    Metavariable(Metavariable),
    Repetition(Repetition),
}

#[derive(Debug)]
pub struct Group {
    pub content: Vec<TokenTree>,
    pub delimiter: proc_macro2::Delimiter,
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

// TODO: make this private
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RepetitionId(pub(crate) usize);

#[derive(Debug)]
pub enum RepetitionCount {
    AtMostOne,
    ZeroOrMore,
    OneOrMore,
}

impl RepetitionCount {
    pub(crate) fn to_token_tree(&self, span: Span) -> GenericTokenTree {
        let ch = match self {
            RepetitionCount::AtMostOne => '?',
            RepetitionCount::ZeroOrMore => '*',
            RepetitionCount::OneOrMore => '+',
        };

        let mut punct = Punct::new(ch, Spacing::Alone);
        punct.set_span(span);

        punct.into()
    }
}

#[derive(Debug)]
pub struct Metavariable {
    pub name: proc_macro2::Ident,
    pub kind: FragmentKind,
    pub span: Span,
}
