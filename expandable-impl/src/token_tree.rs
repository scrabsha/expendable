use crate::FragmentKind;

#[derive(Debug)]
pub enum TokenTree {
    Ident(proc_macro2::Ident),
    Punct(proc_macro2::Punct),
    Literal(proc_macro2::Literal),
    Group(Group),
    Metavariable(proc_macro2::Literal),
    Repetition(Repetition),
}

#[derive(Debug)]
pub struct Group {
    pub content: Vec<TokenTree>,
    pub delimiter: proc_macro2::Delimiter,
    pub span: proc_macro2::Span,
}

#[derive(Debug)]
pub struct Repetition {
    pub content: Vec<TokenTree>,
    pub separator: Option<proc_macro2::Punct>,
    pub count: RepetitionCount,
    pub span: proc_macro2::Span,
}

#[derive(Debug)]
pub enum RepetitionCount {
    AtMostOne,
    ZeroOrMore,
    OneOrMore,
}

#[derive(Debug)]
pub struct Metavariable {
    pub name: proc_macro2::Literal,
    pub span: proc_macro2::Span,
}
