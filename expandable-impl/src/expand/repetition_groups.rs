use crate::token_tree::{RepetitionId, Span, TokenTree};
use crate::Error;
use std::collections::BTreeMap;

#[derive(Debug)]
pub(super) struct RepetitionGroups {
    by_repetition: BTreeMap<RepetitionId, GroupId>,
}

impl RepetitionGroups {
    pub(super) fn new(
        matcher: &[TokenTree],
        transcriber: &[TokenTree],
    ) -> Result<Self, Error<Span>> {
        let mut grouper = Grouper {
            next_group_id: 0,
            repetitions_stack: Vec::new(),
            definitions: definitions(matcher),
            by_repetition: BTreeMap::new(),
            by_metavariable: BTreeMap::new(),
        };

        for token in matcher.iter().chain(transcriber.iter()) {
            grouper.ingest(token)?;
        }

        Ok(Self {
            by_repetition: grouper.by_repetition,
        })
    }

    #[expect(dead_code)]
    pub(super) fn group(&self, repetition: RepetitionId) -> GroupId {
        *self
            .by_repetition
            .get(&repetition)
            .expect("repetition was not grouped")
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub(super) struct GroupId(usize);

impl std::fmt::Debug for GroupId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "group {}", self.0)
    }
}

struct Grouper {
    next_group_id: usize,
    repetitions_stack: Vec<RepetitionId>,
    definitions: BTreeMap<String, Definition>,

    by_repetition: BTreeMap<RepetitionId, GroupId>,
    by_metavariable: BTreeMap<String, GroupId>,
}

impl Grouper {
    fn ingest(&mut self, token: &TokenTree) -> Result<(), Error<Span>> {
        match token {
            TokenTree::Ident(_) => {}
            TokenTree::Punct(_) => {}
            TokenTree::Literal(_) => {}
            TokenTree::Group(group) => {
                for token in &group.content {
                    self.ingest(token)?;
                }
            }
            TokenTree::Repetition(repetition) => {
                self.repetitions_stack.push(repetition.id);
                for token in &repetition.content {
                    self.ingest(token)?;
                }
                self.repetitions_stack.pop();

                if self.by_repetition.get(&repetition.id).is_none() {
                    return Err(Error::RepetitionWithoutMetavariables {
                        span: repetition.span,
                    });
                }
            }
            TokenTree::Metavariable(meta) => {
                let name = meta.name.to_string();
                let definition = self
                    .definitions
                    .get(&name)
                    .expect("metavariable in the trascriber is not defined in the matcher");

                let repetition = if definition.depth == 0 {
                    None
                } else if definition.depth > self.repetitions_stack.len() {
                    return Err(Error::MetavariableDefinedAtLowerDepth {
                        name,
                        definition_span: definition.span,
                        usage_span: meta.span,
                        definition_depth: definition.depth,
                        usage_depth: self.repetitions_stack.len(),
                    });
                } else {
                    Some(self.repetitions_stack[definition.depth - 1])
                };

                let existing_meta_group = self.by_metavariable.get(&name).copied();
                let existing_repetition_group =
                    repetition.and_then(|r| self.by_repetition.get(&r)).copied();

                let group = match (existing_meta_group, existing_repetition_group) {
                    (Some(group), None) | (None, Some(group)) => group,
                    (Some(lhs), Some(rhs)) => self.merge_groups(lhs, rhs),
                    (None, None) => self.new_group(),
                };

                self.by_metavariable.insert(name, group);
                if let Some(repetition) = repetition {
                    self.by_repetition.insert(repetition, group);
                }
            }
        }
        Ok(())
    }

    fn merge_groups(&mut self, lhs: GroupId, rhs: GroupId) -> GroupId {
        // To merge two groups, we are going to keep one of them (we arbitrarily choose lhs) and we
        // replace all occurences of the other group with the one we chose.

        for value in self
            .by_repetition
            .values_mut()
            .chain(self.by_metavariable.values_mut())
        {
            if *value == rhs {
                *value = lhs;
            }
        }
        lhs
    }

    fn new_group(&mut self) -> GroupId {
        let group = GroupId(self.next_group_id);
        self.next_group_id += 1;
        group
    }
}

fn definitions(matcher: &[TokenTree]) -> BTreeMap<String, Definition> {
    fn recurse(result: &mut BTreeMap<String, Definition>, depth: usize, token: &TokenTree) {
        match token {
            TokenTree::Ident(_) => {}
            TokenTree::Punct(_) => {}
            TokenTree::Literal(_) => {}
            TokenTree::Group(group) => {
                for token in &group.content {
                    recurse(result, depth, token);
                }
            }
            TokenTree::Metavariable(meta) => {
                let name = meta.name.to_string();

                // Note that this assumes another part of expandable will check whether the
                // metavariable names are unique. If you encounter this panic it means the rest of
                // expandable is not performing the check.
                assert!(
                    !result.contains_key(&name),
                    "duplicate metavariable name {name}"
                );

                result.insert(
                    name,
                    Definition {
                        span: meta.span,
                        depth,
                    },
                );
            }
            TokenTree::Repetition(repetition) => {
                for token in &repetition.content {
                    recurse(result, depth + 1, token);
                }
            }
        }
    }

    let mut result = BTreeMap::new();
    for token in matcher {
        recurse(&mut result, 0, token);
    }

    result
}

#[derive(Debug)]
struct Definition {
    span: Span,
    depth: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token_tree::ParseCtxt;
    use expect_test::{expect, Expect};
    use std::collections::HashMap;

    #[test]
    fn test_no_metavariables() {
        do_test_groups(
            "foo",
            "bar",
            expect!["<nothing>"],
        );
    }

    #[test]
    fn test_metavariable_outside_repetition() {
        do_test_groups(
            "$foo:ident",
            "$foo",
            expect!["<nothing>"],
        );
    }

    #[test]
    fn test_one_metavariable() {
        do_test_groups(
            "$($foo:ident),*",
            "$($foo),*",
            expect![[r#"
                span( ($foo:ident) ) => group 0
                span( ($foo) )       => group 0
            "#]],
        );
    }

    #[test]
    fn test_independent_repetitions() {
        do_test_groups(
            "$($foo:ident)* $($bar:ident)*",
            "$($foo)* $($bar)*",
            expect![[r#"
                span( ($foo:ident) ) => group 0
                span( ($bar:ident) ) => group 1
                span( ($foo) )       => group 0
                span( ($bar) )       => group 1
            "#]],
        );
    }

    #[test]
    fn test_two_repetitions_in_matcher_and_one_in_transcriber() {
        do_test_groups(
            "$($foo:ident)* $($bar:ident)*",
            "$($foo $bar)*",
            expect![[r#"
                span( ($foo:ident) ) => group 1
                span( ($bar:ident) ) => group 1
                span( ($foo $bar) )  => group 1
            "#]],
        );
    }

    #[test]
    fn test_one_repetition_in_matcher_and_two_in_transcriber() {
        do_test_groups(
            "$($foo:ident $bar:ident)*",
            "$($foo)* $($bar)*",
            expect![[r#"
                span( ($foo:ident $bar:ident) ) => group 0
                span( ($foo) )                  => group 0
                span( ($bar) )                  => group 0
            "#]],
        );
    }

    #[test]
    fn test_nested_repetitions() {
        do_test_groups(
            "$($foo:ident $($bar:ident)*)*",
            "$($foo $($bar)*)*",
            expect![[r#"
                span( ($foo:ident $($bar:ident)*) ) => group 0
                span( ($bar:ident) )                => group 1
                span( ($foo $($bar)*) )             => group 0
                span( ($bar) )                      => group 1
            "#]],
        );
    }

    #[test]
    fn test_nested_repetitions_using_variables_from_higher_depths() {
        do_test_groups(
            "$($foo:ident $($bar:ident)*)* $baz:ident",
            "$($($foo $bar $baz)*)*",
            expect![[r#"
                span( ($foo:ident $($bar:ident)*) ) => group 0
                span( ($bar:ident) )                => group 1
                span( ($($foo $bar $baz)*) )        => group 0
                span( ($foo $bar $baz) )            => group 1
            "#]],
        );
    }

    #[test]
    fn test_metavariable_at_lower_depth_doesnt_link_repetitions() {
        do_test_groups(
            "$foo:ident $($bar:ident)* $($baz:ident)*",
            "$($foo $bar)* $($foo $baz)*",
            expect![[r#"
                span( ($bar:ident) ) => group 1
                span( ($baz:ident) ) => group 2
                span( ($foo $bar) )  => group 1
                span( ($foo $baz) )  => group 2
            "#]],
        );
    }

    #[test]
    fn test_using_metavariable_of_lower_depth_than_repetition() {
        do_test_groups(
            "$($foo:ident)*",
            "$foo",
            expect![[r#"
                Err(
                    MetavariableDefinedAtLowerDepth {
                        name: "foo",
                        definition_span: span( $foo:ident ),
                        usage_span: span( $foo ),
                        definition_depth: 1,
                        usage_depth: 0,
                    },
                )
            "#]],
        );
    }

    #[test]
    fn test_repetition_without_metavariables() {
        do_test_groups(
            "",
            "$(1,)*",
            expect![[r#"
                Err(
                    RepetitionWithoutMetavariables {
                        span: span( (1,) ),
                    },
                )
            "#]],
        );
    }

    #[test]
    fn test_definitions() {
        do_test_definitions(
            "foo $bar:ident [ foo $($foo:ident $baz:ident $($quux:ident),*)* ]",
            expect![[r#"
                {
                    "bar": Definition {
                        span: span( $bar:ident ),
                        depth: 0,
                    },
                    "baz": Definition {
                        span: span( $baz:ident ),
                        depth: 1,
                    },
                    "foo": Definition {
                        span: span( $foo:ident ),
                        depth: 1,
                    },
                    "quux": Definition {
                        span: span( $quux:ident ),
                        depth: 2,
                    },
                }
            "#]],
        );
    }

    fn do_test_groups(matcher: &str, transcriber: &str, expect: Expect) {
        let mut ctx = ParseCtxt::matcher();
        let matcher = TokenTree::from_generic(&mut ctx, matcher.parse().unwrap()).unwrap();
        ctx.turn_into_transcriber();
        let transcriber = TokenTree::from_generic(&mut ctx, transcriber.parse().unwrap()).unwrap();

        let mut repetition_spans = HashMap::new();
        for token in matcher.iter().chain(transcriber.iter()) {
            find_repetition_spans(&mut repetition_spans, token);
        }

        let groups = match RepetitionGroups::new(&matcher, &transcriber) {
            Ok(groups) => groups,
            err @ Err(_) => {
                expect.assert_debug_eq(&err);
                return;
            }
        };

        let lines = groups
            .by_repetition
            .iter()
            .map(|(repetition, group)| {
                (
                    format!("{:?}", repetition_spans.get(repetition).unwrap()),
                    format!("{group:?}"),
                )
            })
            .collect::<Vec<_>>();
        let first_column_len = lines
            .iter()
            .map(|(repetition, _)| repetition.len())
            .max()
            .unwrap_or(0);

        let mut assertable = String::new();
        for (repetition, group) in lines {
            assertable.push_str(&format!("{repetition:first_column_len$} => {group}\n"));
        }

        if assertable.is_empty() {
            assertable = "<nothing>".into();
        }

        expect.assert_eq(&assertable);
    }

    fn do_test_definitions(input: &str, expect: Expect) {
        let mut ctx = ParseCtxt::matcher();
        let tokens = TokenTree::from_generic(&mut ctx, input.parse().unwrap()).unwrap();
        expect.assert_debug_eq(&definitions(&tokens));
    }

    fn find_repetition_spans(spans: &mut HashMap<RepetitionId, Span>, token: &TokenTree) {
        match token {
            TokenTree::Ident(_)
            | TokenTree::Punct(_)
            | TokenTree::Literal(_)
            | TokenTree::Metavariable(_) => {}
            TokenTree::Group(group) => {
                for token in &group.content {
                    find_repetition_spans(spans, token);
                }
            }
            TokenTree::Repetition(repetition) => {
                spans.insert(repetition.id, repetition.span);
                for token in &repetition.content {
                    find_repetition_spans(spans, token);
                }
            }
        }
    }
}
