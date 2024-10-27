//! Create groups of repetitions that repeat the same number of times.
//!
//! When expanding a macro, if a metavariable is repeated `n` times, *all* repetitions containing
//! it will repeat exactly `n` times.
//!
//! This is transitive: if you have two metavariables `$a` and `$b`, `$a` repeats `n` times, and
//! there is a repetition referencing both `$a` and `$b`, `$b` will also repeat `n` times.
//!
//! Knowing this allows to greatly constrain the amount of expansions `expandable-impl` has to
//! check. For the transcriber `$($a $b)* $($a)* $($b)*`, this module shows that if the first
//! repetition is repeated `n` times all other repetitions will only repeat exactly `n` times.
//!
//! Note that the grouping can only happen at the same repetition depth: a metavariable defined at
//! depth 1 cannot be grouped with a metavariable defined at depth 2, even if they are both used
//! inside of the same repetition. This is because a metavariable affect how many times the
//! repetition at the metavariable *definition* depth is repeated, not how many times the
//! repetition at the metavariable *usage* depth is repeated.

// TODO: vocabulary - lower and higher depth is confusing

use std::collections::BTreeMap;

use crate::{
    Error,
    token_tree::{RepetitionId, Span, TokenTree},
};

#[derive(Debug)]
pub(super) struct RepetitionGroups {
    by_repetition: BTreeMap<RepetitionId, GroupId>,
}

impl RepetitionGroups {
    pub(super) fn new(matcher: &[TokenTree], transcriber: &[TokenTree]) -> Result<Self, Error> {
        let analysis = Analysis::gather(matcher, transcriber);
        let mut grouper = Grouper {
            next_group_id: 0,
            repetitions_stack: Vec::new(),
            analysis,
            by_repetition: BTreeMap::new(),
            by_metavar: BTreeMap::new(),
        };

        // We need to ingest repetitions and metavariables from the matcher and the
        // transcriber, but there is no practical difference in which of the two
        // the repetition is defined in. We can just concatenate them together.
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
    analysis: Analysis,

    by_repetition: BTreeMap<RepetitionId, GroupId>,
    by_metavar: BTreeMap<Metavar, GroupId>,
}

impl Grouper {
    fn ingest(&mut self, token: &TokenTree) -> Result<(), Error> {
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

                // TODO: explain what this is.
                for metavar in self
                    .analysis
                    .ghosts
                    .remove(&repetition.id)
                    .expect("no ghost entry for repetition")
                {
                    self.ingest_metavar(metavar, Some(repetition.id));
                }

                self.repetitions_stack.pop();

                // Check whether there were no metavariables attached to this repetition.
                if !self.by_repetition.contains_key(&repetition.id) {
                    return Err(Error::RepetitionWithoutMetavariables {
                        span: repetition.span,
                    });
                }
            }

            TokenTree::Metavariable(meta) => {
                let name = meta.name.to_string();
                let definition = self
                    .analysis
                    .metavars
                    .get(&name)
                    .expect("metavariable in the transcriber is not defined in the matcher");

                let repetition = if definition.depth == 0 {
                    // Not defined inside of a repetition.
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
                    // We consider the repetition at the depth the metavariable is defined in, not
                    // the depth of the repetition we're currently processing.
                    //
                    // When a repetition refers to a metavariable defined at a higher depth, that
                    // metavariable doesn't influence how many times the current repetition is
                    // repeated. We should thus not group the metavariable with the repetition.
                    //
                    // Still, we can't just ignore the metavariable if it's not defined at the same
                    // depth, because that metavariable influences how much the repetition *at its
                    // definition depth* is repeated. We should then group the metavariable with
                    // the repetition at its definition depth.
                    //
                    // Let's take this code for example:
                    //
                    // ```rust
                    // macro_rules! example {
                    //     ($($first:ident $($second:ident),*),*) => {
                    //         $($($first, $second),*),*
                    //     }
                    // }
                    // ```
                    //
                    // In the example, $first should be grouped with the outer repetition (as it
                    // affects how much times it's repeated), while $second should be grouped with
                    // the inner repetition (for the same reason), and there is no correlation for
                    // how many times $first and $second repeat.
                    Some(self.repetitions_stack[definition.depth - 1])
                };

                self.ingest_metavar(Metavar::Real(name), repetition);
            }
        }
        Ok(())
    }

    fn ingest_metavar(&mut self, var: Metavar, repetition: Option<RepetitionId>) {
        // This can be `None` if either we are inside of a repetition and there is no
        // group assigned to it *yet*, or if we are outside of a
        // repetition.
        let existing_repetition_group = match &repetition {
            Some(id) => self.by_repetition.get(id).copied(),
            None => None,
        };
        let existing_metavar_group = self.by_metavar.get(&var).copied();

        let group = match (existing_metavar_group, existing_repetition_group) {
            (Some(group), None) | (None, Some(group)) => group,
            (Some(lhs), Some(rhs)) => self.merge_groups(lhs, rhs),
            (None, None) => self.new_group(),
        };

        self.by_metavar.insert(var, group);
        if let Some(repetition) = repetition {
            self.by_repetition.insert(repetition, group);
        }
    }

    fn merge_groups(&mut self, lhs: GroupId, rhs: GroupId) -> GroupId {
        // To merge two groups, we are going to keep one of them (we arbitrarily choose
        // lhs) and we replace all occurences of the other group with the one we
        // chose.
        if lhs != rhs {
            for value in self
                .by_repetition
                .values_mut()
                .chain(self.by_metavar.values_mut())
            {
                if *value == rhs {
                    *value = lhs;
                }
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Metavar {
    Real(String),
    Ghost(usize),
}

impl std::fmt::Debug for Metavar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Real(name) => write!(f, "${name}"),
            Self::Ghost(idx) => write!(f, "ghost({idx})"),
        }
    }
}

#[derive(Debug)]
struct Analysis {
    metavars: BTreeMap<String, MetavarAnalysis>,
    ghosts: BTreeMap<RepetitionId, Vec<Metavar>>,
    next_ghost: usize,
}

impl Analysis {
    fn gather(matcher: &[TokenTree], transcriber: &[TokenTree]) -> Self {
        let mut this = Self {
            metavars: BTreeMap::new(),
            ghosts: BTreeMap::new(),
            next_ghost: 0,
        };

        for token in matcher {
            this.collect_metavars(0, token);
        }

        for token in matcher.iter().chain(transcriber.iter()) {
            this.distribute_ghosts(0, token);
        }

        this
    }

    fn collect_metavars(&mut self, depth: usize, token: &TokenTree) {
        match token {
            TokenTree::Ident(_) => {}
            TokenTree::Punct(_) => {}
            TokenTree::Literal(_) => {}
            TokenTree::Group(group) => {
                for token in &group.content {
                    self.collect_metavars(depth, token);
                }
            }
            TokenTree::Metavariable(meta) => {
                let name = meta.name.to_string();

                // Note that this assumes another part of `expandable-impl` will check whether the
                // metavariable names are unique. If you encounter this panic it means the rest of
                // `expandable-impl` is not performing the check.
                assert!(
                    !self.metavars.contains_key(&name),
                    "duplicate metavariable name {name}"
                );

                // TODO: explain what this is
                let mut ghosts = BTreeMap::new();
                for ghost_depth in 1..depth {
                    ghosts.insert(ghost_depth, Metavar::Ghost(self.next_ghost));
                    self.next_ghost += 1;
                }

                self.metavars.insert(name, MetavarAnalysis {
                    span: meta.span,
                    depth,
                    ghosts,
                });
            }
            TokenTree::Repetition(repetition) => {
                for token in &repetition.content {
                    self.collect_metavars(depth + 1, token);
                }
            }
        }
    }

    fn distribute_ghosts(&mut self, depth: usize, token: &TokenTree) -> Vec<String> {
        let mut nested_metavars = Vec::new();
        match token {
            TokenTree::Ident(_) | TokenTree::Punct(_) | TokenTree::Literal(_) => {}
            TokenTree::Metavariable(meta) => nested_metavars.push(meta.name.to_string()),
            TokenTree::Group(group) => {
                nested_metavars.extend(
                    group
                        .content
                        .iter()
                        .flat_map(|token| self.distribute_ghosts(depth, token)),
                );
            }
            TokenTree::Repetition(repetition) => {
                nested_metavars.extend(
                    repetition
                        .content
                        .iter()
                        .flat_map(|token| self.distribute_ghosts(depth + 1, token)),
                );
                self.ghosts.insert(
                    repetition.id,
                    nested_metavars
                        .iter()
                        .flat_map(|metavar| {
                            self.metavars
                                .get(metavar)
                                .expect("metavar not in the definitions")
                                .ghosts
                                .iter()
                                .filter(|(ghost_depth, _)| **ghost_depth == depth + 1)
                                .map(|(_, ghost)| ghost.clone())
                        })
                        .collect(),
                );
            }
        }
        nested_metavars
    }
}

#[derive(Debug)]
struct MetavarAnalysis {
    span: Span,
    depth: usize,
    ghosts: BTreeMap<usize, Metavar>,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use expect_test::{Expect, expect};

    use super::*;
    use crate::token_tree::ParseCtxt;

    #[test]
    fn test_no_metavariables() {
        do_test_groups("foo", "bar", expect!["<nothing>"]);
    }

    #[test]
    fn test_metavariable_outside_repetition() {
        do_test_groups("$foo:ident", "$foo", expect!["<nothing>"]);
    }

    #[test]
    fn test_one_metavariable() {
        do_test_groups("$($foo:ident),*", "$($foo),*", expect![[r#"
                span( $($foo:ident),* ) => group 0
                span( $($foo),* )       => group 0
            "#]]);
    }

    #[test]
    fn test_independent_repetitions() {
        do_test_groups(
            "$($foo:ident)* $($bar:ident)*",
            "$($foo)* $($bar)*",
            expect![[r#"
                span( $($foo:ident)* ) => group 0
                span( $($bar:ident)* ) => group 1
                span( $($foo)* )       => group 0
                span( $($bar)* )       => group 1
            "#]],
        );
    }

    #[test]
    fn test_two_repetitions_in_matcher_and_one_in_transcriber() {
        do_test_groups("$($foo:ident)* $($bar:ident)*", "$($foo $bar)*", expect![[
            r#"
                span( $($foo:ident)* ) => group 1
                span( $($bar:ident)* ) => group 1
                span( $($foo $bar)* )  => group 1
            "#
        ]]);
    }

    #[test]
    fn test_one_repetition_in_matcher_and_two_in_transcriber() {
        do_test_groups("$($foo:ident $bar:ident)*", "$($foo)* $($bar)*", expect![[
            r#"
                span( $($foo:ident $bar:ident)* ) => group 0
                span( $($foo)* )                  => group 0
                span( $($bar)* )                  => group 0
            "#
        ]]);
    }

    #[test]
    fn test_nested_repetitions() {
        do_test_groups(
            "$($foo:ident $($bar:ident)*)*",
            "$($foo $($bar)*)*",
            expect![[r#"
                span( $($foo:ident $($bar:ident)*)* ) => group 0
                span( $($bar:ident)* )                => group 1
                span( $($foo $($bar)*)* )             => group 0
                span( $($bar)* )                      => group 1
            "#]],
        );
    }

    #[test]
    fn test_nested_repetitions_using_variables_from_higher_depths() {
        do_test_groups(
            "$($foo:ident $($bar:ident)*)* $baz:ident",
            "$($($foo $bar $baz)*)*",
            expect![[r#"
                span( $($foo:ident $($bar:ident)*)* ) => group 0
                span( $($bar:ident)* )                => group 1
                span( $($($foo $bar $baz)*)* )        => group 0
                span( $($foo $bar $baz)* )            => group 1
            "#]],
        );
    }

    #[test]
    fn test_metavariable_at_lower_depth_doesnt_link_repetitions() {
        do_test_groups(
            "$foo:ident $($bar:ident)* $($baz:ident)*",
            "$($foo $bar)* $($foo $baz)*",
            expect![[r#"
                span( $($bar:ident)* ) => group 1
                span( $($baz:ident)* ) => group 2
                span( $($foo $bar)* )  => group 1
                span( $($foo $baz)* )  => group 2
            "#]],
        );
    }

    #[test]
    fn test_using_metavariable_of_lower_depth_than_repetition() {
        do_test_groups("$($foo:ident)*", "$foo", expect![[r#"
            Err(
                MetavariableDefinedAtLowerDepth {
                    name: "foo",
                    definition_span: span( $foo:ident ),
                    usage_span: span( $foo ),
                    definition_depth: 1,
                    usage_depth: 0,
                },
            )
        "#]]);
    }

    #[test]
    fn test_repetition_with_metavariable_inside_repetition_without_one() {
        do_test_groups("$($($a:ident)*)*", "$($($a)*)* $($($a)*)*", expect! {[r#"
            span( $($($a:ident)*)* ) => group 1
            span( $($a:ident)* )     => group 0
            span( $($($a)*)* )       => group 1
            span( $($a)* )           => group 0
            span( $($($a)*)* )       => group 1
            span( $($a)* )           => group 0
        "#]});
    }

    #[test]
    fn test_repetition_with_metavariable_inside_repetition_without_one_inside_repetition_without_metavariables()
     {
        // TODO: find a better name for this test lmao
        // TODO: it points to the wrong span???
        do_test_groups("$($($a:ident)*)*", "$($($($a)*)*)*", expect! {[r#"
            Err(
                RepetitionWithoutMetavariables {
                    span: span( $($a)* ),
                },
            )
        "#]});
    }

    #[test]
    fn test_repetition_without_metavariables() {
        do_test_groups("", "$(1,)*", expect![[r#"
            Err(
                RepetitionWithoutMetavariables {
                    span: span( $(1,)* ),
                },
            )
        "#]]);
    }

    #[test]
    fn test_ghost_metavars_for_repetitions() {
        // TODO: better rendering for this
        do_test_analysis(
            "foo $($($a:ident)* $($b:ident $($c:ident)* bar)*)* $($d:ident $($e:ident baz)*)*",
            "$($($a)* $($b $($c)*)*)* $($d $($e)*)*",
            expect![[r#"
                Analysis {
                    metavars: {
                        "a": MetavarAnalysis {
                            span: span( $a:ident ),
                            depth: 2,
                            ghosts: {
                                1: ghost(0),
                            },
                        },
                        "b": MetavarAnalysis {
                            span: span( $b:ident ),
                            depth: 2,
                            ghosts: {
                                1: ghost(1),
                            },
                        },
                        "c": MetavarAnalysis {
                            span: span( $c:ident ),
                            depth: 3,
                            ghosts: {
                                1: ghost(2),
                                2: ghost(3),
                            },
                        },
                        "d": MetavarAnalysis {
                            span: span( $d:ident ),
                            depth: 1,
                            ghosts: {},
                        },
                        "e": MetavarAnalysis {
                            span: span( $e:ident ),
                            depth: 2,
                            ghosts: {
                                1: ghost(4),
                            },
                        },
                    },
                    ghosts: {
                        repetition 0: [
                            ghost(0),
                            ghost(1),
                            ghost(2),
                        ],
                        repetition 1: [],
                        repetition 2: [
                            ghost(3),
                        ],
                        repetition 3: [],
                        repetition 4: [
                            ghost(4),
                        ],
                        repetition 5: [],
                        repetition 6: [
                            ghost(0),
                            ghost(1),
                            ghost(2),
                        ],
                        repetition 7: [],
                        repetition 8: [
                            ghost(3),
                        ],
                        repetition 9: [],
                        repetition 10: [
                            ghost(4),
                        ],
                        repetition 11: [],
                    },
                    next_ghost: 5,
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
                    format!("{:?}", &repetition_spans[repetition]),
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

    fn do_test_analysis(matcher: &str, transcriber: &str, expect: Expect) {
        let mut ctx = ParseCtxt::matcher();
        let matcher = TokenTree::from_generic(&mut ctx, matcher.parse().unwrap()).unwrap();
        ctx.turn_into_transcriber();
        let transcriber = TokenTree::from_generic(&mut ctx, transcriber.parse().unwrap()).unwrap();

        let analysis = Analysis::gather(&matcher, &transcriber);
        expect.assert_debug_eq(&analysis);
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
