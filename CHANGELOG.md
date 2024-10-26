# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.2](https://github.com/scrabsha/expandable/compare/expandable-v0.1.1...expandable-v0.1.2) - 2024-10-26

### Fixed

- add `repository` field to Cargo.toml ([#79](https://github.com/scrabsha/expandable/pull/79))

## [0.1.1](https://github.com/scrabsha/expandable/compare/expandable-v0.1.0...expandable-v0.1.1) - 2024-10-20

### Added

- add release-plz worklow ([#75](https://github.com/scrabsha/expandable/pull/75))
- use the "vec! with -> instead of ." example in the doc ([#73](https://github.com/scrabsha/expandable/pull/73))
- add new background illustration ([#62](https://github.com/scrabsha/expandable/pull/62))
- match expressions ([#58](https://github.com/scrabsha/expandable/pull/58))
- add support for missing fragments ([#57](https://github.com/scrabsha/expandable/pull/57))
- add support for visibilty and struct definitions ([#55](https://github.com/scrabsha/expandable/pull/55))
- add missing [expandable::*] macros ([#53](https://github.com/scrabsha/expandable/pull/53))
- add macro call support ([#52](https://github.com/scrabsha/expandable/pull/52))
- add path parsing ([#47](https://github.com/scrabsha/expandable/pull/47))
- add (almost) full pattern support ([#44](https://github.com/scrabsha/expandable/pull/44))
- rework ordering in the "expected XXX, YYY, ZZZ" error message ([#45](https://github.com/scrabsha/expandable/pull/45))
- add support for index expression, loop, while and for loops  ([#43](https://github.com/scrabsha/expandable/pull/43))
- add support for tuple expressions and grouped expressions ([#42](https://github.com/scrabsha/expandable/pull/42))
- avoid printing a comically large amount of tokens on error ([#41](https://github.com/scrabsha/expandable/pull/41))
- add support for unqualified path expressions ([#40](https://github.com/scrabsha/expandable/pull/40))
- add a subset of the Rust language as grammar ([#35](https://github.com/scrabsha/expandable/pull/35))
- converge on transitions instead of converging on final state ([#34](https://github.com/scrabsha/expandable/pull/34))
- add support for logic and, or and range expressions ([#33](https://github.com/scrabsha/expandable/pull/33))
- sort expected tokens lexicographically before printing them ([#32](https://github.com/scrabsha/expandable/pull/32))
- Add parser DSL compiler code ([#30](https://github.com/scrabsha/expandable/pull/30))
- parse let statements in block expression and function body ([#28](https://github.com/scrabsha/expandable/pull/28))
- parse statements ([#27](https://github.com/scrabsha/expandable/pull/27))
- parse break and return expressions ([#26](https://github.com/scrabsha/expandable/pull/26))
- parse block expressions and const generics ([#24](https://github.com/scrabsha/expandable/pull/24))
- parse generics in function and method calls ([#23](https://github.com/scrabsha/expandable/pull/23))
- parse <expr> . <something> ([#22](https://github.com/scrabsha/expandable/pull/22))
- add support for array expressions ([#20](https://github.com/scrabsha/expandable/pull/20))
- parse all the tokens of the Rust language ([#16](https://github.com/scrabsha/expandable/pull/16))
- add arithmetic, bit and comparison expression parsing ([#14](https://github.com/scrabsha/expandable/pull/14))

### Fixed

- add link to docs, add missing readme link ([#82](https://github.com/scrabsha/expandable/pull/82))
- add the include field to all published crates ([#80](https://github.com/scrabsha/expandable/pull/80))
- handle newer rustdoc-json output ([#81](https://github.com/scrabsha/expandable/pull/81))
- set up workspace-level dependencies ([#78](https://github.com/scrabsha/expandable/pull/78))
- Add description and license field to the crates ([#77](https://github.com/scrabsha/expandable/pull/77))
- propagate "no repetitions" when checking 0-n repetitions ([#59](https://github.com/scrabsha/expandable/pull/59))
- honour newer rustc/clippy warnings ([#31](https://github.com/scrabsha/expandable/pull/31))
- add license file ([#19](https://github.com/scrabsha/expandable/pull/19))

### Other

- fix latest clippy lints ([#74](https://github.com/scrabsha/expandable/pull/74))
- update to the new rustfmt style ([#64](https://github.com/scrabsha/expandable/pull/64))
- add variable support in `rust-grammar-dpdfa`/`grammar-gen` ([#61](https://github.com/scrabsha/expandable/pull/61))
- add test for underscore expression ([#54](https://github.com/scrabsha/expandable/pull/54))
- bump MSRV to 1.70 ([#51](https://github.com/scrabsha/expandable/pull/51))
- check that generated.rs is up to date in the CI ([#38](https://github.com/scrabsha/expandable/pull/38))
- add rustfmt.toml and cool formatting settings ([#18](https://github.com/scrabsha/expandable/pull/18))
- add backend documentation ([#11](https://github.com/scrabsha/expandable/pull/11))
- add semantic.yml file ([#13](https://github.com/scrabsha/expandable/pull/13))
- Don't lower `=` to `EqualEqual` in the frontend ([#12](https://github.com/scrabsha/expandable/pull/12))
- Add subtraction, multiplication and equality, rework function arguments ([#10](https://github.com/scrabsha/expandable/pull/10))
- Add function call parsing ([#8](https://github.com/scrabsha/expandable/pull/8))
- Bring back the repetition stack check ([#7](https://github.com/scrabsha/expandable/pull/7))
- Improve the crate-level doc and readme ([#4](https://github.com/scrabsha/expandable/pull/4))
- Update README and crate docs to show 1.65 as MSRV, not 1.56 ([#5](https://github.com/scrabsha/expandable/pull/5))
- Add a MSRV policy ([#3](https://github.com/scrabsha/expandable/pull/3))
- Set 1.65 as MSRV in CI
- Only check repetition nesting ([#2](https://github.com/scrabsha/expandable/pull/2))
- Add parsing for if/if-else expressions ([#1](https://github.com/scrabsha/expandable/pull/1))
- find find -> find
- Format trybuild tests
- Check trybuild tests as well
- Force nigthtly clippy install
- Nightly?
- New cache (we need nightly :pleading_face:)
- CI?
- I forgot another part of the frontend :/
- Update trybuild oracles
- Make the test cuter
- I forgot the frontend :/
- State the architecture invariants
- Rework the grammar and transition machinery
- Fix alignment (2)
- Fix alignment
- Update root readme
- Make entry point macro expansion smaller
- Let's make this image funnier
- Improve doc
- Add top image
- Oops - I forgot to add the error message
- Report errors when the repetition nesting does not match
- Reduce our usage of syn
- Ok I actually missed a file
- Add README
- Actually there were more usages of `join`
- Don't rely too much on the `proc_macro_span` feature
- Le justfile
- Very bad stack machine -> full-blown stack machine
- Rework user-facing api, add top-level documentation
- Update tests
- Add tld doc (STILL NEED HUGE WORK)
- Remove warnings
- Rework tests
- Remove warnings
- Add test
- Better error message
- Add the proc_macro entry point
- Add "the whole point" mp4 file
- Fix Cargo.toml
- Documentation + plenty of smol fixes
- Error i guess
- Update test cases
- Make it slightly easier to transform TokenTreeKind -> TokenTree
- Spans everywhere
- Cleaning
- Documentation + remove useless tests
- Work
- Cool checkpoint
- I should have committed earlier :sweat_smile:
