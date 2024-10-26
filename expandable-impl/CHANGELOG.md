# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.2](https://github.com/scrabsha/expandable/compare/expandable-impl-v0.1.1...expandable-impl-v0.1.2) - 2024-10-26

### Fixed

- add `repository` field to Cargo.toml ([#79](https://github.com/scrabsha/expandable/pull/79))

## [0.1.1](https://github.com/scrabsha/expandable/compare/expandable-impl-v0.1.0...expandable-impl-v0.1.1) - 2024-10-20

### Added

- add support for missing fragments ([#57](https://github.com/scrabsha/expandable/pull/57))
- allow grammar-gen functions to return atoms ([#56](https://github.com/scrabsha/expandable/pull/56))
- add missing [expandable::*] macros ([#53](https://github.com/scrabsha/expandable/pull/53))
- generate a possible invalid expansion when an error is found ([#49](https://github.com/scrabsha/expandable/pull/49))
- add path parsing ([#47](https://github.com/scrabsha/expandable/pull/47))
- add (almost) full pattern support ([#44](https://github.com/scrabsha/expandable/pull/44))
- rework ordering in the "expected XXX, YYY, ZZZ" error message ([#45](https://github.com/scrabsha/expandable/pull/45))
- add support for unqualified path expressions ([#40](https://github.com/scrabsha/expandable/pull/40))
- converge on transitions instead of converging on final state ([#34](https://github.com/scrabsha/expandable/pull/34))
- add support for logic and, or and range expressions ([#33](https://github.com/scrabsha/expandable/pull/33))
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

- symlink the doc directory in the expandable-impl crate root ([#83](https://github.com/scrabsha/expandable/pull/83))
- add link to docs, add missing readme link ([#82](https://github.com/scrabsha/expandable/pull/82))
- add the include field to all published crates ([#80](https://github.com/scrabsha/expandable/pull/80))
- set up workspace-level dependencies ([#78](https://github.com/scrabsha/expandable/pull/78))
- Add description and license field to the crates ([#77](https://github.com/scrabsha/expandable/pull/77))
- propagate "no repetitions" when checking 0-n repetitions ([#59](https://github.com/scrabsha/expandable/pull/59))
- honour newer rustc/clippy warnings ([#31](https://github.com/scrabsha/expandable/pull/31))
- add license file ([#19](https://github.com/scrabsha/expandable/pull/19))
- Add the missing AfterIf transitions ([#17](https://github.com/scrabsha/expandable/pull/17))

### Other

- fix latest clippy lints ([#74](https://github.com/scrabsha/expandable/pull/74))
- update to the new rustfmt style ([#64](https://github.com/scrabsha/expandable/pull/64))
- add variable support in `rust-grammar-dpdfa`/`grammar-gen` ([#61](https://github.com/scrabsha/expandable/pull/61))
- bump MSRV to 1.70 ([#51](https://github.com/scrabsha/expandable/pull/51))
- make the expansion check fully deterministic ([#50](https://github.com/scrabsha/expandable/pull/50))
- replace the state machine with the generated parser machinery ([#36](https://github.com/scrabsha/expandable/pull/36))
- add a transition inheritance system ([#21](https://github.com/scrabsha/expandable/pull/21))
- add rustfmt.toml and cool formatting settings ([#18](https://github.com/scrabsha/expandable/pull/18))
- compute the token descriptions before checking the expansion ([#15](https://github.com/scrabsha/expandable/pull/15))
- add backend documentation ([#11](https://github.com/scrabsha/expandable/pull/11))
- Don't lower `=` to `EqualEqual` in the frontend ([#12](https://github.com/scrabsha/expandable/pull/12))
- Add subtraction, multiplication and equality, rework function arguments ([#10](https://github.com/scrabsha/expandable/pull/10))
- Add function call parsing ([#8](https://github.com/scrabsha/expandable/pull/8))
- Fix an embarassing bug in the "stack size reduction" optimization ([#9](https://github.com/scrabsha/expandable/pull/9))
- Bring back the repetition stack check ([#7](https://github.com/scrabsha/expandable/pull/7))
- Add debug-only span ([#6](https://github.com/scrabsha/expandable/pull/6))
- Add a MSRV policy ([#3](https://github.com/scrabsha/expandable/pull/3))
- Only check repetition nesting ([#2](https://github.com/scrabsha/expandable/pull/2))
- Add parsing for if/if-else expressions ([#1](https://github.com/scrabsha/expandable/pull/1))
- Make FragmentKind public, fix public API on stable
- Make sure we don't accidentally keep some stack symbols
- I forgot the macros :/
- Add all the keywords in the Rust language
- State the architecture invariants
- Rework the grammar and transition machinery
- Remove intermediate test
- Report errors when the repetition nesting does not match
- Keep the repetition stack when parsing the macro matcher
- Very bad stack machine -> full-blown stack machine
- Remove warnings
- Gitigrone
- Check that macro expansion is complete
- Better error message
- Move the impl into a separate impl crate
