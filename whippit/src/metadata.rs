//! Contains data structures and logic for constructing them by parsing from . The main entry
//! point for this module is the [`File`] API.
//!
//! Documentation for types in this module refer liberally to the [WPT metadata format],
//! particularly the [`Web-Platform-Tests Metadata` section]. It is recommended that you
//! familiarize yourself with this document, if you plan to use this API.
//!
//! [WPT metadata format]: https://web-platform-tests.org/tools/wptrunner/docs/expectation.html#metadata-format
//! [`Web-Platform-Tests Metadata` section]: https://web-platform-tests.org/tools/wptrunner/docs/expectation.html#web-platform-tests-metadata

#[cfg(test)]
use {
    crate::metadata::properties::unstructured::{UnstructuredFile, UnstructuredSubtest},
    insta::assert_debug_snapshot,
};

use std::fmt::{self, Debug, Display};

use chumsky::{
    extra::Full,
    input::Emitter,
    prelude::Rich,
    primitive::{any, choice, custom, end, group, just},
    span::SimpleSpan,
    text::newline,
    IterParser, Parser,
};
use format::lazy_format;

use self::properties::{Properties, PropertiesParseHelper};

pub mod properties;

/// An error emitted by [`File::parser`] and [other WPT metadata parsing logic][self].
pub type ParseError<'a> = Full<Rich<'a, char>, (), ()>;

/// Behavior that needs to be defined to parse [the WPT metadata format][self] with
/// [`file_parser`].
///
/// If you don't have any opinions for an implementation of your own, you can use the
/// [`properties::unstructured`] API (gated behind the `unstructured` feature) for minimally
/// structured data that is maximally permissive in its parsing.
///
/// N.B. that you should not rely solely on data represented in this structure to compute test
/// metadata for test execution. It is _not_ complete by itself, because WPT metadata properties
/// can be layered across multiple sources (i.e., [`__dir__.ini`] files exist).
///
/// [`__dir__.ini`]: https://web-platform-tests.org/tools/wptrunner/docs/expectation.html#directory-metadata
pub trait File<'a>
where
    Self: Default,
{
    type Test: Test<'a>;

    fn add_test(
        &mut self,
        name: SectionHeader,
        test: Self::Test,
        span: SimpleSpan,
        emitter: &mut Emitter<Rich<'a, char>>,
    );
}

/// Returns a parser for a single [`File`] written in the [WPT metadata format][self].
///
/// No attempt is made to reconcile the provided string with additional layers of
/// configuration. See [`File`] for more details.
pub fn file_parser<'a, F>() -> impl Parser<'a, &'a str, F, ParseError<'a>>
where
    F: File<'a>,
{
    filler()
        .ignore_then(test_parser())
        .then_ignore(filler())
        .map_with(|test, e| (e.span(), test))
        .repeated()
        .collect::<Vec<_>>()
        .validate(|tests, _e, emitter| {
            let mut file = F::default();
            for (span, (name, test)) in tests {
                file.add_test(name, test, span, emitter)
            }
            file
        })
}

fn filler<'a>() -> impl Parser<'a, &'a str, (), ParseError<'a>> {
    choice((comment(0).ignored(), newline())).repeated()
}

#[test]
fn smoke_parser() {
    let file_parser = UnstructuredFile::parser;

    assert_debug_snapshot!(
        file_parser().parse(""),
        @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {},
            },
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(file_parser().parse("[hoot]"), @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "hoot": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 0..6,
                    },
                },
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(file_parser().parse("[blarg]\n"), @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "blarg": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 0..8,
                    },
                },
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(file_parser().parse("[blarg]\n[stuff]"), @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "blarg": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 0..8,
                    },
                    "stuff": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 8..15,
                    },
                },
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(file_parser().parse("\n[blarg]\n[stuff]\n"), @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "blarg": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 1..9,
                    },
                    "stuff": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 9..17,
                    },
                },
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(file_parser().parse("\n[blarg]\n\n[stuff]\n"), @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "blarg": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 1..10,
                    },
                    "stuff": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 10..18,
                    },
                },
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(file_parser().parse("\n[blarg]\n  expected: PASS\n[stuff]\n"), @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "blarg": UnstructuredTest {
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {},
                        span: 1..26,
                    },
                    "stuff": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 26..34,
                    },
                },
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(
        file_parser().parse(r#"
[blarg]
  expected: PASS
[stuff]
"#),
        @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "blarg": UnstructuredTest {
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {},
                        span: 1..26,
                    },
                    "stuff": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 26..34,
                    },
                },
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(
        file_parser().parse(r#"
[blarg]
  expected: PASS
  # Below is wrong: indentation is off!
    [stuff]
      expected: TIMEOUT
"#),
        @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 66..67 expected "test section header",
        ],
    }
    "###);

    assert_debug_snapshot!(
        file_parser().parse(r#"
[blarg]
  expected: PASS
  [stuff]
    expected: TIMEOUT
"#),
        @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "blarg": UnstructuredTest {
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {
                            "stuff": UnstructuredSubtest {
                                properties: {
                                    "expected": Unconditional(
                                        "TIMEOUT",
                                    ),
                                },
                                span: 36..58,
                            },
                        },
                        span: 1..58,
                    },
                },
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(
        file_parser().parse(
r#"
[asdf]
  [blarg]
    expected:
      if os == "linux": FAIL
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "asdf": UnstructuredTest {
                        properties: {},
                        subtests: {
                            "blarg": UnstructuredSubtest {
                                properties: {
                                    "expected": Conditional(
                                        ConditionalValue {
                                            conditions: [
                                                (
                                                    Eq(
                                                        Value(
                                                            Variable(
                                                                "os",
                                                            ),
                                                        ),
                                                        Value(
                                                            Literal(
                                                                String(
                                                                    "linux",
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                    "FAIL",
                                                ),
                                            ],
                                            fallback: None,
                                        },
                                    ),
                                },
                                span: 18..61,
                            },
                        },
                        span: 1..61,
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
    file_parser().parse(
r#"
[asdf]
  expected: PASS
  [blarg]
    expected: PASS
"#
    ),
    @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "asdf": UnstructuredTest {
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {
                            "blarg": UnstructuredSubtest {
                                properties: {
                                    "expected": Unconditional(
                                        "PASS",
                                    ),
                                },
                                span: 35..54,
                            },
                        },
                        span: 1..54,
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );

    let parser = newline().ignore_then(file_parser());

    assert_debug_snapshot!(
        parser.parse(r#"
[asdf]
  expected: PASS
"#),
    @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "asdf": UnstructuredTest {
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {},
                        span: 1..25,
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );
}

/// A single first-level section in a [`File`].
///
/// See [`File`] for more details for the human-readable format this corresponds to.
pub trait Test<'a> {
    type Properties: Properties<'a>;
    type Subtests: Subtests<'a>;

    fn new(span: SimpleSpan, properties: Self::Properties, subtests: Self::Subtests) -> Self;
}

pub fn test_parser<'a, T>() -> impl Parser<'a, &'a str, (SectionHeader, T), ParseError<'a>>
where
    T: Test<'a>,
{
    #[derive(Debug)]
    enum Item<Tp, S> {
        Subtest { name: SectionHeader, subtest: S },
        Property(Tp),
        Newline,
        Comment,
    }

    let items = choice((
        subtest_parser().map(|(name, subtest)| Item::Subtest { name, subtest }),
        T::Properties::property_parser(&mut PropertiesParseHelper::new(1))
            .labelled("test property")
            .map(Item::Property),
        newline().labelled("empty line").map(|()| Item::Newline),
        comment(1).map(|_comment| Item::Comment),
    ))
    .map_with(|item, e| (e.span(), item))
    .repeated()
    .collect::<Vec<_>>();

    let test_header = SectionHeader::parser(0)
        .then_ignore(newline().or(end()))
        .labelled("test section header");

    test_header
        .then(items)
        .validate(|(name, items), e, emitter| {
            let mut properties = T::Properties::default();
            let mut subtests = T::Subtests::default();
            for (span, item) in items {
                match item {
                    Item::Property(prop) => properties.add_property(prop, emitter),
                    Item::Subtest { name, subtest } => {
                        subtests.add_subtest(name, subtest, span, emitter)
                    }
                    Item::Newline | Item::Comment => (),
                }
            }
            let test = T::new(e.span(), properties, subtests);
            (name, test)
        })
}

pub trait Subtests<'a>
where
    Self: Default,
{
    type Subtest: Subtest<'a>;

    fn add_subtest(
        &mut self,
        name: SectionHeader,
        subtest: Self::Subtest,
        span: SimpleSpan,
        emitter: &mut Emitter<Rich<'a, char>>,
    );
}

/// A single second-level section in a [`File`] underneath a [`Test`].
///
/// See [`File`] for more details for the human-readable format this corresponds to.
pub trait Subtest<'a> {
    type Properties: Properties<'a>;

    fn new(span: SimpleSpan, properties: Self::Properties) -> Self;
}

fn comment<'a>(indentation: u8) -> impl Parser<'a, &'a str, &'a str, ParseError<'a>> {
    group((indent(indentation), just('#'), just(' ').or_not()))
        .ignore_then(any().and_is(newline().not()).repeated().to_slice())
        .then_ignore(choice((newline(), end())))
        .labelled("comment")
}

#[test]
fn smoke_comment() {
    assert_debug_snapshot!(comment(0).parse("asdf"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''a'' at 0..1 expected ''#'',
        ],
    }
    "###);
    assert_debug_snapshot!(comment(0).parse("# asdf"), @r###"
    ParseResult {
        output: Some(
            "asdf",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(comment(0).parse("# "), @r###"
    ParseResult {
        output: Some(
            "",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(comment(0).parse("#"), @r###"
    ParseResult {
        output: Some(
            "",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(
        comment(0).parse("# asdf # blarg"),
        @r###"
    ParseResult {
        output: Some(
            "asdf # blarg",
        ),
        errs: [],
    }
    "###
    );
    assert_debug_snapshot!(comment(0).parse(" # asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 0..1 expected "comment",
        ],
    }
    "###);
    assert_debug_snapshot!(comment(0).parse("  # asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 0..1 expected "comment",
        ],
    }
    "###);
    assert_debug_snapshot!(comment(1).parse("    # asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 2..3 expected something else,
        ],
    }
    "###);
    assert_debug_snapshot!(comment(1).parse("   # asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 2..3 expected something else,
        ],
    }
    "###);
    assert_debug_snapshot!(comment(1).parse(" # asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''#'' at 1..2 expected "comment",
        ],
    }
    "###);
    assert_debug_snapshot!(comment(1).parse("# asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''#'' at 0..1 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(comment(2).parse("      # asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 4..5 expected something else,
        ],
    }
    "###);
    assert_debug_snapshot!(comment(2).parse("     # asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 4..5 expected something else,
        ],
    }
    "###);
    assert_debug_snapshot!(comment(2).parse("    # asdf # blarg"), @r###"
    ParseResult {
        output: Some(
            "asdf # blarg",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(comment(2).parse("   # asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''#'' at 3..4 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(comment(2).parse(" # asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''#'' at 1..2 expected "comment",
        ],
    }
    "###);
    assert_debug_snapshot!(comment(2).parse("# asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''#'' at 0..1 expected '' '',
        ],
    }
    "###);
}

#[test]
fn smoke_test() {
    let test = UnstructuredFile::parser;

    assert_debug_snapshot!(
        test().parse("[stuff and things]\n"),
        @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "stuff and things": UnstructuredTest {
                        properties: {},
                        subtests: {},
                        span: 0..19,
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );
    assert_debug_snapshot!(
        test()
            .parse("[stuff and things]\n  expected: PASS\n"),
        @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "stuff and things": UnstructuredTest {
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {},
                        span: 0..36,
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );

    let test = || newline().ignore_then(test());

    assert_debug_snapshot!(
        test().parse(r#"
[stuff and things]
  expected: PASS
"#),
        @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "stuff and things": UnstructuredTest {
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {},
                        span: 1..37,
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        test().parse(r#"
[stuff and things]
  expected:
    if thing: boo
    yay
"#),
        @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "stuff and things": UnstructuredTest {
                        properties: {
                            "expected": Conditional(
                                ConditionalValue {
                                    conditions: [
                                        (
                                            Value(
                                                Variable(
                                                    "thing",
                                                ),
                                            ),
                                            "boo",
                                        ),
                                    ],
                                    fallback: Some(
                                        "yay",
                                    ),
                                },
                            ),
                        },
                        subtests: {},
                        span: 1..58,
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        test().parse(r#"
[cts.https.html?q=webgpu:api,operation,adapter,requestAdapter:requestAdapter_no_parameters:*]
  [:]
    expected:
      if os == "mac": FAIL


"#),
    @r###"
    ParseResult {
        output: Some(
            UnstructuredFile {
                tests: {
                    "cts.https.html?q=webgpu:api,operation,adapter,requestAdapter:requestAdapter_no_parameters:*": UnstructuredTest {
                        properties: {},
                        subtests: {
                            ":": UnstructuredSubtest {
                                properties: {
                                    "expected": Conditional(
                                        ConditionalValue {
                                            conditions: [
                                                (
                                                    Eq(
                                                        Value(
                                                            Variable(
                                                                "os",
                                                            ),
                                                        ),
                                                        Value(
                                                            Literal(
                                                                String(
                                                                    "mac",
                                                                ),
                                                            ),
                                                        ),
                                                    ),
                                                    "FAIL",
                                                ),
                                            ],
                                            fallback: None,
                                        },
                                    ),
                                },
                                span: 101..142,
                            },
                        },
                        span: 1..144,
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );
}

fn subtest_parser<'a, S>() -> impl Parser<'a, &'a str, (SectionHeader, S), ParseError<'a>>
where
    S: Subtest<'a>,
{
    SectionHeader::parser(1)
        .then_ignore(newline().or(end()))
        .labelled("subtest section header")
        .then(
            S::Properties::property_parser(&mut PropertiesParseHelper::new(2))
                .labelled("subtest property")
                .repeated()
                .collect::<Vec<_>>()
                .validate(|props, e, emitter| {
                    let mut properties = S::Properties::default();
                    for prop in props {
                        properties.add_property(prop, emitter);
                    }
                    S::new(e.span(), properties)
                }),
        )
        .labelled("subtest")
}

#[test]
fn smoke_subtest() {
    let subtest = || newline().ignore_then(subtest_parser::<UnstructuredSubtest<'_>>());

    assert_debug_snapshot!(
        subtest().parse(r#"
  [stuff and things]
"#),
        @r###"
    ParseResult {
        output: Some(
            (
                "stuff and things",
                UnstructuredSubtest {
                    properties: {},
                    span: 22..22,
                },
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        subtest().parse(r#"
  [stuff and things]
    some_prop: it_works
"#),
        @r###"
    ParseResult {
        output: Some(
            (
                "stuff and things",
                UnstructuredSubtest {
                    properties: {
                        "some_prop": Unconditional(
                            "it_works",
                        ),
                    },
                    span: 22..46,
                },
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        subtest().parse(r#"
  [stuff and things]
    expected:
      if thing: boo
      yay
"#),
        @r###"
    ParseResult {
        output: Some(
            (
                "stuff and things",
                UnstructuredSubtest {
                    properties: {
                        "expected": Conditional(
                            ConditionalValue {
                                conditions: [
                                    (
                                        Value(
                                            Variable(
                                                "thing",
                                            ),
                                        ),
                                        "boo",
                                    ),
                                ],
                                fallback: Some(
                                    "yay",
                                ),
                            },
                        ),
                    },
                    span: 22..66,
                },
            ),
        ),
        errs: [],
    }
    "###
    );
}

fn indent<'a>(level: u8) -> impl Parser<'a, &'a str, (), ParseError<'a>> {
    let level_as_space_count = usize::from(level) * 2;
    just(' ')
        .repeated()
        .exactly(level_as_space_count)
        .then_ignore(just(" ").not())
        .labelled("indentation at the proper level")
}

#[test]
fn test_indent() {
    assert_debug_snapshot!(indent(0).parse(""), @r###"
    ParseResult {
        output: Some(
            (),
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(indent(0).parse(" "), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 0..1 expected "indentation at the proper level",
        ],
    }
    "###);
    assert_debug_snapshot!(indent(0).parse("  "), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 0..1 expected "indentation at the proper level",
        ],
    }
    "###);
    assert_debug_snapshot!(indent(1).parse(""), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 0..0 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(indent(1).parse(" "), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 1..1 expected "indentation at the proper level",
        ],
    }
    "###);
    assert_debug_snapshot!(indent(1).parse("  "), @r###"
    ParseResult {
        output: Some(
            (),
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(indent(1).parse("   "), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 2..3 expected something else,
        ],
    }
    "###);
    assert_debug_snapshot!(indent(1).parse("    "), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 2..3 expected something else,
        ],
    }
    "###);
    assert_debug_snapshot!(indent(2).parse(""), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 0..0 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(indent(2).parse(" "), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 1..1 expected "indentation at the proper level",
        ],
    }
    "###);
    assert_debug_snapshot!(indent(2).parse("  "), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 2..2 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(indent(2).parse("   "), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 3..3 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(indent(2).parse("    "), @r###"
    ParseResult {
        output: Some(
            (),
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(indent(2).parse("     "), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 4..5 expected something else,
        ],
    }
    "###);
    assert_debug_snapshot!(indent(2).parse("      "), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 4..5 expected something else,
        ],
    }
    "###);
}

#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SectionHeader(pub String);

impl Debug for SectionHeader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(inner) = self;
        Debug::fmt(inner, f)
    }
}

impl SectionHeader {
    fn parser<'a>(indentation: u8) -> impl Parser<'a, &'a str, Self, ParseError<'a>> {
        let name = custom::<_, &str, _, _>(|input| {
            let mut escaped_name = String::new();
            loop {
                match input.peek() {
                    None => {
                        let start = input.offset();
                        input.skip();
                        let span = input.span_since(start);
                        return Err(Rich::custom(
                            span,
                            "reached end of input before ending section header",
                        ));
                    }
                    Some(']') => break,
                    Some('\\') => {
                        // NOTE: keep in sync. with the escaping in `Self::escaped`!
                        let c = input.parse(just("\\]").to(']'))?;
                        escaped_name.push(c);
                    }
                    Some(other) => {
                        escaped_name.push(other);
                        input.skip();
                    }
                }
            }
            Ok(escaped_name)
        })
        .validate(|escaped_name, e, emitter| {
            for (idx, c) in escaped_name.char_indices() {
                if c.is_control() {
                    let span_idx = e.span().start.checked_add(idx).unwrap();
                    emitter.emit(Rich::custom(
                        SimpleSpan::new(span_idx, span_idx),
                        "found illegal character in section header",
                    ));
                }
            }
            escaped_name
        });
        indent(indentation)
            .ignore_then(name.delimited_by(just('['), just(']')))
            .map(Self)
    }

    pub fn unescaped(&self) -> impl Display + '_ {
        let Self(inner) = self;
        inner
    }

    pub fn escaped(&self) -> impl Display + '_ {
        let Self(inner) = self;

        lazy_format!(|f| {
            let mut escaped_start = 0;
            for (idx, c) in inner.char_indices() {
                // NOTE: keep in sync. with the escaping in `Self::parser`!
                if let ']' = c {
                    write!(f, "{}\\", &inner[escaped_start..idx])?;
                    escaped_start = idx;
                }
            }
            write!(f, "{}", &inner[escaped_start..])?;
            Ok(())
        })
    }
}

#[test]
fn smoke_section_name() {
    let section_name = SectionHeader::parser;

    assert_debug_snapshot!(section_name(0).parse("hoot"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''h'' at 0..1 expected ''['',
        ],
    }
    "###);
    assert_debug_snapshot!(section_name(0).parse("[hoot]"), @r###"
    ParseResult {
        output: Some(
            "hoot",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(section_name(0).parse("[asdf\\]blarg]"), @r###"
    ParseResult {
        output: Some(
            "asdf]blarg",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(section_name(0).parse("[asdf]blarg]"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''b'' at 6..7 expected end of input,
        ],
    }
    "###);
}
