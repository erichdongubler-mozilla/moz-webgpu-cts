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
use {crate::metadata::properties::UnstructuredProperties, insta::assert_debug_snapshot};

use std::fmt::{self, Debug, Display};

use chumsky::{
    extra::Full,
    prelude::Rich,
    primitive::{any, choice, custom, end, group, just},
    span::SimpleSpan,
    text::newline,
    IterParser, Parser,
};
use indexmap::IndexMap;

use self::properties::{Properties, PropertiesParseHelper};

pub mod properties;

/// An error emitted by [`File::parser`] and [other WPT metadata parsing logic][self].
pub type ParseError<'a> = Full<Rich<'a, char>, (), ()>;

/// Represents the contents of a single file written in the [WPT metadata format][self]. It can be
/// constructed from this format using [`Self::parser`].
///
/// Properties for tests and subtests are abstracted out. If you don't have any opinions on how to
/// parse these yet, you can use the [`properties::unstructured`] API, or
/// [`File::parser_with_unstructured_props`] for convenience (both of which are gated behind the
/// `unstructured-properties` feature) to just get output with minimally structured property data.
/// If you'd like to more performance or stronger types with your properties, you will need to
/// provide types that implement the [`Properties`] trait for `Tp` and `Sp`.
///
/// N.B. that you should not rely only on data represented in this structure to compute test
/// metadata. It is _not_ complete by itself, because WPT metadata properties can be layered across
/// multiple sources (i.e., [`__dir__.ini`] files exist).
///
/// [`__dir__.ini`]: https://web-platform-tests.org/tools/wptrunner/docs/expectation.html#directory-metadata
#[derive(Clone, Debug)]
pub struct File<Tp, Sp> {
    pub tests: Vec<Test<Tp, Sp>>,
}

impl<Tp, Sp> File<Tp, Sp> {
    /// Returns a parser for a single file written in the [WPT metadata format][self].
    ///
    /// No attempt is made to reconcile the provided string with additional layers of
    /// configuration. See [`Self`] for more details.
    pub fn parser<'a>() -> impl Parser<'a, &'a str, File<Tp, Sp>, ParseError<'a>>
    where
        Tp: Properties<'a>,
        Sp: Properties<'a>,
    {
        filler()
            .ignore_then(Test::<Tp, Sp>::parser())
            .then_ignore(filler())
            .repeated()
            .collect()
            .map(|tests| File { tests })
    }
}

fn filler<'a>() -> impl Parser<'a, &'a str, (), ParseError<'a>> {
    choice((comment(0).ignored(), newline())).repeated()
}

#[test]
fn smoke_parser() {
    assert_debug_snapshot!(
        File::parser_with_unstructured_props().parse(""),
        @r###"
    ParseResult {
        output: Some(
            File {
                tests: [],
            },
        ),
        errs: [],
    }
    "###
    );
    assert_debug_snapshot!(File::parser_with_unstructured_props().parse("[hoot]"), @r###"
    ParseResult {
        output: Some(
            File {
                tests: [
                    Test {
                        name: "hoot",
                        properties: {},
                        subtests: {},
                        span: 0..6,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(File::parser_with_unstructured_props().parse("[blarg]\n"), @r###"
    ParseResult {
        output: Some(
            File {
                tests: [
                    Test {
                        name: "blarg",
                        properties: {},
                        subtests: {},
                        span: 0..8,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(File::parser_with_unstructured_props().parse("[blarg]\n[stuff]"), @r###"
    ParseResult {
        output: Some(
            File {
                tests: [
                    Test {
                        name: "blarg",
                        properties: {},
                        subtests: {},
                        span: 0..8,
                    },
                    Test {
                        name: "stuff",
                        properties: {},
                        subtests: {},
                        span: 8..15,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(File::parser_with_unstructured_props().parse("\n[blarg]\n[stuff]\n"), @r###"
    ParseResult {
        output: Some(
            File {
                tests: [
                    Test {
                        name: "blarg",
                        properties: {},
                        subtests: {},
                        span: 1..9,
                    },
                    Test {
                        name: "stuff",
                        properties: {},
                        subtests: {},
                        span: 9..17,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(File::parser_with_unstructured_props().parse("\n[blarg]\n\n[stuff]\n"), @r###"
    ParseResult {
        output: Some(
            File {
                tests: [
                    Test {
                        name: "blarg",
                        properties: {},
                        subtests: {},
                        span: 1..10,
                    },
                    Test {
                        name: "stuff",
                        properties: {},
                        subtests: {},
                        span: 10..18,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(File::parser_with_unstructured_props().parse("\n[blarg]\n  expected: PASS\n[stuff]\n"), @r###"
    ParseResult {
        output: Some(
            File {
                tests: [
                    Test {
                        name: "blarg",
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {},
                        span: 1..26,
                    },
                    Test {
                        name: "stuff",
                        properties: {},
                        subtests: {},
                        span: 26..34,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(
        File::parser_with_unstructured_props().parse(r#"
[blarg]
  expected: PASS
[stuff]
"#),
        @r###"
    ParseResult {
        output: Some(
            File {
                tests: [
                    Test {
                        name: "blarg",
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {},
                        span: 1..26,
                    },
                    Test {
                        name: "stuff",
                        properties: {},
                        subtests: {},
                        span: 26..34,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(
        File::parser_with_unstructured_props().parse(r#"
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
        File::parser_with_unstructured_props().parse(r#"
[blarg]
  expected: PASS
  [stuff]
    expected: TIMEOUT
"#),
        @r###"
    ParseResult {
        output: Some(
            File {
                tests: [
                    Test {
                        name: "blarg",
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {
                            "stuff": Subtest {
                                properties: {
                                    "expected": Unconditional(
                                        "TIMEOUT",
                                    ),
                                },
                            },
                        },
                        span: 1..58,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(
        File::parser_with_unstructured_props().parse(
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
            File {
                tests: [
                    Test {
                        name: "asdf",
                        properties: {},
                        subtests: {
                            "blarg": Subtest {
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
                                                    " FAIL",
                                                ),
                                            ],
                                            fallback: None,
                                        },
                                    ),
                                },
                            },
                        },
                        span: 1..61,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
    File::parser_with_unstructured_props().parse(
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
            File {
                tests: [
                    Test {
                        name: "asdf",
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {
                            "blarg": Subtest {
                                properties: {
                                    "expected": Unconditional(
                                        "PASS",
                                    ),
                                },
                            },
                        },
                        span: 1..54,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###
    );

    let parser = newline().ignore_then(File::parser_with_unstructured_props());

    assert_debug_snapshot!(
        parser.parse(r#"
[asdf]
  expected: PASS
"#),
    @r###"
    ParseResult {
        output: Some(
            File {
                tests: [
                    Test {
                        name: "asdf",
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {},
                        span: 1..25,
                    },
                ],
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Test<Tp, Sp> {
    pub name: SectionHeader,
    pub properties: Tp,
    pub subtests: IndexMap<SectionHeader, Subtest<Sp>>,
    span: SimpleSpan,
}

impl<Tp, Sp> Test<Tp, Sp> {
    pub fn parser<'a>() -> impl Parser<'a, &'a str, Test<Tp, Sp>, ParseError<'a>>
    where
        Tp: Properties<'a>,
        Sp: Properties<'a>,
    {
        #[derive(Clone, Debug)]
        enum Item<Tp, Sp> {
            Subtest { name: SectionHeader, properties: Sp },
            Property(Tp),
            Newline,
            Comment,
        }

        let items = choice((
            subtest().map(|(name, properties)| Item::Subtest { name, properties }),
            Tp::property_parser(&mut PropertiesParseHelper::new(1))
                .labelled("test property")
                .map(Item::Property),
            newline().labelled("empty line").to(Item::Newline),
            comment(1).to(Item::Comment),
        ))
        .repeated()
        .collect::<Vec<_>>()
        .validate(|items, e, emitter| {
            let mut properties = <Tp as Default>::default();
            let mut subtests = IndexMap::new();
            for item in items {
                match item {
                    Item::Property(prop) => properties.insert(prop, emitter),
                    Item::Subtest { name, properties } => {
                        if subtests.contains_key(&name) {
                            emitter.emit(Rich::custom(
                                e.span(),
                                format!("duplicate {} subtest", name.unescaped()),
                            ))
                        }
                        subtests.insert(name, Subtest { properties });
                    }
                    Item::Newline | Item::Comment => (),
                }
            }
            (properties, subtests)
        });

        let test_header = SectionHeader::parser(0)
            .then_ignore(newline().or(end()))
            .labelled("test section header");

        test_header
            .then(items)
            .map_with(|(name, (properties, subtests)), e| Test {
                name,
                span: e.span(),
                properties,
                subtests,
            })
    }
}

/// A single second-level section in a [`File`] underneath a [`Test`].
///
/// See [`File`] for more details for the human-readable format this corresponds to.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Subtest<P> {
    pub properties: P,
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
    let test = || Test::<UnstructuredProperties<'_>, UnstructuredProperties<'_>>::parser();

    assert_debug_snapshot!(
        test().parse("[stuff and things]\n"),
        @r###"
    ParseResult {
        output: Some(
            Test {
                name: "stuff and things",
                properties: {},
                subtests: {},
                span: 0..19,
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
            Test {
                name: "stuff and things",
                properties: {
                    "expected": Unconditional(
                        "PASS",
                    ),
                },
                subtests: {},
                span: 0..36,
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
            Test {
                name: "stuff and things",
                properties: {
                    "expected": Unconditional(
                        "PASS",
                    ),
                },
                subtests: {},
                span: 1..37,
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
            Test {
                name: "stuff and things",
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
                                    " boo",
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
            Test {
                name: "cts.https.html?q=webgpu:api,operation,adapter,requestAdapter:requestAdapter_no_parameters:*",
                properties: {},
                subtests: {
                    ":": Subtest {
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
                                            " FAIL",
                                        ),
                                    ],
                                    fallback: None,
                                },
                            ),
                        },
                    },
                },
                span: 1..144,
            },
        ),
        errs: [],
    }
    "###
    );
}

fn subtest<'a, Sp>() -> impl Parser<'a, &'a str, (SectionHeader, Sp), ParseError<'a>>
where
    Sp: Properties<'a>,
{
    SectionHeader::parser(1)
        .then_ignore(newline().or(end()))
        .labelled("subtest section header")
        .then(
            Sp::property_parser(&mut PropertiesParseHelper::new(2))
                .labelled("subtest property")
                .repeated()
                .collect::<Vec<_>>()
                .validate(|props, _e, emitter| {
                    let mut properties = Sp::default();
                    for prop in props {
                        properties.insert(prop, emitter);
                    }
                    properties
                }),
        )
        .labelled("subtest")
}

#[test]
fn smoke_subtest() {
    let subtest = || newline().ignore_then(subtest::<UnstructuredProperties<'_>>());

    assert_debug_snapshot!(
        subtest().parse(r#"
  [stuff and things]
"#),
        @r###"
    ParseResult {
        output: Some(
            (
                "stuff and things",
                {},
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
                {
                    "some_prop": Unconditional(
                        "it_works",
                    ),
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
                {
                    "expected": Conditional(
                        ConditionalValue {
                            conditions: [
                                (
                                    Value(
                                        Variable(
                                            "thing",
                                        ),
                                    ),
                                    " boo",
                                ),
                            ],
                            fallback: Some(
                                "yay",
                            ),
                        },
                    ),
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
