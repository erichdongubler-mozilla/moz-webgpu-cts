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
use insta::assert_debug_snapshot;

use chumsky::{
    extra::Full,
    prelude::Rich,
    primitive::{any, choice, custom, end, group, just},
    span::SimpleSpan,
    text::{ident, inline_whitespace, keyword, newline},
    IterParser, Parser,
};
use indexmap::IndexMap;

/// An error emitted by [`File::parser`] and [other WPT metadata parsing logic][self].
pub type ParseError<'a> = Full<Rich<'a, char>, (), ()>;

/// Represents the contents of a single file written in the [WPT metadata format][self]. It can be
/// constructed from this format using [`Self::parser`].
///
/// N.B. that you should not only use the data represented in this structure to compute test
/// metadata. It is _not_ complete by itself, because of the existence of layering in WPT metadata
/// (i.e., [`__dir__.ini`] files).
///
/// [`__dir__.ini`]: https://web-platform-tests.org/tools/wptrunner/docs/expectation.html#directory-metadata
#[derive(Clone, Debug)]
pub struct File<'a> {
    pub tests: Vec<Test<'a>>,
}

impl<'a> File<'a> {
    /// Returns a parser for a single file written in the [WPT metadata format][self].
    ///
    /// No attempt is made to reconcile the provided string with additional layers of
    /// configuration. See [`Self`] for more details.
    pub fn parser() -> impl Parser<'a, &'a str, File<'a>, ParseError<'a>> {
        filler()
            .ignore_then(test())
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
        File::parser().parse(""),
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
    assert_debug_snapshot!(File::parser().parse("[hoot]"), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 6..6 expected ''\r'', or ''\n'',
        ],
    }
    "###);
    assert_debug_snapshot!(File::parser().parse("[blarg]\n"), @r###"
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
    assert_debug_snapshot!(File::parser().parse("[blarg]\n[stuff]"), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 15..15 expected ''\r'', or ''\n'',
        ],
    }
    "###);
    assert_debug_snapshot!(File::parser().parse("\n[blarg]\n[stuff]\n"), @r###"
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
    assert_debug_snapshot!(File::parser().parse("\n[blarg]\n\n[stuff]\n"), @r###"
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
    assert_debug_snapshot!(File::parser().parse("\n[blarg]\n  expected: PASS\n[stuff]\n"), @r###"
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
}

/// A single first-level section in a [`File`].
///
/// See [`File`] for more details for the human-readable format this corresponds to.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Test<'a> {
    pub name: String,
    pub properties: IndexMap<&'a str, PropertyValue<'a>>,
    pub subtests: IndexMap<String, Subtest<'a>>,
    span: SimpleSpan,
}

/// A single second-level section in a [`File`] underneath a [`Test`].
///
/// See [`File`] for more details for the human-readable format this corresponds to.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Subtest<'a> {
    pub properties: IndexMap<&'a str, PropertyValue<'a>>,
}

/// A property value in a [`File`], [`Test`], or [`Subtest`]. Can be "unconditional"  or
/// "conditional" (runtime-evaluated).
///
/// See [`File`] for more details for the human-readable format this corresponds to.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PropertyValue<'a> {
    /// A property value that is only ever a specific value.
    Unconditional(&'a str),
    /// A property value that must be computed from variables provided by an evaluator. Usually,
    /// these variables do not vary between test runs on the same machine.
    ///
    /// Upstream documentation: [`Conditional Values`](https://web-platform-tests.org/tools/wptrunner/docs/expectation.html#conditional-values)
    Conditional {
        conditions: Vec<(Condition<'a>, &'a str)>,
        fallback: Option<&'a str>,
    },
}

/// A (yet-to-be) strongly typed correspondent to conditions that can be used in
/// [`PropertyValue::Conditional`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Condition<'a>(&'a str);

fn comment<'a>(indentation: u8) -> impl Parser<'a, &'a str, &'a str, ParseError<'a>> {
    group((indent(indentation), just('#'), just(' ').or_not()))
        .ignore_then(any().and_is(newline().not()).repeated().slice())
        .then_ignore(choice((newline(), end())))
        .labelled("comment")
}

#[test]
fn smoke_comment() {
    assert_debug_snapshot!(comment(0).parse("asdf"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''a'' at 0..1 expected "comment",
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
            found ''#'' at 1..2 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(comment(1).parse("# asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''#'' at 0..1 expected "comment",
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
            found ''#'' at 1..2 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(comment(2).parse("# asdf # blarg"), @r###"
    ParseResult {
        output: None,
        errs: [
            found ''#'' at 0..1 expected "comment",
        ],
    }
    "###);
}

fn test<'a>() -> impl Parser<'a, &'a str, Test<'a>, ParseError<'a>> {
    #[derive(Clone, Debug)]
    enum Item<'a> {
        Subtest {
            name: String,
            properties: IndexMap<&'a str, PropertyValue<'a>>,
        },
        Property {
            key: &'a str,
            value: PropertyValue<'a>,
        },
        Newline,
        Comment,
    }

    let subtest = || {
        section_name(1)
            .then_ignore(newline())
            .labelled("subtest section header")
            .then(
                property(2)
                    .labelled("subtest property")
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(subtest_name, properties)| Item::Subtest {
                name: subtest_name,
                properties: properties.into_iter().collect(),
            })
            .labelled("subtest")
    };

    let items = choice((
        subtest(),
        property(1)
            .labelled("test property")
            .map(|(key, value)| Item::Property { key, value }),
        newline().labelled("empty line").to(Item::Newline),
        comment(1).to(Item::Comment),
    ))
    .repeated()
    .collect::<Vec<_>>()
    .validate(|items, _span, emitter| {
        let mut properties = IndexMap::new();
        let mut subtests = IndexMap::new();
        for item in items {
            match item {
                Item::Property { key, value } => {
                    if let Some(_old) = properties.insert(key, value) {
                        emitter.emit(Rich::custom(_span, format!("duplicate {key} property")))
                    }
                }
                Item::Subtest { name, properties } => {
                    if subtests.contains_key(&name) {
                        // TODO: use old and new item span, better msg.
                        emitter.emit(Rich::custom(_span, format!("duplicate {name} subtest")))
                    }
                    subtests.insert(name, Subtest { properties });
                }
                Item::Newline | Item::Comment => (),
            }
        }
        (properties, subtests)
    });

    let test_header = section_name(0)
        .then_ignore(newline())
        .labelled("test section header");

    test_header
        .then(items)
        .map_with_span(|(name, (properties, subtests)), span| Test {
            name,
            span,
            properties,
            subtests,
        })
}

#[test]
fn smoke_test() {
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
}

fn property<'a>(
    indentation: u8,
) -> impl Parser<'a, &'a str, (&'a str, PropertyValue<'a>), ParseError<'a>> {
    let conditional_indent_level = indentation
        .checked_add(1)
        .expect("unexpectedly high indentation level");

    let property_value = || {
        choice((
            unconditional_value().map(PropertyValue::Unconditional),
            conditional_value(conditional_indent_level).map(|(conditions, fallback)| {
                PropertyValue::Conditional {
                    conditions,
                    fallback,
                }
            }),
        ))
        .labelled("property value")
    };

    property_key(indentation)
        .then_ignore(group((just(':'), inline_whitespace())))
        .then(property_value())
}

fn property_key<'a>(indentation: u8) -> impl Parser<'a, &'a str, &'a str, ParseError<'a>> {
    indent(indentation)
        .ignore_then(ident())
        .labelled("property key")
}

fn unconditional_value<'a>() -> impl Parser<'a, &'a str, &'a str, ParseError<'a>> {
    any()
        .and_is(newline().not())
        .repeated()
        .at_least(1)
        .slice()
        .then_ignore(newline())
        .labelled("unconditional value")
}

fn conditional_rule<'a>(
    indentation: u8,
) -> impl Parser<'a, &'a str, (Condition<'a>, &'a str), ParseError<'a>> {
    group((indent(indentation), keyword("if"), just(' ')))
        .ignore_then(
            any()
                .and_is(choice((newline(), just(":").to(()))).not())
                .repeated()
                .slice()
                .map(Condition)
                .then_ignore(group((just(':'), inline_whitespace())))
                .then(
                    any()
                        .and_is(choice((newline(), just(':').to(()))).not())
                        .repeated()
                        .slice(),
                ),
        )
        .then_ignore(newline().or(end()))
        .labelled("conditional value rule")
}

fn conditional_fallback<'a>(indentation: u8) -> impl Parser<'a, &'a str, &'a str, ParseError<'a>> {
    indent(indentation)
        .ignore_then(
            keyword("if")
                .not()
                .ignore_then(any().and_is(newline().not()).repeated())
                .slice(),
        )
        .then_ignore(newline().or(end()))
        .labelled("conditional value fallback")
}

#[test]
fn test_conditional_fallback() {
    assert_debug_snapshot!(conditional_fallback(0).parse("[PASS, FAIL]"), @r###"
    ParseResult {
        output: Some(
            "[PASS, FAIL]",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(0).parse(r#""okgo""#), @r###"
    ParseResult {
        output: Some(
            "\"okgo\"",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(0).parse(""), @r###"
    ParseResult {
        output: Some(
            "",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(1).parse(""), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 0..0 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(1).parse("  "), @r###"
    ParseResult {
        output: Some(
            "",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(1).parse("  @False"), @r###"
    ParseResult {
        output: Some(
            "@False",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(1).parse("    @False"), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 2..3 expected something else,
        ],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(3).parse(""), @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 0..0 expected '' '',
        ],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(3).parse("      "), @r###"
    ParseResult {
        output: Some(
            "",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(3).parse("      @True"), @r###"
    ParseResult {
        output: Some(
            "@True",
        ),
        errs: [],
    }
    "###);
    assert_debug_snapshot!(conditional_fallback(3).parse("        @True"), @r###"
    ParseResult {
        output: None,
        errs: [
            found '' '' at 6..7 expected something else,
        ],
    }
    "###);
}

fn conditional_value<'a>(
    indentation: u8,
) -> impl Parser<'a, &'a str, (Vec<(Condition<'a>, &'a str)>, Option<&'a str>), ParseError<'a>> {
    newline()
        .ignore_then(conditional_rule(indentation).repeated().collect::<Vec<_>>())
        .then(conditional_fallback(indentation).or_not())
        .validate(|(conditions, fallback), span, emitter| {
            if conditions.is_empty() && fallback.is_none() {
                emitter.emit(Rich::custom(
                    span,
                    concat!(
                        "this conditional property value has no conditional ",
                        "rules or fallback specified",
                    ),
                ));
            }
            (conditions, fallback)
        })
        .labelled("conditional value")
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

fn section_name<'a>(indentation: u8) -> impl Parser<'a, &'a str, String, ParseError<'a>> {
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
                    let c = input.parse(choice((just("\\]").to(']'), just("\\\"").to('"'))))?;
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
    .validate(|escaped_name, span, emitter| {
        for (idx, c) in escaped_name.char_indices() {
            if c.is_control() {
                let span_idx = span.start.checked_add(idx).unwrap();
                emitter.emit(Rich::custom(
                    SimpleSpan::new(span_idx, span_idx),
                    "found illegal character in section header",
                ));
            }
        }
        escaped_name
    });
    indent(indentation).ignore_then(name.delimited_by(just('['), just(']')))
}

#[test]
fn smoke_section_name() {
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
            found ''b'' at 6..7 expected something else,
        ],
    }
    "###);
}
