#[cfg(test)]
use insta::assert_debug_snapshot;

use chumsky::{
    prelude::Rich,
    primitive::{any, choice, end, group, just},
    text::{ident, inline_whitespace, keyword, newline},
    IterParser, Parser,
};

use crate::metadata::{indent, ParseError};

/// A property value in a [`File`], [`Test`], or [`Subtest`]. Can be "unconditional"  or
/// "conditional" (runtime-evaluated).
///
/// See [`File`] for more details for the human-readable format this corresponds to.
///
/// [`File`]: crate::metadata::File
/// [`Test`]: crate::metadata::Test
/// [`Subtest`]: crate::metadata::Subtest
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PropertyValue<C, V> {
    /// A property value that is only ever a specific value.
    Unconditional(V),
    /// A property value that must be computed from variables provided by an evaluator. Usually,
    /// these variables do not vary between test runs on the same machine.
    ///
    /// Upstream documentation: [`Conditional Values`](https://web-platform-tests.org/tools/wptrunner/docs/expectation.html#conditional-values)
    Conditional {
        conditions: Vec<(Condition<C>, V)>,
        fallback: Option<V>,
    },
}

/// A (yet-to-be) strongly typed correspondent to conditions that can be used in
/// [`PropertyValue::Conditional`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Condition<T>(pub(crate) T);

pub(super) fn property<'a>(
    indentation: u8,
) -> impl Parser<'a, &'a str, (&'a str, PropertyValue<&'a str, &'a str>), ParseError<'a>> {
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
        .to_slice()
        .then_ignore(newline())
        .labelled("unconditional value")
}

fn conditional_rule<'a>(
    indentation: u8,
) -> impl Parser<'a, &'a str, (Condition<&'a str>, &'a str), ParseError<'a>> {
    group((indent(indentation), keyword("if"), just(' ')))
        .ignore_then(
            any()
                .and_is(choice((newline(), just(":").to(()))).not())
                .repeated()
                .to_slice()
                .map(Condition)
                .then_ignore(group((just(':'), inline_whitespace())))
                .then(
                    any()
                        .and_is(choice((newline(), just(':').to(()))).not())
                        .repeated()
                        .to_slice(),
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
                .to_slice(),
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
) -> impl Parser<'a, &'a str, (Vec<(Condition<&'a str>, &'a str)>, Option<&'a str>), ParseError<'a>>
{
    newline()
        .ignore_then(conditional_rule(indentation).repeated().collect::<Vec<_>>())
        .then(conditional_fallback(indentation).or_not())
        .validate(|(conditions, fallback), e, emitter| {
            if conditions.is_empty() && fallback.is_none() {
                emitter.emit(Rich::custom(
                    e.span(),
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
