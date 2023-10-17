//! Data structures representing conditional expressions in [WPT metadata properties](super).

pub(crate) mod expr;

#[cfg(test)]
use insta::assert_debug_snapshot;

use chumsky::{
    prelude::Rich,
    primitive::{any, choice, end, group, just},
    text::{ascii::keyword, newline},
    IterParser, Parser,
};

use crate::metadata::{indent, ParseError};

pub use self::expr::{Expr, Literal, Value};

fn conditional_rule<'a>(
    indentation: u8,
) -> impl Parser<'a, &'a str, (Expr<Value<'a>>, &'a str), ParseError<'a>> {
    let unstructured_value = unstructured_value();
    group((indent(indentation), keyword("if"), just(' ')))
        .ignore_then(Expr::parser(Value::parser()).nested_in(unstructured_value.clone()))
        .then_ignore(just(':'))
        .then(unstructured_value)
        .then_ignore(newline().or(end()))
        .labelled("conditional value rule")
}

#[test]
fn test_conditional_rule() {
    assert_debug_snapshot!(conditional_rule(0).parse("if os == \"sux\": woot"), @r###"
    ParseResult {
        output: Some(
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
                                "sux",
                            ),
                        ),
                    ),
                ),
                " woot",
            ),
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(conditional_rule(1).parse("  if os == \"sux\": woot"), @r###"
    ParseResult {
        output: Some(
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
                                "sux",
                            ),
                        ),
                    ),
                ),
                " woot",
            ),
        ),
        errs: [],
    }
    "###);

    let conditional_rule = |indent| newline().ignore_then(conditional_rule(indent));

    assert_debug_snapshot!(
        conditional_rule(1).parse(r#"
  if os == "sux": woot
"#),
    @r###"
    ParseResult {
        output: Some(
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
                                "sux",
                            ),
                        ),
                    ),
                ),
                " woot",
            ),
        ),
        errs: [],
    }
    "###);

    assert_debug_snapshot!(
        conditional_rule(1).parse(r#"
  if debug: ohnoes
"#),
    @r###"
    ParseResult {
        output: Some(
            (
                Value(
                    Variable(
                        "debug",
                    ),
                ),
                " ohnoes",
            ),
        ),
        errs: [],
    }
    "###);
}

fn conditional_fallback<'a>(indentation: u8) -> impl Parser<'a, &'a str, &'a str, ParseError<'a>> {
    indent(indentation)
        .ignore_then(unstructured_value())
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
        output: None,
        errs: [
            found end of input at 0..0 expected something else,
        ],
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
        output: None,
        errs: [
            found end of input at 2..2 expected something else,
        ],
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
        output: None,
        errs: [
            found end of input at 6..6 expected something else,
        ],
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

pub(super) fn conditional_value<'a>(
    indentation: u8,
) -> impl Parser<'a, &'a str, (Vec<(Expr<Value<'a>>, &'a str)>, Option<&'a str>), ParseError<'a>> {
    conditional_rule(indentation)
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
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

pub(crate) fn unstructured_value<'a>() -> impl Clone + Parser<'a, &'a str, &'a str, ParseError<'a>>
{
    any()
        .and_is(choice((newline(), just(":").to(()))).not())
        .repeated()
        .at_least(1)
        .to_slice()
}

#[test]
fn test_conditional_value() {
    assert_debug_snapshot!(
        // Should fail, no conditional rules.
        conditional_value(0).parse("TIMEOUT"),
        @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 0..7 expected something else,
        ],
    }
    "###
    );

    assert_debug_snapshot!(
        conditional_value(0).parse("if os == \"awesome\": great"),
        @r###"
    ParseResult {
        output: Some(
            (
                [
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
                                        "awesome",
                                    ),
                                ),
                            ),
                        ),
                        " great",
                    ),
                ],
                None,
            ),
        ),
        errs: [],
    }
    "###
    );

    let conditional_value = |indent| newline().ignore_then(conditional_value(indent));

    assert_debug_snapshot!(
        conditional_value(0).parse(r#"
if os == "mac": PASS
if os == "linux": FAIL
TIMEOUT
"#),
        @r###"
    ParseResult {
        output: Some(
            (
                [
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
                        " PASS",
                    ),
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
                Some(
                    "TIMEOUT",
                ),
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        conditional_value(0).parse(r#"
if os == "mac": PASS
"#),
        @r###"
    ParseResult {
        output: Some(
            (
                [
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
                        " PASS",
                    ),
                ],
                None,
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        conditional_value(0).parse(r#"
if os == "mac": PASS
if os == "linux": FAIL
"#),
        @r###"
    ParseResult {
        output: Some(
            (
                [
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
                        " PASS",
                    ),
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
                None,
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        conditional_value(1).parse(r#"
  if os == "mac": PASS
  if os == "linux": FAIL
"#),
        @r###"
    ParseResult {
        output: Some(
            (
                [
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
                        " PASS",
                    ),
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
                None,
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        conditional_value(1).parse(r#"
  if os == "mac": PASS
  if os == "linux": FAIL
  TIMEOUT
"#),
        @r###"
    ParseResult {
        output: Some(
            (
                [
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
                        " PASS",
                    ),
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
                Some(
                    "TIMEOUT",
                ),
            ),
        ),
        errs: [],
    }
    "###
    );
}
