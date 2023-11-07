use std::borrow::Cow;

#[cfg(test)]
use insta::assert_debug_snapshot;

use chumsky::{
    pratt::{infix, left, prefix},
    primitive::{any, choice, just},
    recursive::recursive,
    text::{ascii::ident, inline_whitespace, newline},
    Parser,
};

use crate::metadata::ParseError;

/// Values that can be placed into [`Value::Literal`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Literal<'a> {
    /// At time of writing, no escaping is used for string values in this implementation.
    String(Cow<'a, str>),
}

impl<'a> Literal<'a> {
    pub(crate) fn parser() -> impl Clone + Clone + Parser<'a, &'a str, Literal<'a>, ParseError<'a>>
    {
        any()
            .and_is(choice((newline(), just('"').to(()))).not())
            .repeated()
            .to_slice()
            .delimited_by(just('"'), just('"'))
            .map(|s: &str| Literal::String(s.into()))
            .labelled("string literal")
    }

    pub fn to_static(&self) -> Literal<'static> {
        match self {
            Self::String(s) => Literal::String(s.clone().into_owned().into()),
        }
    }
}

/// Variable and literal values supported by [WPT metadata
/// properties](crate::metadata::properties). Usually the terminal of a [`Expr`] expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value<'a> {
    Variable(Cow<'a, str>),
    Literal(Literal<'a>),
}

impl<'a> Value<'a> {
    /// Retrieve a parser for [`Self`]. Often used as input to [`Expr::parser`].
    pub fn parser() -> impl Clone + Parser<'a, &'a str, Value<'a>, ParseError<'a>> {
        choice((
            ident().map(|i: &str| Value::Variable(i.into())),
            Literal::parser().map(Value::Literal),
        ))
    }

    pub fn to_static(&self) -> Value<'static> {
        match self {
            Value::Variable(var) => Value::Variable(var.clone().into_owned().into()),
            Value::Literal(lit) => Value::Literal(lit.to_static()),
        }
    }
}

/// Conditional expressions supported by [WPT metadata properties](crate::metadata::properties).
/// Usually contained by a [`ConditionalValue`](super::ConditionalValue).
///
/// It is recommended that implementors of the [`Properties`] trait use this type to implement the
/// [`Properties::property_parser`] method.
///
/// [`PropertyValue::Conditional`]: crate::metadata::properties::PropertyValue::Conditional
/// [`Properties`]: crate::metadata::properties::Properties
/// [`Properties::property_parser`]: crate::metadata::properties::Properties::property_parser
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr<V> {
    Value(V),
    And(Box<Expr<V>>, Box<Expr<V>>),
    Not(Box<Expr<V>>),
    Eq(Box<Expr<V>>, Box<Expr<V>>),
}

impl<V> Expr<V> {
    /// Retrieve a parser for [`Self`], using `var_parser` to parse terminal expressions.
    /// `var_parser` is usually (but not always) a call to [`Value::parser`].
    pub fn parser<'a, Pt>(
        var_parser: Pt,
    ) -> impl Clone + Parser<'a, &'a str, Expr<V>, ParseError<'a>>
    where
        V: 'a,
        Pt: Clone + Parser<'a, &'a str, V, ParseError<'a>> + 'a,
    {
        let op = |symbol| just(symbol).padded_by(inline_whitespace());

        recursive(move |expr| {
            var_parser
                .map(Expr::Value)
                .or(expr.delimited_by(just('('), just(')')))
                .pratt((
                    prefix(3, op("not"), |c| Expr::Not(Box::new(c))),
                    infix(left(2), op("=="), |c1, c2| {
                        Expr::Eq(Box::new(c1), Box::new(c2))
                    }),
                    infix(left(1), op("and"), |c1, c2| {
                        Expr::And(Box::new(c1), Box::new(c2))
                    }),
                ))
        })
    }
}

#[test]
fn snapshots() {
    let condition = || Expr::parser(Value::parser());

    assert_debug_snapshot!(
        condition().parse(r#"os == "win" and debug"#),
        @r###"
    ParseResult {
        output: Some(
            And(
                Eq(
                    Value(
                        Variable(
                            "os",
                        ),
                    ),
                    Value(
                        Literal(
                            String(
                                "win",
                            ),
                        ),
                    ),
                ),
                Value(
                    Variable(
                        "debug",
                    ),
                ),
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        condition().parse(r#"(debug)"#),
        @r###"
    ParseResult {
        output: Some(
            Value(
                Variable(
                    "debug",
                ),
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        condition().parse(r#"(((((((debug)))))))"#),
        @r###"
    ParseResult {
        output: Some(
            Value(
                Variable(
                    "debug",
                ),
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        condition().parse(r#"((((os == "linux") and debug)))"#),
        @r###"
    ParseResult {
        output: Some(
            And(
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
                Value(
                    Variable(
                        "debug",
                    ),
                ),
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        condition().parse(r#"((os == "linux") and debug)"#),
        @r###"
    ParseResult {
        output: Some(
            And(
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
                Value(
                    Variable(
                        "debug",
                    ),
                ),
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        condition().parse(r#"(os == "win") and not debug"#),
        @r###"
    ParseResult {
        output: Some(
            And(
                Eq(
                    Value(
                        Variable(
                            "os",
                        ),
                    ),
                    Value(
                        Literal(
                            String(
                                "win",
                            ),
                        ),
                    ),
                ),
                Not(
                    Value(
                        Variable(
                            "debug",
                        ),
                    ),
                ),
            ),
        ),
        errs: [],
    }
    "###
    );
}
