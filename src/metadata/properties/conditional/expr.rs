#[cfg(test)]
use insta::assert_debug_snapshot;

use chumsky::{
    pratt::{infix, left, prefix},
    primitive::{any, choice, just},
    text::{ascii::ident, inline_whitespace, newline},
    Parser,
};

use crate::metadata::ParseError;

/// Values that can be placed into [`Value::Literal`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Literal<'a> {
    /// At time of writing, no escaping is used for string values in this implementation.
    String(&'a str),
}

impl<'a> Literal<'a> {
    pub(crate) fn parser() -> impl Clone + Clone + Parser<'a, &'a str, Literal<'a>, ParseError<'a>>
    {
        any()
            .and_is(choice((newline(), just('"').to(()))).not())
            .repeated()
            .to_slice()
            .delimited_by(just('"'), just('"'))
            .map(Literal::String)
            .labelled("string literal")
    }
}

/// Variable and literal values supported by [WPT metadata
/// properties](crate::metadata::properties). Usually the terminal of a [`Expr`] expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value<'a> {
    Variable(&'a str),
    Literal(Literal<'a>),
}

impl<'a> Value<'a> {
    /// Retrieve a parser for [`Self`]. Often used as input to [`Expr::parser`].
    pub fn parser() -> impl Clone + Parser<'a, &'a str, Value<'a>, ParseError<'a>> {
        choice((
            ident().map(Value::Variable),
            Literal::parser().map(Value::Literal),
        ))
    }
}

/// Conditional expressions supported by [WPT metadata properties](crate::metadata::properties).
/// Usually contained by a [`ConditionalValue`](super::ConditionalValue).
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
        Pt: Clone + Parser<'a, &'a str, V, ParseError<'a>>,
    {
        let op = |symbol| just(symbol).padded_by(inline_whitespace());

        var_parser.map(Expr::Value).pratt((
            prefix(3, op("not"), |c| Expr::Not(Box::new(c))),
            infix(left(2), op("=="), |c1, c2| {
                Expr::Eq(Box::new(c1), Box::new(c2))
            }),
            infix(left(1), op("and"), |c1, c2| {
                Expr::And(Box::new(c1), Box::new(c2))
            }),
        ))
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
        condition().parse(r#"(os == "win") and not debug"#),
        @r###"
    ParseResult {
        output: None,
        errs: [
            found ''('' at 0..1 expected something else,
        ],
    }
    "###
    );
}
