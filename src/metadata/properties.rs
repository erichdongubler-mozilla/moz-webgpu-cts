pub(crate) mod conditional;

use self::conditional::{conditional_value, Expr, Value};

use chumsky::{
    primitive::{any, choice, group, just},
    text::{ident, inline_whitespace, newline},
    Parser,
};

use crate::metadata::{indent, ParseError};

/// A right-hand-side property value in a [`File`], [`Test`], or [`Subtest`]. Can be
/// "unconditional"  or "conditional" (viz., runtime-evaluated). The `C` type parameter represents
/// conditional clauses. The `V` type parameter represents right-hand values that this property can
/// evaluate to.
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
    /// Upstream documentation: [`Conditional
    /// Values`](https://web-platform-tests.org/tools/wptrunner/docs/expectation.html#conditional-values)
    Conditional {
        conditions: Vec<(C, V)>,
        fallback: Option<V>,
    },
}

pub(super) fn property<'a>(
    indentation: u8,
) -> impl Parser<'a, &'a str, (&'a str, PropertyValue<Expr<Value<'a>>, &'a str>), ParseError<'a>> {
    let conditional_indent_level = indentation
        .checked_add(1)
        .expect("unexpectedly high indentation level");

    let property_value = || {
        choice((
            unconditional_value().map(PropertyValue::Unconditional),
            newline()
                .ignore_then(conditional_value(conditional_indent_level))
                .map(|(conditions, fallback)| PropertyValue::Conditional {
                    conditions,
                    fallback,
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
