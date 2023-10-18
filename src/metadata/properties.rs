pub(crate) mod conditional;

use std::marker::PhantomData;

pub use self::conditional::{ConditionalValue, Expr, Literal, Value};

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
    Conditional(ConditionalValue<C, V>),
}

pub struct PropertiesParseHelper<'a> {
    indentation: u8,
    _disable_ctor: PhantomData<&'a mut ()>,
}

impl<'a> PropertiesParseHelper<'a> {
    pub(super) fn new(indentation: u8) -> Self {
        Self {
            indentation,
            _disable_ctor: PhantomData,
        }
    }

    pub fn parser(
        &mut self,
    ) -> impl Parser<'a, &'a str, (&'a str, PropertyValue<Expr<Value<'a>>, &'a str>), ParseError<'a>>
    {
        let indentation = self.indentation;

        let conditional_indent_level = indentation
            .checked_add(1)
            .expect("unexpectedly high indentation level");

        let property_value = || {
            choice((
                unconditional_value().map(PropertyValue::Unconditional),
                newline().ignore_then(
                    ConditionalValue::parser(conditional_indent_level)
                        .map(PropertyValue::Conditional),
                ),
            ))
            .labelled("property value")
        };

        indent(indentation)
            .ignore_then(ident())
            .labelled("property key")
            .then_ignore(group((just(':'), inline_whitespace())))
            .then(property_value())
    }
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
