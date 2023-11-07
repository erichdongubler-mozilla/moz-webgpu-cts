pub(crate) mod conditional;
pub mod unstructured;

#[cfg(any(test, feature = "unstructured-properties"))]
pub use unstructured::UnstructuredProperties;

use std::{fmt::Debug, marker::PhantomData};

pub use self::conditional::{ConditionalValue, Expr, Literal, Value};

use chumsky::{
    input::Emitter,
    prelude::Rich,
    primitive::{choice, end, group, just},
    text::{inline_whitespace, newline},
    Boxed, Parser,
};

use crate::metadata::{indent, ParseError};

/// A right-hand-side property value in a [`File`], [`Test`], or [`Subtest`]. Can be
/// "unconditional"  or "conditional" (viz., runtime-evaluated). The `C` type parameter represents
/// conditional clauses. The `V` type parameter represents right-hand values that this property can
/// evaluate to.
///
/// This type is parsed using an implementation of [`Properties::property_parser`].
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

impl<C, V> Default for PropertyValue<C, V>
where
    V: Default,
{
    fn default() -> Self {
        Self::Unconditional(Default::default())
    }
}

/// The core abstraction of strong property typing for [`File`]s, [`Test`]s, and [`Subtest`]s.
///
/// It is used to initialize and accumulate into the implementing data structure to represent
/// properties during parsing.
///
/// [`File`]: crate::metadata::File
/// [`Test`]: crate::metadata::Test
/// [`Subtest`]: crate::metadata::Subtest
/// [`File::parser`]: crate::metadata::File::parser
pub trait Properties<'a>
where
    Self: Clone + Default + Sized,
{
    /// The data structure that will be used to represent a single parsed property, including key,
    /// conditional expression(s), and value.
    type ParsedProperty: Clone + Debug;

    /// Retrieve a parser for a single property that [`Self::insert`] can accept.
    fn property_parser(
        helper: &mut PropertiesParseHelper<'a>,
    ) -> Boxed<'a, 'a, &'a str, Self::ParsedProperty, ParseError<'a>>;

    /// Accumulate a parsed property into this data structure.
    ///
    /// No span is provided here, but you can get one with [`chumsky::Parser::map_with`] in your
    /// implementation of [`Properties::property_parser`].
    fn add_property(&mut self, prop: Self::ParsedProperty, emitter: &mut Emitter<Rich<'a, char>>);
}

/// A helper passed to implementors of [`Properties::property_parser`]. The major affordance of
/// this API is the [`PropertiesParseHelper::parser`] API.
pub struct PropertiesParseHelper<'a> {
    indentation: u8,
    _disable_ctor: PhantomData<&'a mut ()>,
}

impl<'a> PropertiesParseHelper<'a> {
    #[cfg_attr(not(any(test, doctest)), doc(hidden))]
    pub fn new(indentation: u8) -> Self {
        Self {
            indentation,
            _disable_ctor: PhantomData,
        }
    }

    /// Create a parser for a single type of property, using the provided parsers for key,
    /// condition, and value. It is strongly recommended that you use the [`Expr`] and [`Value`]
    /// APIs to assist in parsing conditional expressions.
    ///
    /// # Examples
    ///
    /// ```
    /// use whippit::{
    ///     metadata::properties::{
    ///         ConditionalValue, Expr, Literal, PropertiesParseHelper, PropertyValue, Value
    ///     },
    ///     reexport::chumsky::{
    ///         prelude::*,
    ///         text::{ascii::keyword, newline},
    ///     }
    /// };
    ///
    /// # macro_rules! todo {
    /// #     ($expr:expr) => {
    /// #         PropertiesParseHelper::new(0)
    /// #     };
    /// # }
    /// let mut helper: PropertiesParseHelper<'_> = todo!(concat!(
    ///     "unfortunately, you will need to use your imagination for this part; ",
    ///     "assume zero indentation for now!"
    /// ));
    ///
    /// // Use `Expr` and `Value` parsing logic for conditionals.
    /// let my_condition_parser = Expr::parser(Value::parser());
    ///
    /// // Define a small set of properties to accept:
    /// // * `disabled`, which only ever expects a `true` RHS
    /// // * `expected`, which only ever expects a `FAIL` RHS
    /// //
    /// // If these were truly the full set of accepted values, one might want to use an `enum` and
    /// // `()` instead of retaining the `&str` values for key and value, respectively. For the
    /// // purposes of this example, though, we'll just keep things simple.
    /// let my_property_parser = choice((
    ///     helper.parser(
    ///         keyword("disabled"),
    ///         my_condition_parser.clone(),
    ///         keyword("true").padded(),
    ///     ),
    ///     helper.parser(
    ///         keyword("expected"),
    ///         my_condition_parser,
    ///         keyword("FAIL").padded(),
    ///     ),
    /// ));
    ///
    /// // Make multiline string examples parseable by skipping the first newline
    /// let my_property_parser = newline().ignore_then(my_property_parser);
    ///
    /// assert_eq!(
    ///     my_property_parser.parse(r#"
    /// disabled: true
    /// "#).into_result().unwrap(),
    ///     (
    ///         "disabled",
    ///         PropertyValue::Unconditional("true"),
    ///     ),
    /// );
    ///
    /// assert_eq!(
    ///     my_property_parser.parse(r#"
    /// expected:
    ///   if os == "win": FAIL
    /// "#).into_result().unwrap(),
    ///     (
    ///         "expected",
    ///         PropertyValue::Conditional(ConditionalValue {
    ///             conditions: vec![
    ///                 (
    ///                     Expr::Eq(
    ///                         Box::new(Expr::Value(Value::Variable("os".into()))),
    ///                         Box::new(Expr::Value(Value::Literal(Literal::String("win".into())))),
    ///                     ),
    ///                     "FAIL",
    ///                 ),
    ///             ],
    ///             fallback: None,
    ///         })
    ///     ),
    /// );
    /// ```
    pub fn parser<K, C, V, Pk, Pc, Pv>(
        &mut self,
        key_ident_parser: Pk,
        condition_parser: Pc,
        value_parser: Pv,
    ) -> impl Parser<'a, &'a str, (K, PropertyValue<C, V>), ParseError<'a>>
    where
        Pk: Parser<'a, &'a str, K, ParseError<'a>>,
        Pc: Parser<'a, &'a str, C, ParseError<'a>>,
        Pv: Clone + Parser<'a, &'a str, V, ParseError<'a>>,
    {
        let indentation = self.indentation;

        let key_ident_parser = key_ident_parser.labelled("property key");
        let condition_parser = condition_parser.labelled("conditional term for property value");
        let value_parser = value_parser.labelled("property value");

        let conditional_indent_level = indentation
            .checked_add(1)
            .expect("unexpectedly high indentation level");

        let property_value = || {
            choice((
                value_parser
                    .clone()
                    .then_ignore(newline().or(end()))
                    .map(PropertyValue::Unconditional),
                newline().ignore_then(
                    ConditionalValue::parser(
                        conditional_indent_level,
                        condition_parser,
                        value_parser,
                    )
                    .map(PropertyValue::Conditional),
                ),
            ))
            .labelled("property value")
        };

        indent(indentation).ignore_then(
            key_ident_parser
                .labelled("property key")
                .then_ignore(group((just(':'), inline_whitespace())))
                .then(property_value()),
        )
    }
}
