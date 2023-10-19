#![cfg(any(test, feature = "unstructured-properties"))]

use chumsky::{input::Emitter, prelude::Rich, span::SimpleSpan, text::ascii::ident, Boxed, Parser};
use indexmap::IndexMap;

use crate::metadata::{File, ParseError};

use super::{
    conditional::{self, unstructured_value},
    Properties, PropertiesParseHelper, PropertyValue,
};

impl<'a> File<UnstructuredProperties<'a>, UnstructuredProperties<'a>> {
    /// Constructs
    pub fn parser_with_unstructured_props() -> impl Parser<
        'a,
        &'a str,
        File<UnstructuredProperties<'a>, UnstructuredProperties<'a>>,
        ParseError<'a>,
    > {
        Self::parser()
    }
}

pub type UnstructuredProperties<'a> =
    IndexMap<&'a str, PropertyValue<conditional::Expr<conditional::Value<'a>>, &'a str>>;

impl<'a> Properties<'a> for UnstructuredProperties<'a> {
    type ParsedProperty = (
        (SimpleSpan, &'a str),
        PropertyValue<conditional::Expr<conditional::Value<'a>>, &'a str>,
    );
    fn property_parser(
        helper: &mut PropertiesParseHelper<'a>,
    ) -> Boxed<'a, 'a, &'a str, Self::ParsedProperty, ParseError<'a>> {
        helper
            .parser(
                ident().map_with(|key, e| (e.span(), key)),
                unstructured_conditional_term(),
                unstructured_value(),
            )
            .boxed()
    }

    fn insert(&mut self, prop: Self::ParsedProperty, emitter: &mut Emitter<Rich<'a, char>>) {
        let ((key_span, key), value) = prop;
        if self.insert(key, value).is_some() {
            emitter.emit(Rich::custom(
                key_span,
                format!("duplicate {:?} property", key),
            ));
        }
    }
}

pub(crate) fn unstructured_conditional_term<'a>(
) -> impl Clone + Parser<'a, &'a str, conditional::Expr<conditional::Value<'a>>, ParseError<'a>> {
    conditional::Expr::parser(conditional::Value::parser())
}
