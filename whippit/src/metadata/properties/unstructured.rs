#![cfg(any(test, feature = "unstructured-properties"))]

use std::fmt::{Display, Formatter};

use chumsky::{input::Emitter, prelude::Rich, span::SimpleSpan, text::ascii::ident, Boxed, Parser};
use indexmap::IndexMap;
use lazy_format::make_lazy_format;

use crate::metadata::{
    properties::{ConditionalValue, Expr, Literal, Value},
    File, ParseError, SectionHeader, Subtest, Subtests, Test, Tests,
};

use super::{
    conditional::{self, unstructured_value},
    Properties, PropertiesParseHelper, PropertyValue,
};

#[derive(Clone, Debug, Default)]
pub struct UnstructuredFile<'a> {
    pub properties: UnstructuredProperties<'a>,
    pub tests: IndexMap<SectionHeader, UnstructuredTest<'a>>,
}

impl<'a> UnstructuredFile<'a> {
    pub fn parser() -> impl Parser<'a, &'a str, UnstructuredFile<'a>, ParseError<'a>> {
        crate::metadata::file_parser()
    }
}

pub fn format<'a>(file: &'a UnstructuredFile<'_>) -> impl Display + 'a {
    make_lazy_format!(|f| {
        let UnstructuredFile { properties, tests } = file;

        for (key, val) in properties {
            write!(f, "{key}:")?;
            match val {
                PropertyValue::Unconditional(val) => write!(f, " {val}")?,
                PropertyValue::Conditional(val) => {
                    let ConditionalValue {
                        conditions,
                        fallback,
                    } = val;

                    let write_val = |f: &mut Formatter<'_>, val: &_| match val {
                        Value::Variable(name) => f.write_str(name),
                        Value::Literal(val) => match val {
                            Literal::String(val) => write!(f, "\"{val}\""),
                        },
                    };

                    for (condition, val) in conditions {
                        f.write_str("  if ")?;
                        match condition {
                            Expr::Value(val) => write_val(f, val)?,
                            Expr::And(lhs, rhs) => match (&**lhs, &**rhs) {
                                (Expr::Value(lhs), Expr::Value(rhs)) => {
                                    write_val(f, lhs)?;
                                    f.write_str(" and ")?;
                                    write_val(f, rhs)?;
                                }
                                _ => todo!("the rest of the owl"),
                            },
                            Expr::Not(expr) => {
                                f.write_str("not ")?;
                                match &**expr {
                                    Expr::Value(val) => write_val(f, val)?,
                                    _ => todo!("the rest of the owl"),
                                }
                            }
                            Expr::Eq(lhs, rhs) => match (&**lhs, &**rhs) {
                                (Expr::Value(lhs), Expr::Value(rhs)) => {
                                    write_val(f, lhs)?;
                                    f.write_str(" == ")?;
                                    write_val(f, rhs)?;
                                }
                                _ => todo!("the rest of the owl"),
                            },
                        }
                        writeln!(f, ": {val}")?;
                    }
                    if let Some(val) = fallback {
                        writeln!(f, "  {val}")?;
                    }
                }
            }
        }

        Ok(())
    })
}

impl<'a> File<'a> for UnstructuredFile<'a> {
    type Properties = UnstructuredProperties<'a>;
    type Tests = UnstructuredTests<'a>;

    fn new(properties: Self::Properties, tests: Self::Tests) -> Self {
        let UnstructuredTests(tests) = tests;
        Self { properties, tests }
    }
}

#[derive(Clone, Debug, Default)]
pub struct UnstructuredTests<'a>(pub IndexMap<SectionHeader, UnstructuredTest<'a>>);

impl<'a> Tests<'a> for UnstructuredTests<'a> {
    type Test = UnstructuredTest<'a>;

    fn add_test(
        &mut self,
        name: SectionHeader,
        test: Self::Test,
        span: SimpleSpan,
        emitter: &mut Emitter<Rich<'a, char>>,
    ) {
        let Self(tests) = self;
        if tests.get(&name).is_some() {
            emitter.emit(Rich::custom(span, format!("duplicate test {name:?}")))
        }
        tests.insert(name, test);
    }
}

#[derive(Clone, Debug)]
pub struct UnstructuredTest<'a> {
    pub properties: UnstructuredProperties<'a>,
    pub subtests: IndexMap<SectionHeader, UnstructuredSubtest<'a>>,
    pub span: SimpleSpan,
}

impl<'a> Test<'a> for UnstructuredTest<'a> {
    type Properties = UnstructuredProperties<'a>;
    type Subtests = UnstructuredSubtests<'a>;

    fn new(
        span: SimpleSpan,
        properties: Self::Properties,
        subtests: UnstructuredSubtests<'a>,
    ) -> Self {
        let UnstructuredSubtests(subtests) = subtests;
        Self {
            span,
            properties,
            subtests,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct UnstructuredSubtests<'a>(IndexMap<SectionHeader, UnstructuredSubtest<'a>>);

impl<'a> Subtests<'a> for UnstructuredSubtests<'a> {
    type Subtest = UnstructuredSubtest<'a>;

    fn add_subtest(
        &mut self,
        name: SectionHeader,
        subtest: Self::Subtest,
        span: SimpleSpan,
        emitter: &mut Emitter<Rich<'a, char>>,
    ) {
        let Self(subtests) = self;
        if subtests.get(&name).is_some() {
            emitter.emit(Rich::custom(span, format!("duplicate subtest {name:?}")))
        }
        subtests.insert(name, subtest);
    }
}

#[derive(Clone, Debug)]
pub struct UnstructuredSubtest<'a> {
    pub properties: UnstructuredProperties<'a>,
    pub span: SimpleSpan,
}

impl<'a> Subtest<'a> for UnstructuredSubtest<'a> {
    type Properties = UnstructuredProperties<'a>;

    fn new(span: SimpleSpan, properties: Self::Properties) -> Self {
        Self { properties, span }
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

    fn add_property(&mut self, prop: Self::ParsedProperty, emitter: &mut Emitter<Rich<'a, char>>) {
        let ((key_span, key), value) = prop;
        if self.insert(key, value).is_some() {
            emitter.emit(Rich::custom(
                key_span,
                format!("duplicate {key:?} property"),
            ));
        }
    }
}

pub(crate) fn unstructured_conditional_term<'a>(
) -> impl Clone + Parser<'a, &'a str, conditional::Expr<conditional::Value<'a>>, ParseError<'a>> {
    conditional::Expr::parser(conditional::Value::parser())
}
