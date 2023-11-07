use std::{
    collections::BTreeMap,
    convert::Infallible,
    fmt::{self, Display},
    hash::Hash,
};

use enumset::EnumSetType;
use format::lazy_format;
use joinery::JoinableIterator;
use serde::Deserialize;
use strum::{EnumIter, IntoEnumIterator};
use whippit::{
    metadata::{
        self, file_parser,
        properties::{
            ConditionalValue, Expr, Literal, Properties, PropertiesParseHelper, PropertyValue,
            Value,
        },
        ParseError, SectionHeader,
    },
    reexport::chumsky::{
        input::Emitter,
        prelude::Rich,
        primitive::{choice, custom, just},
        span::SimpleSpan,
        text::{inline_whitespace, keyword},
        Boxed, IterParser, Parser,
    },
};

use crate::shared::{Expectation, MaybeCollapsed, NormalizedExpectationPropertyValue};

#[cfg(test)]
use {insta::assert_debug_snapshot, whippit::reexport::chumsky::text::newline};

#[derive(Clone, Debug, Default)]
pub struct File {
    pub properties: FileProps,
    pub tests: BTreeMap<SectionHeader, Test>,
}

impl File {
    pub fn parser<'a>() -> impl Parser<'a, &'a str, File, ParseError<'a>> {
        file_parser()
    }
}

impl<'a> metadata::File<'a> for File {
    type Properties = FileProps;
    type Tests = Tests;

    fn new(properties: Self::Properties, tests: Self::Tests) -> Self {
        let Tests(tests) = tests;
        Self { properties, tests }
    }
}

#[derive(Clone, Debug, Default)]
pub struct FileProps {}

impl<'a> Properties<'a> for FileProps {
    type ParsedProperty = Infallible;

    fn property_parser(
        _helper: &mut PropertiesParseHelper<'a>,
    ) -> Boxed<'a, 'a, &'a str, Self::ParsedProperty, ParseError<'a>> {
        custom(|input| {
            let point = input.offset();
            Err(Rich::custom(
                input.span(point..point),
                "files currently do not support properties",
            ))
        })
        .boxed()
    }

    fn add_property(
        &mut self,
        _prop: Self::ParsedProperty,
        _emitter: &mut Emitter<Rich<'a, char>>,
    ) {
        panic!("attempted to add property");
    }
}

#[derive(Debug, Default)]
pub struct Tests(BTreeMap<SectionHeader, Test>);

impl<'a> metadata::Tests<'a> for Tests {
    type Test = Test;

    fn add_test(
        &mut self,
        name: SectionHeader,
        test: Self::Test,
        span: SimpleSpan,
        emitter: &mut Emitter<Rich<'a, char>>,
    ) {
        let Self(tests) = self;
        if tests.get(&name).is_some() {
            emitter.emit(Rich::custom(span, "duplicate test {name:?}"));
        }
        tests.insert(name, test);
    }
}

#[derive(Clone, Debug, Default)]
pub struct Test {
    pub properties: TestProps<TestOutcome>,
    pub subtests: BTreeMap<SectionHeader, Subtest>,
}

#[cfg(test)]
impl Test {
    fn parser<'a>() -> impl Parser<'a, &'a str, (SectionHeader, Test), ParseError<'a>> {
        metadata::test_parser()
    }
}

impl<'a> metadata::Test<'a> for Test {
    type Properties = TestProps<TestOutcome>;
    type Subtests = Subtests;

    fn new(_span: SimpleSpan, properties: Self::Properties, subtests: Self::Subtests) -> Self {
        let Subtests(subtests) = subtests;
        Self {
            properties,
            subtests,
        }
    }
}

#[derive(Default)]
pub struct Subtests(BTreeMap<SectionHeader, Subtest>);

impl<'a> metadata::Subtests<'a> for Subtests {
    type Subtest = Subtest;

    fn add_subtest(
        &mut self,
        name: SectionHeader,
        subtest: Self::Subtest,
        span: SimpleSpan,
        emitter: &mut Emitter<Rich<'a, char>>,
    ) {
        let Self(subtests) = self;
        if subtests.get(&name).is_some() {
            emitter.emit(Rich::custom(span, "duplicate subtest {name:?}"));
        }
        subtests.insert(name, subtest);
    }
}

#[derive(Clone, Debug, Default)]
pub struct Subtest {
    pub properties: TestProps<SubtestOutcome>,
}

impl<'a> metadata::Subtest<'a> for Subtest {
    type Properties = TestProps<SubtestOutcome>;

    fn new(_span: SimpleSpan, properties: Self::Properties) -> Self {
        Self { properties }
    }
}

pub fn format_file(file: &File) -> impl Display + '_ {
    lazy_format!(|f| {
        let File {
            properties: FileProps {},
            tests,
        } = file;
        write!(
            f,
            "{}",
            tests
                .iter()
                .map(|(name, test)| format_test(name, test))
                .join_with("\n\n")
        )
    })
}

fn format_test<'a>(name: &'a SectionHeader, test: &'a Test) -> impl Display + 'a {
    lazy_format!(|f| {
        let Test {
            subtests,
            properties,
            ..
        } = test;
        write!(
            f,
            "[{}]\n{}{}",
            name.escaped(),
            format_test_properties(1, properties),
            subtests
                .iter()
                .map(|(name, subtest)| {
                    let Subtest { properties } = subtest;
                    lazy_format!(move |f| write!(
                        f,
                        "  [{}]\n{}",
                        name.escaped(),
                        format_test_properties(2, properties)
                    ))
                })
                .join_with('\n')
        )
    })
}

fn format_test_properties<Out>(indentation: u8, property: &TestProps<Out>) -> impl Display + '_
where
    Out: Default + Display + EnumSetType + Eq + PartialEq,
{
    lazy_format!(move |f| {
        let indent = lazy_format!(move |f| write!(
            f,
            "{}",
            vec![""; usize::from(indentation) + 1]
                .into_iter()
                .join_with("  ")
        ));
        let TestProps {
            is_disabled,
            expectations,
        } = property;

        if *is_disabled {
            writeln!(f, "{indent}disabled: true")?;
        }

        if let Some(exps) = expectations {
            fn if_not_default<Out>(
                exp: &Expectation<Out>,
                f: impl FnOnce() -> fmt::Result,
            ) -> fmt::Result
            where
                Out: Default + EnumSetType + Eq + PartialEq,
            {
                if exp != &Default::default() {
                    f()
                } else {
                    Ok(())
                }
            }

            let expected = lazy_format!("{indent}expected");
            let r#if = lazy_format!("{indent}  if");
            let disp_build_profile = |build_profile| match build_profile {
                BuildProfile::Debug => "debug",
                BuildProfile::Optimized => "not debug",
            };
            match exps.inner() {
                MaybeCollapsed::Collapsed(exps) => match exps {
                    MaybeCollapsed::Collapsed(exps) => {
                        if_not_default(exps, || writeln!(f, "{expected}: {exps}"))?;
                    }
                    MaybeCollapsed::Expanded(by_build_profile) => {
                        writeln!(f, "{expected}:")?;
                        debug_assert!(!by_build_profile.is_empty());
                        for (build_profile, exps) in by_build_profile {
                            let build_profile = disp_build_profile(*build_profile);
                            if_not_default(exps, || writeln!(f, "{if} {build_profile}: {exps}"))?;
                        }
                    }
                },
                MaybeCollapsed::Expanded(by_platform) => {
                    writeln!(f, "{expected}:")?;
                    debug_assert!(!by_platform.is_empty());
                    for (platform, exps) in by_platform {
                        let platform = {
                            let platform_str = match platform {
                                Platform::Windows => "win",
                                Platform::Linux => "linux",
                                Platform::MacOs => "mac",
                            };
                            lazy_format!(move |f| write!(f, "os == {platform_str:?}"))
                        };
                        match exps {
                            MaybeCollapsed::Collapsed(exps) => {
                                if_not_default(exps, || writeln!(f, "{if} {platform}: {exps}"))?
                            }
                            MaybeCollapsed::Expanded(by_build_profile) => {
                                debug_assert!(!by_build_profile.is_empty());
                                for (build_profile, exps) in by_build_profile {
                                    let build_profile = disp_build_profile(*build_profile);
                                    if_not_default(exps, || {
                                        writeln!(f, "{if} {platform} and {build_profile}: {exps}")
                                    })?;
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    })
}

#[derive(Clone, Copy, Debug, EnumIter, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Platform {
    Windows,
    Linux,
    MacOs,
}

#[derive(Clone, Copy, Debug, EnumIter, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum BuildProfile {
    Debug,
    Optimized,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TestProps<Out>
where
    Out: EnumSetType,
{
    pub is_disabled: bool,
    pub expectations: Option<NormalizedExpectationPropertyValue<Out>>,
}

impl<Out> Default for TestProps<Out>
where
    Out: EnumSetType,
{
    fn default() -> Self {
        Self {
            is_disabled: false,
            expectations: None,
        }
    }
}

impl<'a, Out> TestProps<Out>
where
    Out: Clone + Default + EnumSetType + Eq + PartialEq + Hash,
{
    fn insert(&mut self, prop: TestProp<Out>, emitter: &mut Emitter<Rich<'a, char>>) {
        let Self {
            is_disabled,
            expectations,
        } = self;

        let TestProp { kind, span } = prop;

        match kind {
            TestPropKind::Expected(val) => {
                if expectations.is_some() {
                    emitter.emit(Rich::custom(span, "duplicate `expected` key detected"));
                    return;
                }
                expectations.replace(match val {
                    PropertyValue::Unconditional(exp) => {
                        NormalizedExpectationPropertyValue::uniform(exp)
                    }
                    PropertyValue::Conditional(val) => {
                        let ConditionalValue {
                            conditions,
                            fallback,
                        } = val;
                        if conditions.is_empty() {
                            NormalizedExpectationPropertyValue::uniform(fallback.expect(concat!(
                                "at least one condition or fallback not present ",
                                "in conditional `expected` property value"
                            )))
                        } else {
                            let fully_expanded = Platform::iter()
                                .filter_map(|p| {
                                    let by_build_profile = BuildProfile::iter()
                                        .filter_map(|bp| {
                                            let mut matched = None;

                                            for (applicability, val) in &conditions {
                                                let Applicability {
                                                    platform,
                                                    build_profile,
                                                } = applicability;
                                                if platform.as_ref().map_or(true, |p2| *p2 == p)
                                                    && build_profile
                                                        .as_ref()
                                                        .map_or(true, |bp2| *bp2 == bp)
                                                {
                                                    matched = Some(*val);
                                                }
                                            }
                                            matched.or(fallback).map(|matched| (bp, matched))
                                        })
                                        .collect::<BTreeMap<_, _>>();
                                    (!by_build_profile.is_empty())
                                        .then_some(by_build_profile)
                                        .map(|tree| (p, tree))
                                })
                                .collect();

                            NormalizedExpectationPropertyValue::from_fully_expanded(fully_expanded)
                        }
                    }
                });
            }
            TestPropKind::Disabled => {
                if *is_disabled {
                    emitter.emit(Rich::custom(span, "duplicate `disabled` key detected"))
                }
                *is_disabled = true;
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Applicability {
    pub platform: Option<Platform>,
    pub build_profile: Option<BuildProfile>,
}

#[derive(Clone, Debug)]
pub struct TestProp<Out>
where
    Out: EnumSetType,
{
    span: SimpleSpan,
    kind: TestPropKind<Out>,
}

#[derive(Clone, Debug)]
enum TestPropKind<Out>
where
    Out: EnumSetType,
{
    Expected(PropertyValue<Applicability, Expectation<Out>>),
    Disabled,
}

impl<Out> TestProp<Out>
where
    Out: EnumSetType,
{
    fn property_parser<'a, P>(
        helper: &mut PropertiesParseHelper<'a>,
        outcome_parser: P,
    ) -> impl Parser<'a, &'a str, TestProp<Out>, ParseError<'a>>
    where
        Out: Eq + Hash + PartialEq,
        P: Clone + Parser<'a, &'a str, Out, ParseError<'a>>,
    {
        let conditional_term = Expr::parser(Value::parser()).validate(|prop_val, e, emitter| {
            let mut acc = Applicability::default();
            let try_match_var =
                |acc: &mut Applicability, val: &_, inverted: bool, emitter: &mut Emitter<_>| {
                    match val {
                        Value::Variable(var_name) if var_name == "debug" => {
                            let build_profile = if inverted {
                                BuildProfile::Optimized
                            } else {
                                BuildProfile::Debug
                            };
                            if let Some(_old) = acc.build_profile.replace(build_profile) {
                                emitter.emit(Rich::custom(
                                    e.span(),
                                    "multiple `debug` conditions specified, discarding oldest",
                                ))
                            }
                        }
                        _ => {
                            emitter.emit(Rich::custom(
                                e.span(),
                                format!(
                                    concat!(
                                        "{:?} is not a variable evaluatable with ",
                                        "tests and subtests for WebGPU's purposes, discarding"
                                    ),
                                    e.slice()
                                ),
                            ));
                        }
                    }
                };

            let try_match_eq =
                |acc: &mut Applicability, lhs: &_, rhs: &_, emitter: &mut Emitter<_>| match (
                    lhs, rhs,
                ) {
                    (
                        Expr::Value(Value::Variable(var)),
                        Expr::Value(Value::Literal(Literal::String(lit))),
                    ) if var == "os" => {
                        let platform = match &**lit {
                            "mac" => Some(Platform::MacOs),
                            "linux" => Some(Platform::Linux),
                            "win" => Some(Platform::Windows),
                            _ => None,
                        };
                        if let Some(platform) = platform {
                            if let Some(_old) = acc.platform.replace(platform) {
                                emitter.emit(Rich::custom(
                                    e.span(),
                                    "multiple `os` conditions specified, discarding oldest",
                                ))
                            }
                        } else {
                            emitter.emit(Rich::custom(
                                e.span(),
                                format!(
                                    "{lit:?}{}",
                                    concat!(
                                        " is not a platform that the WebGPU ",
                                        "team recognizes, discarding"
                                    )
                                ),
                            ))
                        }
                    }
                    _ => emitter.emit(Rich::custom(
                        e.span(),
                        format!(
                            "{:?}{}",
                            concat!(
                                " is not an comparison evaluatable ",
                                "for WebGPU's purposes, discarding"
                            ),
                            e.slice()
                        ),
                    )),
                };
            match &prop_val {
                Expr::Value(v) => try_match_var(&mut acc, v, false, emitter),
                Expr::And(lhs, rhs) => {
                    for term in [lhs, rhs] {
                        match &**term {
                            Expr::Value(v) => try_match_var(&mut acc, v, false, emitter),
                            Expr::And(_, _) => emitter.emit(Rich::custom(
                                e.span(),
                                "`and` clauses too deep here, discarding",
                            )),
                            Expr::Not(term) => match &**term {
                                Expr::Value(v) => try_match_var(&mut acc, v, true, emitter),
                                _ => emitter.emit(Rich::custom(
                                    e.span(),
                                    "conditional clause inside `not` too deep here, discarding",
                                )),
                            },
                            Expr::Eq(lhs, rhs) => try_match_eq(&mut acc, &**lhs, &**rhs, emitter),
                        }
                    }
                }
                Expr::Not(term) => match &**term {
                    Expr::Value(v) => try_match_var(&mut acc, v, true, emitter),
                    _ => emitter.emit(Rich::custom(
                        e.span(),
                        "conditional clause inside `not` too deep here, discarding",
                    )),
                },
                Expr::Eq(lhs, rhs) => try_match_eq(&mut acc, &**lhs, &**rhs, emitter),
            };
            acc
        });
        choice((
            helper
                .parser(
                    just("expected").to(()),
                    conditional_term.clone(),
                    choice((
                        outcome_parser.clone().map(Expectation::permanent),
                        outcome_parser
                            .padded_by(inline_whitespace())
                            .separated_by(just(','))
                            .collect::<Vec<_>>()
                            .map(|vec| vec.into_iter().collect())
                            .delimited_by(just('['), just(']'))
                            .try_map(|outcomes, span| {
                                Expectation::intermittent(outcomes).ok_or_else(|| {
                                    Rich::custom(
                                        span,
                                        "intermittent outcomes must have at least 2 elements",
                                    )
                                })
                            }),
                    ))
                    .padded_by(inline_whitespace()),
                )
                .map_with(|((), val), e| TestProp {
                    span: e.span(),
                    kind: TestPropKind::Expected(val),
                }),
            helper
                .parser(
                    just("disabled").to(()),
                    conditional_term,
                    just("true").to(()),
                )
                .validate(|((), val), e, emitter| {
                    match val {
                        PropertyValue::Unconditional(()) => (),
                        PropertyValue::Conditional { .. } => {
                            emitter.emit(Rich::custom(
                                e.span(),
                                "conditional rules for `disabled` aren't supported yet",
                            ));
                        }
                    }
                    TestProp {
                        span: e.span(),
                        kind: TestPropKind::Disabled,
                    }
                }),
        ))
    }
}

#[derive(Debug, Deserialize, EnumSetType, Hash)]
#[serde(rename_all = "UPPERCASE")]
pub enum TestOutcome {
    Ok,
    Timeout,
    Crash,
    Error,
    Skip,
}

impl Default for TestOutcome {
    fn default() -> Self {
        Self::Ok
    }
}

impl Display for TestOutcome {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Ok => "OK",
                Self::Timeout => "TIMEOUT",
                Self::Crash => "CRASH",
                Self::Error => "ERROR",
                Self::Skip => "SKIP",
            }
        )
    }
}

impl<'a> Properties<'a> for TestProps<TestOutcome> {
    type ParsedProperty = TestProp<TestOutcome>;
    fn property_parser(
        helper: &mut PropertiesParseHelper<'a>,
    ) -> Boxed<'a, 'a, &'a str, Self::ParsedProperty, ParseError<'a>> {
        TestProp::property_parser(
            helper,
            choice((
                keyword("OK").to(TestOutcome::Ok),
                keyword("CRASH").to(TestOutcome::Crash),
                keyword("TIMEOUT").to(TestOutcome::Timeout),
                keyword("ERROR").to(TestOutcome::Error),
                keyword("SKIP").to(TestOutcome::Skip),
            )),
        )
        .boxed()
    }

    fn add_property(&mut self, prop: Self::ParsedProperty, emitter: &mut Emitter<Rich<'a, char>>) {
        self.insert(prop, emitter)
    }
}

#[derive(Debug, Deserialize, EnumSetType, Hash)]
#[serde(rename_all = "UPPERCASE")]
pub enum SubtestOutcome {
    Pass,
    Fail,
    Timeout,
    Crash,
    NotRun,
}

impl Default for SubtestOutcome {
    fn default() -> Self {
        Self::Pass
    }
}

impl Display for SubtestOutcome {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Pass => "PASS",
                Self::Fail => "FAIL",
                Self::Timeout => "TIMEOUT",
                Self::Crash => "CRASH",
                Self::NotRun => "NOTRUN",
            }
        )
    }
}

impl<'a> Properties<'a> for TestProps<SubtestOutcome> {
    type ParsedProperty = TestProp<SubtestOutcome>;
    fn property_parser(
        helper: &mut PropertiesParseHelper<'a>,
    ) -> Boxed<'a, 'a, &'a str, Self::ParsedProperty, ParseError<'a>> {
        TestProp::property_parser(
            helper,
            choice((
                keyword("PASS").to(SubtestOutcome::Pass),
                keyword("FAIL").to(SubtestOutcome::Fail),
                keyword("TIMEOUT").to(SubtestOutcome::Timeout),
                keyword("CRASH").to(SubtestOutcome::Crash),
                keyword("NOTRUN").to(SubtestOutcome::NotRun),
            )),
        )
        .boxed()
    }

    fn add_property(&mut self, prop: Self::ParsedProperty, emitter: &mut Emitter<Rich<'a, char>>) {
        self.insert(prop, emitter)
    }
}

#[cfg(test)]
fn single_leading_newline<'a, T, Pt>(p: Pt) -> impl Parser<'a, &'a str, T, ParseError<'a>>
where
    Pt: Parser<'a, &'a str, T, ParseError<'a>>,
{
    newline().ignore_then(p)
}

#[test]
fn test_sample_exps() {
    let parser = || newline().ignore_then(File::parser());

    assert_debug_snapshot!(
        parser().parse(
r#"
[asdf]
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            File {
                properties: FileProps,
                tests: {
                    "asdf": Test {
                        properties: TestProps {
                            is_disabled: false,
                            expectations: None,
                        },
                        subtests: {},
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        parser().parse(
r#"
[asdf]
  [blarg]
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            File {
                properties: FileProps,
                tests: {
                    "asdf": Test {
                        properties: TestProps {
                            is_disabled: false,
                            expectations: None,
                        },
                        subtests: {
                            "blarg": Subtest {
                                properties: TestProps {
                                    is_disabled: false,
                                    expectations: None,
                                },
                            },
                        },
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        parser().parse(
r#"
[asdf]
  [blarg]
    expected: PASS
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            File {
                properties: FileProps,
                tests: {
                    "asdf": Test {
                        properties: TestProps {
                            is_disabled: false,
                            expectations: None,
                        },
                        subtests: {
                            "blarg": Subtest {
                                properties: TestProps {
                                    is_disabled: false,
                                    expectations: Some(
                                        NormalizedExpectationPropertyValue(
                                            Collapsed(
                                                Collapsed(
                                                    [
                                                        Pass,
                                                    ],
                                                ),
                                            ),
                                        ),
                                    ),
                                },
                            },
                        },
                    },
                },
            },
        ),
        errs: [],
    }
    "###
    );

    let parser = || single_leading_newline(Test::parser());

    assert_debug_snapshot!(
        parser().parse(
r#"
[asdf]
  # Incorrect; `PASS` isn't a valid test outcome (but it _is_ a valid subtest outcome).
  expected: PASS
"#
        ),
        @r###"
    ParseResult {
        output: None,
        errs: [
            found end of input at 108..112 expected something else,
        ],
    }
    "###
    );

    assert_debug_snapshot!(
        parser().parse(
r#"
[asdf]
  [blarg]
    expected: [PASS, FAIL]
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            (
                "asdf",
                Test {
                    properties: TestProps {
                        is_disabled: false,
                        expectations: None,
                    },
                    subtests: {
                        "blarg": Subtest {
                            properties: TestProps {
                                is_disabled: false,
                                expectations: Some(
                                    NormalizedExpectationPropertyValue(
                                        Collapsed(
                                            Collapsed(
                                                [
                                                    Pass,
                                                    Fail,
                                                ],
                                            ),
                                        ),
                                    ),
                                ),
                            },
                        },
                    },
                },
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        parser().parse(
r#"
[asdf]
  expected: OK
  [blarg]
    expected: PASS
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            (
                "asdf",
                Test {
                    properties: TestProps {
                        is_disabled: false,
                        expectations: Some(
                            NormalizedExpectationPropertyValue(
                                Collapsed(
                                    Collapsed(
                                        [
                                            Ok,
                                        ],
                                    ),
                                ),
                            ),
                        ),
                    },
                    subtests: {
                        "blarg": Subtest {
                            properties: TestProps {
                                is_disabled: false,
                                expectations: Some(
                                    NormalizedExpectationPropertyValue(
                                        Collapsed(
                                            Collapsed(
                                                [
                                                    Pass,
                                                ],
                                            ),
                                        ),
                                    ),
                                ),
                            },
                        },
                    },
                },
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        parser().parse(
r#"
[asdf]
  [blarg]
    expected:
      if os == "linux": FAIL
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            (
                "asdf",
                Test {
                    properties: TestProps {
                        is_disabled: false,
                        expectations: None,
                    },
                    subtests: {
                        "blarg": Subtest {
                            properties: TestProps {
                                is_disabled: false,
                                expectations: Some(
                                    NormalizedExpectationPropertyValue(
                                        Expanded(
                                            {
                                                Linux: Collapsed(
                                                    [
                                                        Fail,
                                                    ],
                                                ),
                                            },
                                        ),
                                    ),
                                ),
                            },
                        },
                    },
                },
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        parser().parse(
r#"
[asdf]
  [blarg]
    expected:
      if os == "linux": FAIL
      TIMEOUT
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            (
                "asdf",
                Test {
                    properties: TestProps {
                        is_disabled: false,
                        expectations: None,
                    },
                    subtests: {
                        "blarg": Subtest {
                            properties: TestProps {
                                is_disabled: false,
                                expectations: Some(
                                    NormalizedExpectationPropertyValue(
                                        Expanded(
                                            {
                                                Windows: Collapsed(
                                                    [
                                                        Timeout,
                                                    ],
                                                ),
                                                Linux: Collapsed(
                                                    [
                                                        Fail,
                                                    ],
                                                ),
                                                MacOs: Collapsed(
                                                    [
                                                        Timeout,
                                                    ],
                                                ),
                                            },
                                        ),
                                    ),
                                ),
                            },
                        },
                    },
                },
            ),
        ),
        errs: [],
    }
    "###
    );

    assert_debug_snapshot!(
        parser().parse(r#"
[cts.https.html?q=webgpu:api,validation,buffer,destroy:twice:*]
  [:]
    expected:
      if os == "mac": FAIL

"#),
    @r###"
    ParseResult {
        output: Some(
            (
                "cts.https.html?q=webgpu:api,validation,buffer,destroy:twice:*",
                Test {
                    properties: TestProps {
                        is_disabled: false,
                        expectations: None,
                    },
                    subtests: {
                        ":": Subtest {
                            properties: TestProps {
                                is_disabled: false,
                                expectations: Some(
                                    NormalizedExpectationPropertyValue(
                                        Expanded(
                                            {
                                                MacOs: Collapsed(
                                                    [
                                                        Fail,
                                                    ],
                                                ),
                                            },
                                        ),
                                    ),
                                ),
                            },
                        },
                    },
                },
            ),
        ),
        errs: [],
    }
    "###
    );
}
