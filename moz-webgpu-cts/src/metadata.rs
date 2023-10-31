use std::{
    collections::BTreeMap,
    fmt::{self, Display},
    hash::Hash,
};

use chumsky::{
    input::Emitter,
    prelude::Rich,
    primitive::{choice, just},
    span::SimpleSpan,
    text::{inline_whitespace, keyword},
    Boxed, IterParser, Parser,
};
use enumset::EnumSetType;
use format::lazy_format;
use joinery::JoinableIterator;
use strum::{EnumIter, IntoEnumIterator};
use whippit::metadata::{
    self,
    properties::{
        ConditionalValue, Expr, Literal, Properties, PropertiesParseHelper, PropertyValue, Value,
    },
    ParseError,
};

use crate::shared::{Expectation, MaybeCollapsed, NormalizedExpectationPropertyValue};

#[cfg(test)]
use {chumsky::text::newline, insta::assert_debug_snapshot};

pub type File = metadata::File<AnalyzeableProps<TestOutcome>, AnalyzeableProps<SubtestOutcome>>;

pub type Test = metadata::Test<AnalyzeableProps<TestOutcome>, AnalyzeableProps<SubtestOutcome>>;

pub type Subtest = metadata::Subtest<AnalyzeableProps<SubtestOutcome>>;

pub fn format_file(file: &File) -> impl Display + '_ {
    lazy_format!(|f| {
        let metadata::File { tests } = file;
        write!(f, "{}", tests.iter().map(format_test).join_with("\n\n"))
    })
}

fn format_test(test: &Test) -> impl Display + '_ {
    lazy_format!(|f| {
        let Test {
            name,
            subtests,
            properties,
            ..
        } = test;
        write!(
            f,
            "[{}]\n{}{}",
            name.escaped(),
            format_properties(1, properties),
            subtests
                .iter()
                .map(|(name, subtest)| {
                    let Subtest { properties } = subtest;
                    lazy_format!(move |f| write!(
                        f,
                        "  [{}]\n{}",
                        name.escaped(),
                        format_properties(2, properties)
                    ))
                })
                .join_with('\n')
        )
    })
}

fn format_properties<Out>(indentation: u8, property: &AnalyzeableProps<Out>) -> impl Display + '_
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
        let AnalyzeableProps {
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
                if !matches!(exp, Expectation::Permanent(perma) if perma == &Default::default()) {
                    f()
                } else {
                    Ok(())
                }
            }

            let expected = lazy_format!("{indent}expected");
            let rhs = format_exp;
            let r#if = lazy_format!("{indent}  if");
            let disp_build_profile = |build_profile| match build_profile {
                BuildProfile::Debug => "debug",
                BuildProfile::Optimized => "not debug",
            };
            match exps.inner() {
                MaybeCollapsed::Collapsed(exps) => match exps {
                    MaybeCollapsed::Collapsed(exps) => {
                        if_not_default(exps, || writeln!(f, "{expected}: {}", rhs(exps)))?;
                    }
                    MaybeCollapsed::Expanded(by_build_profile) => {
                        writeln!(f, "{expected}:")?;
                        debug_assert!(!by_build_profile.is_empty());
                        for (build_profile, exps) in by_build_profile {
                            let build_profile = disp_build_profile(*build_profile);
                            if_not_default(exps, || {
                                writeln!(f, "{if} {build_profile}: {}", rhs(exps))
                            })?;
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
                            MaybeCollapsed::Collapsed(exps) => if_not_default(exps, || {
                                writeln!(f, "{if} {platform}: {}", rhs(exps))
                            })?,
                            MaybeCollapsed::Expanded(by_build_profile) => {
                                debug_assert!(!by_build_profile.is_empty());
                                for (build_profile, exps) in by_build_profile {
                                    let build_profile = disp_build_profile(*build_profile);
                                    if_not_default(exps, || {
                                        writeln!(
                                            f,
                                            "{if} {platform} and {build_profile}: {}",
                                            rhs(exps)
                                        )
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

fn format_exp<Exp>(exp: &Expectation<Exp>) -> impl Display + '_
where
    Exp: Display + EnumSetType,
{
    lazy_format!(move |f| {
        match exp {
            Expectation::Permanent(outcome) => write!(f, "{outcome}"),
            Expectation::Intermittent(outcomes) => {
                write!(f, "[{}]", outcomes.iter().join_with(", "))
            }
        }
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

#[derive(Clone, Debug)]
pub struct AnalyzeableProps<Out>
where
    Out: EnumSetType,
{
    pub is_disabled: bool,
    pub expectations: Option<NormalizedExpectationPropertyValue<Out>>,
}

impl<Out> Default for AnalyzeableProps<Out>
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

impl<'a, Out> AnalyzeableProps<Out>
where
    Out: Clone + Default + EnumSetType + Eq + PartialEq + Hash,
{
    fn insert(
        &mut self,
        prop: AnalyzeableProp<Out>,
        emitter: &mut chumsky::input::Emitter<Rich<'a, char>>,
    ) {
        let Self {
            is_disabled,
            expectations,
        } = self;

        let AnalyzeableProp { kind, span } = prop;

        match kind {
            AnalyzeablePropKind::Expected(val) => {
                if expectations.is_some() {
                    emitter.emit(Rich::custom(span, "duplicate `expected` key detected"));
                    return;
                }
                expectations.replace(match val {
                    PropertyValue::Unconditional(exp) => NormalizedExpectationPropertyValue(
                        MaybeCollapsed::Collapsed(MaybeCollapsed::Collapsed(exp)),
                    ),
                    PropertyValue::Conditional(val) => {
                        let ConditionalValue {
                            conditions,
                            fallback,
                        } = val;
                        if conditions.is_empty() {
                            NormalizedExpectationPropertyValue(MaybeCollapsed::Collapsed(
                                MaybeCollapsed::Collapsed(fallback.expect(concat!(
                                    "at least one condition or fallback not present ",
                                    "in conditional `expected` property value"
                                ))),
                            ))
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
                                                    matched = Some(val.clone());
                                                }
                                            }
                                            matched
                                                .or(fallback.clone())
                                                .map(|matched| (bp, matched))
                                        })
                                        .collect::<BTreeMap<_, _>>();
                                    (!by_build_profile.is_empty())
                                        .then_some(by_build_profile)
                                        .map(|tree| (p, tree))
                                })
                                .collect();

                            NormalizedExpectationPropertyValue::from_full(fully_expanded).unwrap()
                        }
                    }
                });
            }
            AnalyzeablePropKind::Disabled => {
                if *is_disabled {
                    emitter.emit(Rich::custom(span, "duplicate `disabled` key detected"))
                }
                *is_disabled = true;
            }
        }
    }
}

impl<Out> NormalizedExpectationPropertyValue<Out>
where
    Out: Clone + Default + EnumSetType + Eq + PartialEq,
{
    fn from_full(
        outcomes: BTreeMap<Platform, BTreeMap<BuildProfile, Expectation<Out>>>,
    ) -> Option<Self> {
        if outcomes.is_empty() {
            return None;
        }

        fn normalize<K, V, T, F>(
            mut map: BTreeMap<K, V>,
            mut f: F,
        ) -> MaybeCollapsed<T, BTreeMap<K, T>>
        where
            F: FnMut(V) -> T,
            K: IntoEnumIterator + Ord,
            V: Default,
            T: Clone + Default + Eq + PartialEq,
        {
            fn skip_default<K, V, I>(iter: I) -> impl Iterator<Item = (K, V)>
            where
                I: IntoIterator<Item = (K, V)>,
                V: Default + Eq + PartialEq,
            {
                iter.into_iter().filter(|(_k, v)| v != &Default::default())
            }

            let mut iter = K::iter().map(|k| {
                let v = map.remove(&k).unwrap_or_default();
                (k, f(v))
            });

            let (first_key, first_t) = iter.next().unwrap();

            let mut inconsistency_found = false;
            let mut expanded = BTreeMap::default();
            for (k, t) in iter.by_ref() {
                if t == first_t {
                    expanded.extend(skip_default([(k, t)]));
                } else {
                    inconsistency_found = true;
                    expanded.extend(skip_default([(k, t)].into_iter().chain(iter)));
                    break;
                }
            }
            if inconsistency_found {
                expanded.extend(skip_default([(first_key, first_t)]));
                MaybeCollapsed::Expanded(expanded)
            } else {
                MaybeCollapsed::Collapsed(first_t)
            }
        }

        Some(NormalizedExpectationPropertyValue(normalize(
            outcomes,
            |by_build_profile| normalize(by_build_profile, std::convert::identity),
        )))
    }
}

#[derive(Clone, Debug, Default)]
pub struct Applicability {
    pub platform: Option<Platform>,
    pub build_profile: Option<BuildProfile>,
}

#[derive(Clone, Debug)]
pub struct AnalyzeableProp<Out>
where
    Out: EnumSetType,
{
    span: SimpleSpan,
    kind: AnalyzeablePropKind<Out>,
}

#[derive(Clone, Debug)]
enum AnalyzeablePropKind<Out>
where
    Out: EnumSetType,
{
    Expected(PropertyValue<Applicability, Expectation<Out>>),
    Disabled,
}

impl<Out> AnalyzeableProp<Out>
where
    Out: EnumSetType,
{
    fn property_parser<'a, P>(
        helper: &mut PropertiesParseHelper<'a>,
        outcome_parser: P,
    ) -> impl Parser<'a, &'a str, AnalyzeableProp<Out>, ParseError<'a>>
    where
        Out: Eq + Hash + PartialEq,
        P: Clone + Parser<'a, &'a str, Out, ParseError<'a>>,
    {
        let conditional_term = Expr::parser(Value::parser()).validate(|prop_val, e, emitter| {
            let mut acc = Applicability::default();
            let try_match_var =
                |acc: &mut Applicability, val: &_, inverted: bool, emitter: &mut Emitter<_>| {
                    match val {
                        Value::Variable("debug") => {
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
                                        "{:?} is not a variable evaluatable ",
                                        "for WebGPU's purposes, discarding"
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
                        Expr::Value(Value::Variable("os")),
                        Expr::Value(Value::Literal(Literal::String(lit))),
                    ) => {
                        let platform = match *lit {
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
                        outcome_parser.clone().map(Expectation::Permanent),
                        outcome_parser
                            .padded_by(inline_whitespace())
                            .separated_by(just(','))
                            .collect::<Vec<_>>()
                            .map(|vec| vec.into_iter().collect())
                            .delimited_by(just('['), just(']'))
                            .map(Expectation::Intermittent),
                    ))
                    .padded_by(inline_whitespace()),
                )
                .map_with(|((), val), e| AnalyzeableProp {
                    span: e.span(),
                    kind: AnalyzeablePropKind::Expected(val),
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
                    AnalyzeableProp {
                        span: e.span(),
                        kind: AnalyzeablePropKind::Disabled,
                    }
                }),
        ))
    }
}

#[derive(Debug, EnumSetType, Hash)]
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

impl<'a> Properties<'a> for AnalyzeableProps<TestOutcome> {
    type ParsedProperty = AnalyzeableProp<TestOutcome>;
    fn property_parser(
        helper: &mut PropertiesParseHelper<'a>,
    ) -> Boxed<'a, 'a, &'a str, Self::ParsedProperty, ParseError<'a>> {
        AnalyzeableProp::property_parser(
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

    fn insert(
        &mut self,
        prop: Self::ParsedProperty,
        emitter: &mut chumsky::input::Emitter<Rich<'a, char>>,
    ) {
        self.insert(prop, emitter)
    }
}

#[derive(Debug, EnumSetType, Hash)]
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

impl<'a> Properties<'a> for AnalyzeableProps<SubtestOutcome> {
    type ParsedProperty = AnalyzeableProp<SubtestOutcome>;
    fn property_parser(
        helper: &mut PropertiesParseHelper<'a>,
    ) -> Boxed<'a, 'a, &'a str, Self::ParsedProperty, ParseError<'a>> {
        AnalyzeableProp::property_parser(
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

    fn insert(
        &mut self,
        prop: Self::ParsedProperty,
        emitter: &mut chumsky::input::Emitter<Rich<'a, char>>,
    ) {
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
    let parser = || {
        newline().ignore_then(metadata::File::<
            AnalyzeableProps<TestOutcome>,
            AnalyzeableProps<SubtestOutcome>,
        >::parser())
    };

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
                tests: [
                    Test {
                        name: "asdf",
                        properties: AnalyzeableProps {
                            is_disabled: false,
                            expectations: None,
                        },
                        subtests: {},
                        span: 1..8,
                    },
                ],
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
                tests: [
                    Test {
                        name: "asdf",
                        properties: AnalyzeableProps {
                            is_disabled: false,
                            expectations: None,
                        },
                        subtests: {
                            "blarg": Subtest {
                                properties: AnalyzeableProps {
                                    is_disabled: false,
                                    expectations: None,
                                },
                            },
                        },
                        span: 1..18,
                    },
                ],
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
                tests: [
                    Test {
                        name: "asdf",
                        properties: AnalyzeableProps {
                            is_disabled: false,
                            expectations: None,
                        },
                        subtests: {
                            "blarg": Subtest {
                                properties: AnalyzeableProps {
                                    is_disabled: false,
                                    expectations: Some(
                                        NormalizedExpectationPropertyValue(
                                            Collapsed(
                                                Collapsed(
                                                    Permanent(
                                                        Pass,
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ),
                                },
                            },
                        },
                        span: 1..37,
                    },
                ],
            },
        ),
        errs: [],
    }
    "###
    );

    let parser = || {
        single_leading_newline(metadata::Test::<
            AnalyzeableProps<TestOutcome>,
            AnalyzeableProps<SubtestOutcome>,
        >::parser())
    };

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
            Test {
                name: "asdf",
                properties: AnalyzeableProps {
                    is_disabled: false,
                    expectations: None,
                },
                subtests: {
                    "blarg": Subtest {
                        properties: AnalyzeableProps {
                            is_disabled: false,
                            expectations: Some(
                                NormalizedExpectationPropertyValue(
                                    Collapsed(
                                        Collapsed(
                                            Intermittent(
                                                EnumSet(Pass | Fail),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        },
                    },
                },
                span: 1..45,
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
  expected: OK
  [blarg]
    expected: PASS
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            Test {
                name: "asdf",
                properties: AnalyzeableProps {
                    is_disabled: false,
                    expectations: Some(
                        NormalizedExpectationPropertyValue(
                            Collapsed(
                                Collapsed(
                                    Permanent(
                                        Ok,
                                    ),
                                ),
                            ),
                        ),
                    ),
                },
                subtests: {
                    "blarg": Subtest {
                        properties: AnalyzeableProps {
                            is_disabled: false,
                            expectations: Some(
                                NormalizedExpectationPropertyValue(
                                    Collapsed(
                                        Collapsed(
                                            Permanent(
                                                Pass,
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        },
                    },
                },
                span: 1..52,
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
    expected:
      if os == "linux": FAIL
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            Test {
                name: "asdf",
                properties: AnalyzeableProps {
                    is_disabled: false,
                    expectations: None,
                },
                subtests: {
                    "blarg": Subtest {
                        properties: AnalyzeableProps {
                            is_disabled: false,
                            expectations: Some(
                                NormalizedExpectationPropertyValue(
                                    Expanded(
                                        {
                                            Linux: Collapsed(
                                                Permanent(
                                                    Fail,
                                                ),
                                            ),
                                        },
                                    ),
                                ),
                            ),
                        },
                    },
                },
                span: 1..61,
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
    expected:
      if os == "linux": FAIL
      TIMEOUT
"#
        ),
        @r###"
    ParseResult {
        output: Some(
            Test {
                name: "asdf",
                properties: AnalyzeableProps {
                    is_disabled: false,
                    expectations: None,
                },
                subtests: {
                    "blarg": Subtest {
                        properties: AnalyzeableProps {
                            is_disabled: false,
                            expectations: Some(
                                NormalizedExpectationPropertyValue(
                                    Expanded(
                                        {
                                            Windows: Collapsed(
                                                Permanent(
                                                    Timeout,
                                                ),
                                            ),
                                            Linux: Collapsed(
                                                Permanent(
                                                    Fail,
                                                ),
                                            ),
                                            MacOs: Collapsed(
                                                Permanent(
                                                    Timeout,
                                                ),
                                            ),
                                        },
                                    ),
                                ),
                            ),
                        },
                    },
                },
                span: 1..75,
            },
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
            Test {
                name: "cts.https.html?q=webgpu:api,validation,buffer,destroy:twice:*",
                properties: AnalyzeableProps {
                    is_disabled: false,
                    expectations: None,
                },
                subtests: {
                    ":": Subtest {
                        properties: AnalyzeableProps {
                            is_disabled: false,
                            expectations: Some(
                                NormalizedExpectationPropertyValue(
                                    Expanded(
                                        {
                                            MacOs: Collapsed(
                                                Permanent(
                                                    Fail,
                                                ),
                                            ),
                                        },
                                    ),
                                ),
                            ),
                        },
                    },
                },
                span: 1..113,
            },
        ),
        errs: [],
    }
    "###
    );
}
