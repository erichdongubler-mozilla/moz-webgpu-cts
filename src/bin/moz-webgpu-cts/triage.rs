use std::hash::Hash;

use chumsky::{
    input::Emitter,
    prelude::Rich,
    primitive::{choice, just},
    text::{inline_whitespace, keyword},
    Boxed, IterParser, Parser,
};
use indexmap::IndexSet;
use whippit::metadata::{
    self,
    properties::{Expr, Literal, Properties, PropertiesParseHelper, PropertyValue, Value},
    ParseError,
};

#[cfg(test)]
use {chumsky::text::newline, insta::assert_debug_snapshot};

pub type Test = metadata::Test<AnalyzeableProps<TestOutcome>, AnalyzeableProps<SubtestOutcome>>;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Platform {
    Windows,
    Linux,
    MacOs,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum BuildProfile {
    Debug,
    Optimized,
}

#[derive(Clone, Debug)]
pub struct AnalyzeableProps<Out> {
    pub is_disabled: bool,
    pub expectations: Option<PropertyValue<Applicability, Expectation<Out>>>,
}

impl<T> Default for AnalyzeableProps<T> {
    fn default() -> Self {
        Self {
            is_disabled: false,
            expectations: None,
        }
    }
}

impl<'a, Exp> AnalyzeableProps<Exp> {
    fn insert(
        &mut self,
        prop: AnalyzeableProp<Exp>,
        emitter: &mut chumsky::input::Emitter<Rich<'a, char>>,
    ) {
        let Self {
            is_disabled,
            expectations,
        } = self;

        match prop {
            AnalyzeableProp::Expected(val) => {
                if expectations.replace(val).is_some() {
                    emitter.emit(Rich::custom(
                        todo!("duplicate `expected` key detected"),
                        "duplicate `expected` key detected",
                    ))
                }
            }
            AnalyzeableProp::Disabled => {
                if *is_disabled {
                    emitter.emit(Rich::custom(
                        todo!("duplicate `expected` key detected"),
                        "duplicate `disabled` key detected",
                    ))
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
pub enum AnalyzeableProp<Out> {
    Expected(PropertyValue<Applicability, Expectation<Out>>),
    Disabled,
}

impl<Out> AnalyzeableProp<Out> {
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
                .map(|((), val)| val)
                .map(AnalyzeableProp::Expected),
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
                    AnalyzeableProp::Disabled
                }),
        ))
    }
}

#[derive(Clone, Debug)]
pub enum Expectation<Out> {
    Permanent(Out),
    Intermittent(IndexSet<Out>),
}

impl<Out> Default for Expectation<Out>
where
    Out: Default,
{
    fn default() -> Self {
        Self::Permanent(Default::default())
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TestOutcome {
    Ok,
    Timeout,
    Crash,
    Error,
}

impl Default for TestOutcome {
    fn default() -> Self {
        Self::Ok
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

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
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
                                        Unconditional(
                                            Permanent(
                                                Pass,
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
                                Unconditional(
                                    Intermittent(
                                        {
                                            Pass,
                                            Fail,
                                        },
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
                        Unconditional(
                            Permanent(
                                Ok,
                            ),
                        ),
                    ),
                },
                subtests: {
                    "blarg": Subtest {
                        properties: AnalyzeableProps {
                            is_disabled: false,
                            expectations: Some(
                                Unconditional(
                                    Permanent(
                                        Pass,
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
                                Conditional(
                                    ConditionalValue {
                                        conditions: [
                                            (
                                                Applicability {
                                                    platform: Some(
                                                        Linux,
                                                    ),
                                                    build_profile: None,
                                                },
                                                Permanent(
                                                    Fail,
                                                ),
                                            ),
                                        ],
                                        fallback: None,
                                    },
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
                                Conditional(
                                    ConditionalValue {
                                        conditions: [
                                            (
                                                Applicability {
                                                    platform: Some(
                                                        Linux,
                                                    ),
                                                    build_profile: None,
                                                },
                                                Permanent(
                                                    Fail,
                                                ),
                                            ),
                                        ],
                                        fallback: Some(
                                            Permanent(
                                                Timeout,
                                            ),
                                        ),
                                    },
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
                                Conditional(
                                    ConditionalValue {
                                        conditions: [
                                            (
                                                Applicability {
                                                    platform: Some(
                                                        MacOs,
                                                    ),
                                                    build_profile: None,
                                                },
                                                Permanent(
                                                    Fail,
                                                ),
                                            ),
                                        ],
                                        fallback: None,
                                    },
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
