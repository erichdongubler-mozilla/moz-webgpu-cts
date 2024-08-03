use std::{
    io::{self, BufRead},
    ops::Range,
};

use chrono::{DateTime, FixedOffset};
use miette::Diagnostic;
use whippit::reexport::chumsky::{
    error::{EmptyErr, Simple},
    extra::{self, Full, ParserExtra},
    input::{Input, SliceInput, StrInput, ValueInput},
    primitive::{any, choice, just},
    span::SimpleSpan,
    text::{ascii, digits},
    IterParser, Parser,
};

use crate::wpt::{
    metadata::{SubtestOutcome, TestOutcome},
    path::{Browser, TestEntryPath},
};

pub(super) struct LogLineReader<R> {
    browser: Browser,
    next_line_idx: u64,
    reader: R,
}

impl<R> LogLineReader<R> {
    pub fn new(browser: Browser, reader: R) -> Self {
        Self {
            browser,
            next_line_idx: 1,
            reader,
        }
    }
}

impl<R> LogLineReader<R>
where
    R: BufRead,
{
    pub fn next_log_line(
        &mut self,
        buf: &mut String,
        test_log_line_parse_error_sink: &mut dyn FnMut(RecognizedLogLineParseError),
    ) -> Option<Result<LogLine, LogLineReadError>> {
        let line_offset_in_buf = buf.len();
        let mut should_keep_line = false;
        let ret = self.read_line(buf)?.and_then(|(line, line_idx)| {
            let LogLineClassificationResult {
                inner: res,
                should_save_spans,
            } = classify_log_line(
                self.browser,
                line_idx,
                line,
                line_offset_in_buf,
                test_log_line_parse_error_sink,
            );
            should_keep_line = should_save_spans;
            let kind =
                res.map_err(LogLineReadErrorKind::from)
                    .map_err(|source| LogLineReadError {
                        line_num: line_idx,
                        source,
                    })?;
            Ok(LogLine {
                _line_num: line_idx,
                kind,
            })
        });
        if !should_keep_line {
            buf.truncate(line_offset_in_buf);
        }
        Some(ret)
    }

    fn read_line<'a>(
        &mut self,
        buf: &'a mut String,
    ) -> Option<Result<(&'a str, u64), LogLineReadError>> {
        let Self {
            next_line_idx,
            reader,
            ..
        } = self;

        let start = buf.len();
        match reader
            .read_line(buf)
            .map_err(|source| LogLineReadErrorKind::Io { source })
            .map_err(|source| LogLineReadError {
                line_num: *next_line_idx,
                source,
            }) {
            Ok(0) => None,
            Ok(bytes_read) => {
                let mut line = &buf[start..buf.len()];
                line = line.strip_suffix('\n').unwrap_or(line);

                let extracted = match bytes_read {
                    0 => None,
                    _ => Some(Ok((line, *next_line_idx))),
                };
                *next_line_idx = next_line_idx.checked_add(1).unwrap();
                extracted
            }
            Err(e) => Some(Err(e)),
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) enum SuiteOrTest {
    Suite,
    Test,
}

#[derive(Debug, Diagnostic, thiserror::Error)]
#[error("failed to read line {line_num} of log")]
pub(super) struct LogLineReadError {
    line_num: u64,
    source: LogLineReadErrorKind,
}

#[derive(Debug, Diagnostic, thiserror::Error)]
pub(super) enum LogLineReadErrorKind {
    #[error("I/O error")]
    Io { source: io::Error },
    #[error("failed to classify log line")]
    ClassifyTestLogLine {
        #[from]
        source: CheckErrorSink,
    },
}

#[derive(Clone, Debug)]
pub(super) struct LogLine {
    pub _line_num: u64,
    pub kind: LogLineKind,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) enum LogLineKind {
    Suite(SuiteLogLine),
    Test(TestLogLine),
    AbortingTask,
    Other,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) struct SuiteLogLine {
    pub timestamp: DateTime<FixedOffset>,
    pub kind: SuiteLogLineKind,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) enum SuiteLogLineKind {
    Start,
    End,
}

impl SuiteLogLineKind {
    pub fn new(s: &str) -> Option<Self> {
        match s {
            "START" => Some(Self::Start),
            "END" => Some(Self::End),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) struct LogLineSpans {
    offset_in_buf: usize,
    length: usize,
}

impl LogLineSpans {
    pub fn buf_slice_idx(&self) -> Range<usize> {
        let &Self {
            offset_in_buf,
            length,
            ..
        } = self;
        offset_in_buf..(offset_in_buf + length)
    }

    #[track_caller]
    pub fn get_from<'a>(&self, s: &'a str) -> &'a str {
        s.get(self.buf_slice_idx()).unwrap()
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) struct TestLogLine {
    pub timestamp: DateTime<FixedOffset>,
    pub kind: TestLogLineKind,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) enum TestLogLineKind {
    Start { test_url_path: LogLineSpans },
    Info,
    LeakCheck,
    FinishTestExpected { _test_name: LogLineSpans },
    FinishTestUnexpected,
}

#[derive(Clone, Debug)]
enum TestLogLineDiscriminant {
    Start,
    Info,
    Expected(Outcome),
    Unexpected(Outcome),
}

impl TestLogLineDiscriminant {
    pub fn new(s: &str) -> Option<Self> {
        let outcome = Outcome::from_ambiguous::<_, extra::Default>;

        match s {
            "START" => Some(Self::Start),
            "INFO" => Some(Self::Info),
            _ => {
                if let Some(s) = s.strip_prefix("UNEXPECTED-") {
                    outcome(s).map(Self::Unexpected)
                } else if let Some(s) = s.strip_prefix("KNOWN-INTERMITTENT-") {
                    outcome(s).map(Self::Expected)
                } else {
                    outcome(s).map(Self::Expected)
                }
            }
        }
    }
}

/// The combined sets of [`TestOutcome`] and [`SubtestOutcome`]. Used in [`classify_log_line`]
/// while the set to which an outcome should belong is ambiguous.
#[derive(Clone, Copy, Debug)]
enum Outcome {
    Pass,
    Fail,
    Skip,
    Crash,
    Timeout,
    Error,
    Ok,
    NotRun,
}

impl Outcome {
    pub fn from_ambiguous<'a, I, E>(input: I) -> Option<Self>
    where
        I: Input<'a, Token = char> + StrInput<'a, char>,
        E: ParserExtra<'a, I>,
        E::Context: Default,
        E::State: Default,
    {
        choice((
            TestOutcome::parser::<I, E>().map(|o| match o {
                TestOutcome::Pass => Self::Pass,
                TestOutcome::Fail => Self::Fail,
                TestOutcome::Timeout => Self::Timeout,
                TestOutcome::Crash => Self::Crash,
                TestOutcome::Error => Self::Error,
                TestOutcome::Skip => Self::Skip,
                TestOutcome::Ok => Self::Ok,
            }),
            SubtestOutcome::parser::<I, E>().map(|o| match o {
                SubtestOutcome::Pass => Self::Pass,
                SubtestOutcome::Fail => Self::Fail,
                SubtestOutcome::Timeout => Self::Timeout,
                SubtestOutcome::NotRun => Self::NotRun,
            }),
        ))
        .parse(input)
        .into_output()
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) struct RecognizedLogLineParseError {
    pub line_num: u64,
    pub kind: RecognizedLogLineParseErrorKind,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) enum RecognizedLogLineParseErrorKind {
    Timestamp {
        source: chrono::ParseError,
        span: LogLineSpans,
    },
    UnrecognizedDiscriminant {
        discriminant_kind: SuiteOrTest,
        span: LogLineSpans,
    },
    TestStart(ParseTestStartError),
    ExpectedTestEnd(ParseExpectedTestEndError),
    UnexpectedTestEnd(ParseUnexpectedTestEndError),
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) enum ParseTestStartError {
    SectionDividerBwDiscriminantAndTestPath { span: LogLineSpans },
    ParseTestPath { inner: TestPathParseError },
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) enum ParseExpectedTestEndError {
    SectionDividerBwDiscriminantAndTestPath { span: LogLineSpans },
    TestPath { inner: TestPathParseError },
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) enum ParseUnexpectedTestEndError {
    SectionDividerBwDiscriminantAndTestPath { span: LogLineSpans },
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub(super) struct TestPathParseError {
    pub discriminant_span: LogLineSpans,
    pub test_path_span: LogLineSpans,
    pub msg: String,
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
#[error("see above errors for more details")]
pub(super) struct CheckErrorSink;

fn parse_rest<'a, I, E, T, S>() -> impl Parser<'a, I, (&'a S, I::Span), E> + Copy
where
    S: ?Sized + 'a,
    I: Input<'a, Token = T> + SliceInput<'a, Slice = &'a S> + ValueInput<'a>,
    E: ParserExtra<'a, I>,
{
    any()
        .repeated()
        .to_slice()
        .map_with(|rest, e| (rest, e.span()))
}

fn mozlog_test_message_section_divider<'a, I, E>() -> impl Parser<'a, I, (), E> + Copy
where
    I: Input<'a, Token = char>,
    E: ParserExtra<'a, I>,
{
    just(" | ").to(())
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
struct LogLineClassificationResult {
    inner: Result<LogLineKind, CheckErrorSink>,
    should_save_spans: bool,
}

fn classify_log_line<'a>(
    browser: Browser,
    line_num: u64,
    s: &'a str,
    slice_start: usize, // TODO: maybe confusing with `s`' start?
    unrecoverable_err_sink: &'a mut dyn FnMut(RecognizedLogLineParseError),
) -> LogLineClassificationResult {
    let log_line = {
        let any = || {
            any::<
                &str,
                Full<Simple<char>, (&mut bool, &mut dyn FnMut(RecognizedLogLineParseError)), ()>,
            >()
        }; // TODO: ew, dis bad, better plz?

        // i.e., something of the form:
        // - `[task] `
        // - `[task 2024-08-02T22:11:54.874Z] `
        // - `[task.something] `
        // - `[task.something 2024-08-02T22:11:54.874Z] `
        let taskcluster_layer = {
            let log_source = any()
                .and_is(just(" ").not())
                .and_is(just("]").not())
                .repeated()
                .to_slice();
            let timestamp = any()
                .and_is(just("]").not())
                .repeated()
                .to_slice()
                .map_with(|raw, e| (raw, e.span()));

            log_source
                .ignore_then(just(" ").ignore_then(timestamp).or_not())
                .delimited_by(just("["), just("] "))
        };

        // i.e., something of the form `22:11:54     INFO - `
        // TODO: narrow to only parsing two digits?
        let script_logger_layer = digits(10)
            .separated_by(just(":"))
            .exactly(3)
            .ignored()
            .then(
                just("     ")
                    .ignore_then(ascii::ident())
                    .then_ignore(just(" - ")),
            );

        let discriminant = ascii::ident()
            .separated_by(just('-'))
            .to_slice()
            .map_with(|ident, e| (ident, e.span()));

        let mozlog_message_layer = choice((
            just("SUITE-").to(SuiteOrTest::Suite),
            just("TEST-").to(SuiteOrTest::Test),
        ))
        .then(discriminant)
        .map(|(discriminant_kind, (discriminant, discriminant_span))| {
            RecognizedLogLineDiscriminant {
                discriminant_kind,
                discriminant,
                discriminant_span,
            }
        });

        taskcluster_layer
            .then(
                script_logger_layer
                    .or_not()
                    .ignore_then(mozlog_message_layer.or_not()),
            )
            .map(|(raw_timestamp, discriminant)| LogLayers {
                raw_timestamp,
                discriminant,
            })
            .or_not()
            .then(parse_rest())
            .map_with(move |parsed, e| {
                let (should_save_spans, unrecoverable_err_sink) = e.state();

                let (log_layers, rest) = parsed;

                classify_log_line_inner(
                    browser,
                    log_layers,
                    rest,
                    line_num,
                    slice_start,
                    should_save_spans,
                    unrecoverable_err_sink,
                )
            })
    };

    let mut s = s;

    // Windows reports may have carriage returns at the end of some (but not all) lines.
    if let Some(stripped) = s.strip_suffix('\r') {
        s = stripped;
    }

    let mut should_save_spans = false;
    let res = if s.is_empty() {
        Ok(LogLineKind::Other)
    } else {
        log_line
            .parse_with_state(s, &mut (&mut should_save_spans, unrecoverable_err_sink))
            .into_result()
            .map_err(|e| {
                let e = e.first().unwrap();
                log::error!("failed to parse log line: {e}");
                CheckErrorSink
            })
            .and_then(|e| e)
    };

    LogLineClassificationResult {
        inner: res,
        should_save_spans,
    }
}

struct LogLayers<'a> {
    raw_timestamp: Option<(&'a str, SimpleSpan)>,
    discriminant: Option<RecognizedLogLineDiscriminant<'a>>,
}

#[derive(Clone, Debug)]
struct RecognizedLogLineDiscriminant<'a> {
    discriminant_kind: SuiteOrTest,
    discriminant: &'a str,
    discriminant_span: SimpleSpan,
}

fn classify_log_line_inner(
    browser: Browser,
    log_layers: Option<LogLayers<'_>>,
    rest: (&str, SimpleSpan),
    line_num: u64,
    slice_start: usize,
    should_save_spans: &mut &mut bool,
    mut unrecoverable_err_sink: impl FnMut(RecognizedLogLineParseError),
) -> Result<LogLineKind, CheckErrorSink> {
    let (rest, rest_span) = rest;

    let mut save_span = |simple_span: SimpleSpan| {
        **should_save_spans = true;
        LogLineSpans {
            offset_in_buf: slice_start + simple_span.start,
            length: simple_span.end - simple_span.start,
        }
    };

    let mut unrecoverable_err_sink =
        |kind| unrecoverable_err_sink(RecognizedLogLineParseError { line_num, kind });

    let Some(LogLayers {
        raw_timestamp,
        discriminant,
    }) = log_layers
    else {
        // We expect log lines to have log layers, but if this is a multi-line message, we may not
        // have it available.
        return Ok(LogLineKind::Other);
    };

    let (raw_timestamp, timestamp_span) = match raw_timestamp {
        Some(some) => some,
        None => match discriminant {
            Some(_) => panic!("lolwut, dunno what to do with a discriminant but no timestamp"),
            None => match rest {
                "Aborting task..." => return Ok(LogLineKind::AbortingTask),
                _ => return Ok(LogLineKind::Other),
            },
        },
    };

    let timestamp = match DateTime::parse_from_rfc3339(raw_timestamp) {
        Ok(ok) => ok,
        Err(source) => {
            unrecoverable_err_sink(RecognizedLogLineParseErrorKind::Timestamp {
                span: save_span(timestamp_span),
                source,
            });
            return Err(CheckErrorSink);
        }
    };

    let Some(RecognizedLogLineDiscriminant {
        discriminant_kind,
        discriminant,
        discriminant_span,
    }) = discriminant
    else {
        return Ok(LogLineKind::Other);
    };

    let test = |kind| Some(LogLineKind::Test(TestLogLine { timestamp, kind }));

    macro_rules! section_divider {
        ($rest:expr, $rest_span:expr, $map_err:expr) => {{
            let rest = $rest;
            let rest_span = $rest_span;
            let map_err = $map_err;
            match mozlog_test_message_section_divider::<'_, _, Full<EmptyErr, (), ()>>()
                .ignore_then(parse_rest())
                .parse(rest.map_span(move |span| {
                    SimpleSpan::new(rest_span.start + span.start, rest_span.start + span.end)
                }))
                .into_output()
            {
                Some(some) => Ok(some),
                None => Err(map_err(save_span(rest_span))),
            }
        }};
    }

    'kind: {
        match discriminant_kind {
        SuiteOrTest::Test => match TestLogLineDiscriminant::new(discriminant) {
            Some(TestLogLineDiscriminant::Start) => {
                let (rest, rest_span) = match section_divider!(
                    rest,
                    rest_span,
                    |span| {
                        RecognizedLogLineParseErrorKind::TestStart(
                            ParseTestStartError::SectionDividerBwDiscriminantAndTestPath { span }
                        )
                    }
                ) {
                    Ok(ok) => ok,
                    Err(e) => {
                        unrecoverable_err_sink(e);
                        break 'kind None;
                    }
                };

                if let Err(e) = TestEntryPath::from_execution_report(browser, rest)
                {
                    unrecoverable_err_sink(
                        RecognizedLogLineParseErrorKind::TestStart(
                                ParseTestStartError::ParseTestPath {
                                    inner:
                            TestPathParseError {
                                discriminant_span: save_span(discriminant_span),
                                test_path_span: save_span(rest_span),
                                msg: e.to_string(),
                            },
                                }
                        ),
                    );
                    break 'kind None;
                }

                test(
                    TestLogLineKind::Start {
                        test_url_path: save_span(rest_span),
                    },
                )
            }
            Some(TestLogLineDiscriminant::Info) => {
                // TODO: expected for test: `TEST-INFO expected <outcome> | took <took>ms`
                // TODO: expected for subtest: `TEST-INFO | expected <outcome>`

                test(TestLogLineKind::Info)
            }
            Some(TestLogLineDiscriminant::Expected(_outcome)) => {
                let (rest, rest_span) = match section_divider!(
                    rest,
                    rest_span,
                    |span| {
                        RecognizedLogLineParseErrorKind::ExpectedTestEnd(
                            ParseExpectedTestEndError::SectionDividerBwDiscriminantAndTestPath { span }
                        )
                    }
                ) {
                    Ok(ok) => ok,
                    Err(e) => {
                        unrecoverable_err_sink(e);
                        break 'kind None;
                    }
                };

                if rest.starts_with("leakcheck") {
                    break 'kind test(TestLogLineKind::LeakCheck);
                }

                let rest = rest.map_span(|span| {
                    SimpleSpan::new(
                        rest_span.start + span.start,
                        rest_span.start + span.end,
                    )
                });
                let [(test_path, test_path_span), (_took_section, _took_span)] = {
                    let res = any::<_, Full<Simple<char>, &str, ()>>()
                        .and_is(mozlog_test_message_section_divider().not())
                        .repeated()
                        .to_slice()
                        .map_with(|section, e| (section, e.span()))
                        .separated_by(mozlog_test_message_section_divider())
                        .collect_exactly()
                        .parse(rest)
                        .into_result()
                        .map_err(|_e| {
                            unrecoverable_err_sink(
                                RecognizedLogLineParseErrorKind::ExpectedTestEnd(
                                    ParseExpectedTestEndError::SectionDividerBwDiscriminantAndTestPath {
                                        span: save_span(rest_span),
                                    },
                                ),
                            );
                            CheckErrorSink
                        });
                    match res {
                        Ok(ok) => ok,
                        Err(CheckErrorSink) => break 'kind None,
                    }
                };

                if let Err(e) =
                    TestEntryPath::from_execution_report(browser, test_path)
                {
                    unrecoverable_err_sink(
                        RecognizedLogLineParseErrorKind::ExpectedTestEnd(
                            ParseExpectedTestEndError::TestPath {
                                inner: TestPathParseError {
                                    discriminant_span: save_span(discriminant_span),
                                    test_path_span: save_span(test_path_span),
                                    msg: e.to_string(),
                                },
                            },
                        ),
                    );
                    break 'kind None;
                }

                test(TestLogLineKind::FinishTestExpected {
                    _test_name: save_span(test_path_span),
                })
            }
            Some(TestLogLineDiscriminant::Unexpected(_outcome)) => {
                let (rest, rest_span) = match section_divider!(
                    rest,
                    rest_span,
                    |span| {
                        RecognizedLogLineParseErrorKind::UnexpectedTestEnd(
                            ParseUnexpectedTestEndError::SectionDividerBwDiscriminantAndTestPath { span }
                        )
                    }
                ) {
                    Ok(ok) => ok,
                    Err(e) => {
                        unrecoverable_err_sink(e);
                        break 'kind None;
                    }
                };

                // TODO: needed?
                if rest.starts_with("leakcheck") {
                    break 'kind test(TestLogLineKind::LeakCheck);
                }

                let rest = rest.map_span(|span| {
                    SimpleSpan::new(
                        rest_span.start + span.start,
                        rest_span.start + span.end,
                    )
                });
                let [
                    (test_path, test_path_span),
                    (_expected_section, _expected_span)
                ] = {
                    let res = any::<_, Full<Simple<char>, &str, ()>>()
                        .and_is(mozlog_test_message_section_divider().not())
                        .repeated()
                        .to_slice()
                        .map_with(|section, e| (section, e.span()))
                        .separated_by(mozlog_test_message_section_divider())
                        .collect_exactly()
                        .parse(rest)
                        .into_result()
                        .map_err(|_e| {
                            unrecoverable_err_sink(
                                RecognizedLogLineParseErrorKind::UnexpectedTestEnd(
                                    ParseUnexpectedTestEndError::SectionDividerBwDiscriminantAndTestPath {
                                        span: save_span(rest_span),
                                    },
                                ),
                            );
                            CheckErrorSink
                        });
                    match res {
                        Ok(ok) => ok,
                        Err(CheckErrorSink) => break 'kind None,
                    }
                };

                if let Err(e) =
                    TestEntryPath::from_execution_report(browser, test_path)
                {
                    unrecoverable_err_sink(
                        RecognizedLogLineParseErrorKind::ExpectedTestEnd(
                            ParseExpectedTestEndError::TestPath {
                                inner: TestPathParseError {
                                    discriminant_span: save_span(discriminant_span),
                                    test_path_span: save_span(test_path_span),
                                    msg: e.to_string(),
                                },
                            },
                        ),
                    );
                    break 'kind None;
                }

                test(TestLogLineKind::FinishTestUnexpected)
            }
            None => {
                unrecoverable_err_sink(
                    RecognizedLogLineParseErrorKind::UnrecognizedDiscriminant {
                        discriminant_kind,
                        span: save_span(discriminant_span),
                    },
                );
                None
            }
        }
        SuiteOrTest::Suite => {
            if let Some(kind) = SuiteLogLineKind::new(discriminant)
            {
                Some(LogLineKind::Suite(SuiteLogLine { timestamp, kind }))
            } else {
                unrecoverable_err_sink(
                    RecognizedLogLineParseErrorKind::UnrecognizedDiscriminant {
                            discriminant_kind,
                        span: save_span(discriminant_span),
                    },
                );
                None
            }
        }
    } }.ok_or(CheckErrorSink)
}

#[test]
fn classify_good_lines() {
    macro_rules! assert_good_parse_eq {
        ($line:expr, $should_save_spans:expr, $expected:expr) => {
            let mut errs = vec![];
            let res = classify_log_line(Browser::Firefox, 0, $line, 0, &mut |e| errs.push(e));
            if !errs.is_empty() {
                for err in &errs {
                    eprintln!("got unexpected test log line error: {err:#?}");
                }
            }
            assert_eq!(
                res,
                LogLineClassificationResult {
                    should_save_spans: $should_save_spans,
                    inner: Ok($expected),
                }
            );
            assert!(errs.is_empty());
        };
    }

    let line = "[task 2024-08-02T22:11:54.874Z] 22:11:54     INFO - TEST-START | /_mozilla/webgpu/cts/webgpu/shader/validation/decl/var/cts.https.html?q=webgpu:shader,validation,decl,var:initializer_kind:*";
    assert_good_parse_eq!(
        line,
        true,
        LogLineKind::Test(TestLogLine {
            timestamp: DateTime::parse_from_rfc3339("2024-08-02T22:11:54.874Z").unwrap(),
            kind: TestLogLineKind::Start {
                test_url_path: LogLineSpans {
                    offset_in_buf: 65,
                    length: 124,
                }
            }
        })
    );

    let line = "[task 2024-08-02T22:17:15.803Z] 22:17:15     INFO - TEST-OK | /_mozilla/webgpu/cts/webgpu/api/operation/shader_module/compilation_info/cts.https.html?q=webgpu:api,operation,shader_module,compilation_info:getCompilationInfo_returns:* | took 9443ms";
    assert_good_parse_eq!(
        line,
        true,
        LogLineKind::Test(TestLogLine {
            timestamp: DateTime::parse_from_rfc3339("2024-08-02T22:17:15.803Z").unwrap(),
            kind: TestLogLineKind::FinishTestExpected {
                _test_name: LogLineSpans {
                    offset_in_buf: 62,
                    length: 170,
                },
                // outcome: TestOutcome::Ok,
                // took: Duration::from_millis(9443),
            }
        })
    );
}

#[test]
fn classify_bad_lines() {
    let mut errs = Vec::new();
    macro_rules! assert_errs {
        ($line:expr, $should_save_spans:expr, $errs:expr) => {
            errs.clear();
            assert_eq!(
                classify_log_line(Browser::Firefox, 0, $line, 0, &mut |e| errs.push(e)),
                LogLineClassificationResult {
                    inner: Err(CheckErrorSink),
                    should_save_spans: $should_save_spans,
                }
            );
            assert_eq!(errs, $errs);
        };
    }

    let line =
        "[task 2024-08-02T22:11:54.874Z] 22:11:54     INFO - TEST-DERP | /valid/test/path.https.html";
    assert_errs!(
        line,
        true,
        vec![RecognizedLogLineParseError {
            line_num: 0,
            kind: RecognizedLogLineParseErrorKind::UnrecognizedDiscriminant {
                discriminant_kind: SuiteOrTest::Test,
                span: LogLineSpans {
                    offset_in_buf: 57,
                    length: 4,
                }
            }
        }]
    );

    let line =
        "[task 2024-08-02T22:11:54.874Z] 22:11:54     INFO - TEST-START | bruh idk this ain't valid";
    assert_errs!(
        line,
        true,
        vec![RecognizedLogLineParseError {
            line_num: 0,
            kind: RecognizedLogLineParseErrorKind::TestStart(ParseTestStartError::ParseTestPath {
                inner: TestPathParseError {
                    discriminant_span: LogLineSpans {
                        offset_in_buf: 57,
                        length: 5,
                    },
                    test_path_span: LogLineSpans {
                        offset_in_buf: 65,
                        length: 25,
                    },
                    msg: crate::wpt::path::ExecutionReportPathError {
                        test_url_path: "bruh idk this ain't valid"
                    }
                    .to_string()
                }
            })
        }]
    );
}
