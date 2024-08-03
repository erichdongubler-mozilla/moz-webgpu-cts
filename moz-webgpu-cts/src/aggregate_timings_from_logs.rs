mod log_line_reader;

use std::{
    collections::BTreeMap,
    fs::File,
    io::{stdout, BufReader},
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::Context;
use chrono::{DateTime, FixedOffset};
use lazy_format::lazy_format;
use log_line_reader::{
    LogLine, LogLineKind, LogLineReader, LogLineSpans, ParseExpectedTestEndError,
    ParseTestStartError, ParseUnexpectedTestEndError, RecognizedLogLineParseError,
    RecognizedLogLineParseErrorKind, SuiteLogLine, SuiteLogLineKind, TestLogLine, TestLogLineKind,
    TestPathParseError,
};
use miette::{diagnostic, LabeledSpan, Report, SourceSpan};

use crate::{
    wpt::path::{Browser, TestEntryPath},
    AlreadyReportedToCommandline,
};

pub(crate) fn aggregate_timings_from_logs(
    browser: Browser,
    log_paths: Vec<PathBuf>,
) -> Result<(), AlreadyReportedToCommandline> {
    if log_paths.is_empty() {
        log::error!(concat!(
            "no log file(s) specified; ",
            "this command doesn't make sense without them!"
        ));
        return Err(AlreadyReportedToCommandline);
    }

    // TODO: Do 'em all in parallel!

    let mut test_timings_by_file = BTreeMap::new();
    let mut buf = String::with_capacity(512);
    for log_path in log_paths.iter() {
        let log_path_entry = test_timings_by_file.entry(log_path).or_default();
        match process_log(browser, log_path, log_path_entry, &mut buf) {
            Ok(()) | Err(AlreadyReportedToCommandline) => (),
        }
    }

    let mut serializer = serde_json::Serializer::new(stdout().lock());
    use serde::Serializer as _;
    serializer
        .collect_map(test_timings_by_file.into_iter().map(|(k, v)| {
            (
                k,
                v.into_iter()
                    .map(|(k, v)| (k.runner_url_path().to_string(), v.as_secs()))
                    .collect::<BTreeMap<_, _>>(),
            )
        }))
        .unwrap();
    let _ = serializer.into_inner();

    Ok(())
}

fn process_log(
    browser: Browser,
    log_path: &Path,
    log_path_entry: &mut BTreeMap<TestEntryPath<'_>, Duration>,
    buf: &mut String,
) -> Result<(), AlreadyReportedToCommandline> {
    let mut reader = LogLineReader::new(browser, BufReader::new(File::open(log_path).unwrap()));
    let mut next_line = |buf: &mut _| {
        let mut errs = Vec::new();

        reader.next_log_line(buf, &mut |e| errs.push(e)).map(|res| {
            res.inspect(|_| assert!(errs.is_empty()))
                .with_context(|| {
                    format!("failed to read next log line of `{}`", log_path.display())
                })
                .map_err(|e| {
                    for e in errs {
                        render_test_log_line_err(log_path, &*buf, e);
                    }
                    log::error!("{e:?}");
                    AlreadyReportedToCommandline
                })
        })
    };

    #[derive(Debug)]
    enum TestLogParserState<'a> {
        WaitingForSuiteStart,
        SuiteStarted,
        TestStarted {
            _timestamp: DateTime<FixedOffset>,
            _test_url_path: &'a TestEntryPath<'static>,
        },
    }

    struct Unexpected {}

    fn expect_line<N, T, F>(
        state: &TestLogParserState,
        mut next_line: N,
        f: F,
    ) -> Result<T, AlreadyReportedToCommandline>
    where
        N: FnMut() -> Option<Result<LogLine, AlreadyReportedToCommandline>>,
        F: FnOnce(&LogLineKind) -> Result<T, Unexpected>,
    {
        match next_line() {
            Some(line) => {
                let line = line?;
                let LogLine { _line_num: _, kind } = &line;
                match f(kind) {
                    Ok(ok) => Ok(ok),
                    Err(Unexpected {}) => {
                        // TODO: diagnostic render plz
                        log::error!("was in state {state:?} and got unexpected {line:?}");
                        Err(AlreadyReportedToCommandline)
                    }
                }
            }
            None => {
                log::error!("was in state {state:?} and got end of log, expected something else");
                Err(AlreadyReportedToCommandline)
            }
        }
    }

    let extract_test_url_path =
        |test_url_path: &LogLineSpans,
         buf: &str|
         -> Result<TestEntryPath<'static>, AlreadyReportedToCommandline> {
            let test_url_path = test_url_path.get_from(buf);
            TestEntryPath::from_execution_report(browser, test_url_path)
                .map(|p| p.into_owned())
                .map_err(|e| {
                    // TODO: good enough? Probably lacking context.
                    log::error!("{e}");
                    AlreadyReportedToCommandline
                })
        };

    // TODO: Use control flow, not parser state? 🤔
    let mut next_line = |buf: &mut String| {
        buf.clear();
        next_line(buf)
    };
    loop {
        let found_suite_start = expect_line(
            &TestLogParserState::WaitingForSuiteStart,
            || next_line(buf),
            |kind| match kind {
                LogLineKind::Suite(SuiteLogLine {
                    kind: SuiteLogLineKind::Start,
                    timestamp: _,
                }) => Ok(true),
                LogLineKind::Other => Ok(false),
                _ => Err(Unexpected {}),
            },
        )?;

        if found_suite_start {
            break;
        }
    }

    let mut started_test_url_path;
    let mut started_test_timestamp;
    loop {
        let inner = expect_line(
            &TestLogParserState::SuiteStarted,
            || next_line(buf),
            |kind| match kind {
                &LogLineKind::Test(TestLogLine {
                    kind: TestLogLineKind::Start { ref test_url_path },
                    timestamp,
                }) => Ok(Some((timestamp, test_url_path.clone()))),
                LogLineKind::Other => Ok(None),
                _ => Err(Unexpected {}),
            },
        )?;

        if let Some((timestamp, inner)) = inner {
            started_test_url_path = extract_test_url_path(&inner, buf)?;
            started_test_timestamp = timestamp;
            break;
        }
    }
    loop {
        // Just ignore everything but the start of the next test.
        enum LineAfterTestStart {
            StartedNewTest {
                timestamp: DateTime<FixedOffset>,
                test_url_path: LogLineSpans,
            },
            FinishedSuite {
                timestamp: DateTime<FixedOffset>,
            },
            Ignore,
            Aborted,
        }
        let line_after_test_start = expect_line(
            &TestLogParserState::TestStarted {
                _timestamp: started_test_timestamp,
                _test_url_path: &started_test_url_path,
            },
            || next_line(buf),
            |kind| match kind {
                LogLineKind::AbortingTask => Ok(LineAfterTestStart::Aborted),
                &LogLineKind::Suite(SuiteLogLine {
                    timestamp,
                    ref kind,
                }) => match kind {
                    SuiteLogLineKind::End => Ok(LineAfterTestStart::FinishedSuite { timestamp }),
                    SuiteLogLineKind::Start => Ok(LineAfterTestStart::Ignore),
                },
                &LogLineKind::Test(TestLogLine {
                    timestamp,
                    ref kind,
                }) => match kind {
                    TestLogLineKind::Start { test_url_path } => {
                        Ok(LineAfterTestStart::StartedNewTest {
                            timestamp,
                            test_url_path: test_url_path.clone(),
                        })
                    }
                    // TODO: We can get back a few seconds of precision if we handle these
                    // properly.
                    TestLogLineKind::LeakCheck
                    | TestLogLineKind::Info
                    | TestLogLineKind::FinishTestExpected { .. }
                    | TestLogLineKind::FinishTestUnexpected => Ok(LineAfterTestStart::Ignore),
                },
                LogLineKind::Other => Ok(LineAfterTestStart::Ignore),
            },
        )?;

        let mut add_test_entry = |path, timestamp: DateTime<FixedOffset>| {
            log_path_entry.insert(
                path,
                timestamp
                    .signed_duration_since(started_test_timestamp)
                    .abs()
                    .to_std()
                    .unwrap(),
            );
        };
        match line_after_test_start {
            LineAfterTestStart::StartedNewTest {
                timestamp,
                test_url_path,
            } => {
                add_test_entry(started_test_url_path, timestamp);
                started_test_timestamp = timestamp;
                started_test_url_path = extract_test_url_path(&test_url_path, buf)?;
                continue;
            }
            LineAfterTestStart::FinishedSuite { timestamp } => {
                add_test_entry(started_test_url_path, timestamp);
                break;
            }
            LineAfterTestStart::Ignore => continue,
            LineAfterTestStart::Aborted => break,
        }
    }

    Ok(())
}

impl From<LogLineSpans> for SourceSpan {
    fn from(value: LogLineSpans) -> Self {
        value.buf_slice_idx().into()
    }
}

fn render_test_log_line_err(log_path: &Path, buf: &str, e: RecognizedLogLineParseError) {
    let RecognizedLogLineParseError { line_num, kind } = e;
    // TODO: use `camino` paths, save everyone some pain 😭
    let log_and_line_prepend =
        lazy_format!("{log_path:?}:{line_num}: failed to parse `TEST` log line: ");
    let test_path_parse_labels = |err| {
        let TestPathParseError {
            discriminant_span,
            test_path_span,
            msg,
        } = err;
        vec![
            LabeledSpan::at(
                discriminant_span,
                "indicates that the test path will be started",
            ),
            LabeledSpan::new_primary_with_span(Some(msg), test_path_span),
        ]
    };
    let section_divider_bw = |span, after_what, before_what| {
        diagnostic!(
            labels = vec![LabeledSpan::new_primary_with_span(None, span)],
            "{}expected a `mozlog` section divider (` | `) after {} and before {}",
            log_and_line_prepend,
            after_what,
            before_what
        )
    };
    let diagnostic = match kind {
        RecognizedLogLineParseErrorKind::Timestamp { source, span } => {
            diagnostic!(
                labels = vec![LabeledSpan::new_primary_with_span(None, span)],
                "{log_and_line_prepend}{source}"
            )
        }
        RecognizedLogLineParseErrorKind::UnrecognizedDiscriminant {
            discriminant_kind,
            span,
        } => {
            let discriminant = span.get_from(buf);
            diagnostic!(
                labels = vec![LabeledSpan::new_primary_with_span(None, span)],
                "{log_and_line_prepend}unrecognized discriminant {:?} for discriminant {:?}",
                discriminant,
                discriminant_kind,
            )
        }
        RecognizedLogLineParseErrorKind::TestStart(inner) => match inner {
            ParseTestStartError::SectionDividerBwDiscriminantAndTestPath { span } => {
                section_divider_bw(span, "`TEST-START`", "test path")
            }
            ParseTestStartError::ParseTestPath { inner } => diagnostic!(
                labels = test_path_parse_labels(inner),
                "{log_and_line_prepend}failed to parse `START`ed test path"
            ),
        },
        RecognizedLogLineParseErrorKind::ExpectedTestEnd(e) => match e {
            ParseExpectedTestEndError::SectionDividerBwDiscriminantAndTestPath { span } => {
                section_divider_bw(span, "`TEST-*`", "test path")
            }
            ParseExpectedTestEndError::TestPath { inner } => diagnostic!(
                labels = test_path_parse_labels(inner),
                "{log_and_line_prepend}failed to parse test path"
            ),
        },
        RecognizedLogLineParseErrorKind::UnexpectedTestEnd(e) => match e {
            ParseUnexpectedTestEndError::SectionDividerBwDiscriminantAndTestPath { span } => {
                section_divider_bw(span, "TEST-UNEXPECTED-<outcome>", "test path")
            }
        },
    }
    .with_help(concat!(
        "If this isn't a malformed edit of yours, it's likely a bug in `",
        env!("CARGO_BIN_NAME"),
        "`. You should file an issue upstream!"
    ));
    let diagnostic = Report::new(diagnostic).with_source_code(buf.to_owned());
    eprintln!("{diagnostic:?}")
}
