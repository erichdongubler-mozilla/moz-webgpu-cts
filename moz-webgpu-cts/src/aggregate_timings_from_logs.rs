mod log_line_reader;

use std::{
    fs::File,
    io::BufReader,
    ops::ControlFlow,
    path::{Path, PathBuf},
    time::Duration,
};

use format::lazy_format;
use indexmap::IndexMap;
use log_line_reader::{
    LogLine, LogLineKind, LogLineReader, LogLineSpans, ParseExpectedTestEndError, TestLogLine,
    TestLogLineKind, TestLogLineParseError, TestLogLineParseErrorKind, TestPathParseError,
    TookParseError,
};
use miette::{IntoDiagnostic, LabeledSpan, Report, SourceSpan};

use crate::{
    wpt::{
        metadata::{SubtestOutcome, TestOutcome},
        path::Browser,
    },
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

    enum TestLogParserState {
        ReadyForTest,
        StartedTest {
            test_name: LogLineSpans,
            // reported_subtest: bool,
        },
    }

    #[derive(Clone, Debug)]
    enum TestLogEvent {
        Test {
            test_name: String,
            outcome: TestOutcome,
            duration: Duration,
            subtests_noted: IndexMap<String, SubtestOutcome>,
        },
    }

    // fn find<T, E>(line_handler: impl FindLogLine<T, E>) -> Result<Option<TestLogEvent>, E> {}

    #[derive(Clone, Copy, Debug)]
    enum BufferClear {
        All,
        Truncate(usize),
        No,
    }

    trait FindLogLine<T, E> {
        fn handle_line(
            &mut self,
            buf_len_before_read: usize,
            log_line: LogLine,
        ) -> ControlFlow<BufferClear, Result<Option<T>, E>>;
        fn handle_eof(&mut self);
    }

    // TODO: Do 'em all in parallel!

    let log_path = log_paths.first().unwrap();
    let mut reader = LogLineReader::new(browser, BufReader::new(File::open(log_path).unwrap()));

    let mut state = TestLogParserState::ReadyForTest;
    let mut buf = String::with_capacity(512);
    let mut errs = Vec::new();
    let mut read_line = |buf: &mut String| {
        let buf_len_before_read = buf.len();

        let line_res = reader
            .next_log_line(buf, &mut |e| errs.push(e))
            .map(|res| res.into_diagnostic());
        match line_res {
            Some(Ok(line)) => {
                assert!(errs.is_empty());
                Ok(Some(line))
            }
            Some(Err(e)) => {
                for err in errs.drain(..) {
                    render_test_log_line_err(&log_path, &buf, err);
                }
                log::error!("{e}");
                buf.truncate(buf_len_before_read);
                return Err(AlreadyReportedToCommandline);
            }
            None => {
                assert!(errs.is_empty());
                Ok(None)
            }
        }
    };
    // let mut last_test_span = None;
    'read_loop: loop {
        let (started_test_span, started_test_line_num, started_test_line_start_idx_in_buf) = 'start_test: loop {
            let len = buf.len();
            let line_start_idx_in_buf = len;
            let LogLine { line_num, kind } = match read_line(&mut buf)? {
                Some(some) => some,
                None => break 'read_loop,
            };
            match kind {
                LogLineKind::Other => (/* s' cool */),
                LogLineKind::Test(TestLogLine { timestamp, kind }) => match kind {
                    TestLogLineKind::StartTest { test_name } => {
                        break 'start_test (test_name, line_num, line_start_idx_in_buf)
                    }
                    TestLogLineKind::LeakCheck => {
                        log::warn!("bruh wat is happening, y u no not leakcheck (line {line_num})");
                    }
                    TestLogLineKind::FinishTestExpected {
                        test_name,
                        outcome,
                        took: _,
                    } => {
                        log::error!(
                            "ocraps, ended a test with {outcome} with no test previously noted as started on line {line_num}: {}",
                            test_name.get_from(&buf)
                        );
                        return Err(AlreadyReportedToCommandline);
                    }
                    TestLogLineKind::FinishSubtest {
                        test_name,
                        subtest_name,
                        outcome,
                    } => {
                        log::error!(
                            "ocraps, ended a subtest with {outcome} with no test previously noted as started on line {line_num}: {} | {}",
                            test_name.get_from(&buf),
                            subtest_name.get_from(&buf)
                        );
                        return Err(AlreadyReportedToCommandline);
                    }
                    TestLogLineKind::FinishTestUnexpected { test_name, outcome } => {
                        log::error!(
                            "ocraps, ended a test with {outcome} with no test previously noted as started on line {line_num}: {}",
                            test_name.get_from(&buf)
                        );
                        return Err(AlreadyReportedToCommandline);
                    }
                    TestLogLineKind::InfoTook { took } => {
                        log::error!("ocraps, ended _something_ with no test previously noted as started on line {line_num}");
                        return Err(AlreadyReportedToCommandline);
                    }
                },
            }
        };

        // Read zero or more subtest results until we get a test result.
        let mut subtests_noted = IndexMap::new();
        let (outcome, duration) = 'end_test: loop {
            let LogLine { line_num, kind } = match read_line(&mut buf)? {
                Some(some) => some,
                None => {
                    // TODO: Maybe this might happen when the task times out?
                    log::error!(
                        "ocraps, log ended before finishing test started on line {}",
                        started_test_line_num
                    );
                    return Err(AlreadyReportedToCommandline);
                }
            };
            match kind {
                LogLineKind::Other => (/* s' cool */),
                LogLineKind::Test(TestLogLine { timestamp, kind }) => match kind {
                    TestLogLineKind::StartTest { test_name } => {
                        log::error!(
                            concat!(
                                "ocraps, started a test on line {} ",
                                "before ending the test that started on line {}: {}",
                            ),
                            line_num,
                            started_test_line_num,
                            test_name.get_from(&buf)
                        );
                        return Err(AlreadyReportedToCommandline);
                    }
                    TestLogLineKind::LeakCheck => {
                        log::warn!("leakcheck on line {line_num} good? teach me plz");
                    }
                    TestLogLineKind::FinishTestExpected {
                        test_name,
                        outcome,
                        took,
                    } => {
                        {
                            let started_test_name = started_test_span.get_from(&buf);
                            let finished_test_name = test_name.get_from(&buf);
                            if started_test_name != finished_test_name {
                                log::error!(
                                    concat!(
                                        "bruh wtf, ",
                                        "started test {:?}, ",
                                        "but then the end event says it's {:?}"
                                    ),
                                    started_test_name,
                                    finished_test_name,
                                )
                            }
                        }
                        break 'end_test (outcome, took);
                    }
                    TestLogLineKind::FinishSubtest {
                        test_name,
                        subtest_name,
                        outcome,
                    } => {
                        {
                            let started_test_name = started_test_span.get_from(&buf);
                            let finished_test_name = test_name.get_from(&buf);
                            if started_test_name != finished_test_name {
                                log::error!(
                                    concat!(
                                        "bruh wtf, ",
                                        "started test {:?}, ",
                                        "but then the end event says it's {:?}"
                                    ),
                                    started_test_name,
                                    finished_test_name,
                                )
                            }
                        }
                        let subtest_name = subtest_name.get_from(&buf).to_owned();
                        subtests_noted.insert(subtest_name, outcome);
                        return Err(AlreadyReportedToCommandline);
                    }
                    TestLogLineKind::FinishTestUnexpected { test_name, outcome } => {
                        {
                            let started_test_name = started_test_span.get_from(&buf);
                            let finished_test_name = test_name.get_from(&buf);
                            if started_test_name != finished_test_name {
                                log::error!(
                                    concat!(
                                        "bruh wtf, ",
                                        "started test {:?}, ",
                                        "but then the end event says it's {:?}"
                                    ),
                                    started_test_name,
                                    finished_test_name,
                                )
                            }
                        }
                        loop {
                            let LogLine { line_num, kind } = match read_line(&mut buf)? {
                                Some(some) => some,
                                None => {
                                    // TODO: Maybe this might happen when the task times out?
                                    log::error!(
                                        concat!(
                                            "ocraps, log EOF reached before finishing test ",
                                            "started on line {}",
                                        ),
                                        started_test_line_num
                                    );
                                    return Err(AlreadyReportedToCommandline);
                                }
                            };
                            match kind {
                                LogLineKind::Test(TestLogLine { timestamp, kind }) => match kind {
                                    TestLogLineKind::InfoTook { took } => {
                                        break 'end_test (outcome, took);
                                    }
                                    TestLogLineKind::StartTest { test_name } => todo!(),
                                    TestLogLineKind::LeakCheck => todo!(),
                                    TestLogLineKind::FinishTestExpected {
                                        test_name,
                                        outcome,
                                        took,
                                    } => todo!(),
                                    TestLogLineKind::FinishSubtest {
                                        test_name,
                                        subtest_name,
                                        outcome,
                                    } => todo!(),
                                    TestLogLineKind::FinishTestUnexpected {
                                        test_name,
                                        outcome,
                                    } => todo!(),
                                },
                                LogLineKind::Other => (),
                            }
                        }
                    }
                    TestLogLineKind::InfoTook { took } => {
                        log::error!(
                            concat!(
                                "ocraps, ended _something_ ",
                                "with no test previously noted as started on line {}"
                            ),
                            line_num
                        );
                        return Err(AlreadyReportedToCommandline);
                    }
                },
            }
        };
        println!(
            "{:?}",
            TestLogEvent::Test {
                test_name: started_test_span.get_from(&buf).to_owned(),
                outcome,
                duration,
                subtests_noted,
            }
        );
        buf.truncate(started_test_line_start_idx_in_buf);
    }

    buf.clear();
    errs.clear();

    Ok(())
}

fn render_test_log_line_err(log_path: &Path, buf: &str, e: TestLogLineParseError) {
    impl From<LogLineSpans> for SourceSpan {
        fn from(value: LogLineSpans) -> Self {
            value.buf_slice_idx().into()
        }
    }

    let TestLogLineParseError { line_num, kind } = e;
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
    let diagnostic = match kind {
        TestLogLineParseErrorKind::ParseTimestamp { source, span } => {
            miette::diagnostic!(
                labels = vec![LabeledSpan::new_primary_with_span(None, span)],
                "{log_and_line_prepend}{source}"
            )
        }
        TestLogLineParseErrorKind::UnrecognizedDiscriminant { span } => {
            let discriminant = span.get_from(&buf);
            miette::diagnostic!(
                labels = vec![LabeledSpan::new_primary_with_span(None, span)],
                "{log_and_line_prepend}unrecognized discriminant {discriminant:?}"
            )
        }
        TestLogLineParseErrorKind::ParseStartTestPath(inner) => miette::diagnostic!(
            labels = test_path_parse_labels(inner),
            "{log_and_line_prepend}failed to parse `START`ed test path"
        ),
        TestLogLineParseErrorKind::ParseExpectedTestEnd(e) => match e {
            ParseExpectedTestEndError::SplitDivider { span } => {
                miette::diagnostic!(
                    labels = vec![LabeledSpan::new_primary_with_span(None, span)],
                    // TODO: share constant, probably via `Display`?
                    "{log_and_line_prepend}failed to find dividing split (` | `) between a presumed test path and `took` duration",
                )
            }
            ParseExpectedTestEndError::ParseTestPath { inner } => miette::diagnostic!(
                labels = test_path_parse_labels(inner),
                "{log_and_line_prepend}failed to parse `OK`'d test path"
            ),
            ParseExpectedTestEndError::ParseTook { inner } => {
                let log_and_line_prepend = lazy_format!("{log_and_line_prepend}`took` duration ");
                match inner {
                    TookParseError::ParseMillis { span, source } => miette::diagnostic!(
                        labels = vec![LabeledSpan::new_primary_with_span(
                            Some(source.to_string()),
                            span
                        )],
                        "{log_and_line_prepend}had invalid milliseconds count"
                    ),
                    TookParseError::ParseUnit { expected_ms_span } => miette::diagnostic!(
                        labels = vec![LabeledSpan::new_primary_with_span(
                            Some("expected here".to_owned()),
                            expected_ms_span
                        )],
                        "{log_and_line_prepend}of the form `took <count>ms` not found"
                    ),
                }
            }
        },
    }.with_help(concat!(
            "If this isn't a malformed edit of yours, it's likely a bug in `",
            env!("CARGO_BIN_NAME"),
            "`. You should file an issue upstream!"
    ));
    let diagnostic = Report::new(diagnostic).with_source_code(buf.to_owned());
    eprintln!("{diagnostic:?}")
}
