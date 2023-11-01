mod metadata;
mod process_reports;
mod report;
mod shared;

use self::{
    metadata::{
        AnalyzeableProps, BuildProfile, File, Platform, Subtest, SubtestOutcome, Test, TestOutcome,
    },
    process_reports::{MaybeDisabled, OutcomesForComparison, TestOutcomes},
    report::{
        ExecutionReport, RunInfo, SubtestExecutionResult, TestExecutionEntry, TestExecutionResult,
    },
    shared::{Expectation, MaybeCollapsed, NormalizedExpectationPropertyValue, TestPath},
};

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    fs,
    hash::Hash,
    io::{self, BufReader, BufWriter},
    path::{Path, PathBuf},
    process::ExitCode,
    sync::{mpsc::channel, Arc},
};

use clap::{Parser, ValueEnum};
use enumset::EnumSetType;
use format::lazy_format;
use indexmap::{IndexMap, IndexSet};
use miette::{miette, Diagnostic, IntoDiagnostic, NamedSource, Report, SourceSpan, WrapErr};
use path_dsl::path;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use regex::Regex;
use strum::IntoEnumIterator;
use wax::Glob;
use whippit::{metadata::SectionHeader, reexport::chumsky::prelude::Rich};

#[derive(Debug, Parser)]
#[command(about, version)]
struct Cli {
    #[clap(long)]
    gecko_checkout: Option<PathBuf>,
    #[clap(subcommand)]
    subcommand: Subcommand,
}

#[derive(Debug, Parser)]
enum Subcommand {
    /// Adjust test expectations in metadata using `wptreport.json` reports from CI runs covering
    /// Firefox's implementation of WebGPU.
    ///
    /// The general usage of this subcommand is to (1) reset expectations according to some
    /// heuristic, and then (2) extend expectations from more reports later to accommodate any
    /// intermittents that are found. More concretely:
    ///
    /// 1. Pick a `reset-*` preset (which we'll call `RESET_PRESET`). See docs for `preset` for
    ///    more details on making this choice.
    /// 2. Gather reports into path(s) of your choice.
    /// 2. Run `moz-webgpu-cts process-reports --preset=$RESET_PRESET …` against the reports
    ///    you've gathered to cover all new permanent outcomes. If you are confident you picked the
    ///    right `RESET_PRESET`, you may delete the reports you provided to this run.
    /// 3. As unexpected intermittent outcomes are discovered, run `moz-webgpu-cts process-reports
    ///    --preset=merge …` with reports. You may delete the reports after their outcomes have
    ///    been merged in.
    ProcessReports {
        /// Direct paths to report files to be processed.
        report_paths: Vec<PathBuf>,
        /// Cross-platform `wax` globs to enumerate report files to be processed.
        ///
        /// N.B. for Windows users: backslashes are used strictly for escaped characters, and
        /// forward slashes (`/`) are the only valid path separator for these globs.
        #[clap(long = "glob", value_name = "REPORT_GLOB")]
        report_globs: Vec<String>,
        /// A heuristic for resolving differences between current metadata and processed reports.
        ///
        /// When you use this subcommand, you need to use both the `merge` preset and a choice of
        /// `reset-*` heuristic. The choice mostly depends on your taste for regressions in
        /// intermittent outcomes:
        ///
        /// * Is your goal is to make changes to Firefox, and make CI pass again? If so, you
        ///   probably want `reset-contractory`.
        /// * Are you trying to run the `triage` subcommand on a minimized set of expected
        ///   outcomes? If so, you probably want `reset-all`.
        ///
        /// `reset-contradictory` changes expectations to match the set of outcomes observed in the
        /// provided `reports_*` when they are not a strict subset of expected outcomes in
        /// metadata. This is guaranteed to cover new permanent outcomes in metadata, while
        /// minimizing changes to current intermittent outcomes in metadata. It may, however,
        /// result in some intermittent outcomes not being reset to new permanent outcomes.
        ///
        /// `reset-all` changes expectations to match reported outcomes _exactly_. Metadata is not
        /// even considered.
        #[clap(long)]
        preset: ReportProcessingPreset,
    },
    #[clap(name = "fmt")]
    Format,
    Triage,
    ReadTestVariants,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum ReportProcessingPreset {
    Merge,
    ResetContradictory,
    ResetAll,
}

fn main() -> ExitCode {
    env_logger::builder()
        .filter_level(log::LevelFilter::Info)
        .parse_default_env()
        .init();
    run(Cli::parse())
}

fn run(cli: Cli) -> ExitCode {
    let Cli {
        gecko_checkout,
        subcommand,
    } = cli;

    let gecko_checkout = match gecko_checkout
        .map(Ok)
        .unwrap_or_else(search_for_moz_central_ckt)
    {
        Ok(ckt_path) => ckt_path,
        Err(AlreadyReportedToCommandline) => return ExitCode::FAILURE,
    };

    let read_metadata = || -> Result<_, AlreadyReportedToCommandline> {
        let webgpu_cts_meta_parent_dir = {
            path!(
                &gecko_checkout
                    | "testing"
                    | "web-platform"
                    | "mozilla"
                    | "meta"
                    | "webgpu"
                    | "chunked"
            )
        };

        let mut found_err = false;
        let collected =
            read_gecko_files_at(&gecko_checkout, &webgpu_cts_meta_parent_dir, "**/*.ini")?
                .filter_map(|res| match res {
                    Ok(ok) => Some(ok),
                    Err(AlreadyReportedToCommandline) => {
                        found_err = true;
                        None
                    }
                })
                .map(|(p, fc)| (Arc::new(p), Arc::new(fc)))
                .collect::<IndexMap<_, _>>();
        if found_err {
            Err(AlreadyReportedToCommandline)
        } else {
            Ok(collected)
        }
    };

    fn render_metadata_parse_errors<'a>(
        path: &Arc<PathBuf>,
        file_contents: &Arc<String>,
        errors: impl IntoIterator<Item = Rich<'a, char>>,
    ) {
        #[derive(Debug, Diagnostic, thiserror::Error)]
        #[error("{inner}")]
        struct ParseError {
            #[label]
            span: SourceSpan,
            #[source_code]
            source_code: NamedSource,
            inner: Rich<'static, char>,
        }
        let source_code = file_contents.clone();
        for error in errors {
            let span = error.span();
            let error = ParseError {
                source_code: NamedSource::new(path.to_str().unwrap(), source_code.clone()),
                inner: error.clone().into_owned(),
                span: SourceSpan::new(span.start.into(), (span.end - span.start).into()),
            };
            let error = Report::new(error);
            eprintln!("{error:?}");
        }
    }

    match subcommand {
        Subcommand::ProcessReports {
            report_globs,
            report_paths,
            preset,
        } => {
            let report_globs = {
                let mut found_glob_parse_err = false;
                // TODO: I think these can just be args. directly?
                let globs = report_globs
                    .into_iter()
                    .filter_map(|glob| match Glob::diagnosed(&glob) {
                        Ok((glob, _diagnostics)) => Some(glob.into_owned().partition()),
                        Err(diagnostics) => {
                            found_glob_parse_err = true;
                            let error_reports = diagnostics
                                .into_iter()
                                .filter(|diag| {
                                    // N.B.: There should be at least one of these!
                                    diag.severity()
                                        .map_or(true, |sev| sev == miette::Severity::Error)
                                })
                                .map(Report::new_boxed);
                            for report in error_reports {
                                eprintln!("{report:?}");
                            }
                            todo!("render glob parse diagnostics in hard error")
                        }
                    })
                    .collect::<Vec<_>>();

                if found_glob_parse_err {
                    log::error!("failed to parse one or more WPT report globs; bailing");
                    return ExitCode::FAILURE;
                }

                globs
            };

            if report_paths.is_empty() && report_globs.is_empty() {
                log::error!("no report paths specified, bailing");
                return ExitCode::FAILURE;
            }

            let exec_report_paths = {
                let mut found_glob_walk_err = false;
                let files = report_globs
                    .iter()
                    .flat_map(|(base_path, glob)| {
                        glob.walk(base_path)
                            .filter_map(|entry| match entry {
                                // TODO: warn when not ending in `wptreport.json`
                                Ok(entry) => Some(entry.into_path()),
                                Err(e) => {
                                    found_glob_walk_err = true;
                                    let ctx_msg = if let Some(path) = e.path() {
                                        format!(
                                            "failed to enumerate files for glob `{}` at path {}",
                                            glob,
                                            path.display()
                                        )
                                    } else {
                                        format!("failed to enumerate files for glob `{glob}`")
                                    };
                                    let e = Report::msg(e).wrap_err(ctx_msg);
                                    eprintln!("{e:?}");
                                    None
                                }
                            })
                            .collect::<Vec<_>>() // OPT: Can we get rid of this somehow?
                    })
                    .chain(report_paths)
                    .collect::<Vec<_>>();

                if found_glob_walk_err {
                    log::error!("failed to enumerate files with WPT report globs; bailing");
                    return ExitCode::FAILURE;
                }

                files
            };

            if exec_report_paths.is_empty() {
                log::error!("no WPT report files found, bailing");
                return ExitCode::FAILURE;
            }

            log::trace!("working with the following WPT report files: {exec_report_paths:#?}");
            log::info!("working with {} WPT report files", exec_report_paths.len());

            // TODO: If we don't need to consult metadata (because we plan on ignoring everything
            // there, and deleting anything not in reports), then don't even load these.
            let meta_files_by_path = {
                let raw_meta_files_by_path = match read_metadata() {
                    Ok(paths) => paths,
                    Err(AlreadyReportedToCommandline) => return ExitCode::FAILURE,
                };

                log::info!("parsing metadata…");
                let mut found_parse_err = false;

                let files = raw_meta_files_by_path
                    .into_iter()
                    .filter_map(|(path, file_contents)| {
                        match chumsky::Parser::parse(&File::parser(), &*file_contents).into_result()
                        {
                            Err(errors) => {
                                found_parse_err = true;
                                render_metadata_parse_errors(&path, &file_contents, errors);
                                None
                            }
                            Ok(file) => Some((path, file)),
                        }
                    })
                    .collect::<IndexMap<_, _>>();

                if found_parse_err {
                    log::error!(concat!(
                        "found one or more failures while parsing metadata, ",
                        "see above for more details"
                    ));
                    return ExitCode::FAILURE;
                }

                files
            };

            let mut outcomes_by_test = IndexMap::<TestPath, TestOutcomes>::default();

            log::info!("loading metadata for comparison to reports…");
            for (path, file) in meta_files_by_path {
                let File { tests } = file;
                for (SectionHeader(name), test) in tests {
                    let Test {
                        properties:
                            AnalyzeableProps {
                                is_disabled,
                                expectations,
                            },
                        subtests,
                    } = test;

                    let test_path = TestPath::from_fx_metadata_test(
                        path.strip_prefix(&gecko_checkout).unwrap(),
                        &name,
                    )
                    .unwrap();

                    let freak_out_do_nothing = |what: &dyn Display| {
                        log::error!("hoo boy, not sure what to do yet: {what}")
                    };

                    let TestOutcomes {
                        test_outcomes: recorded_test_outcomes,
                        subtests: recorded_subtests,
                    } = outcomes_by_test
                        .entry(test_path.clone().into_owned())
                        .or_default();

                    let test_path = &test_path;
                    let mut reported_dupe_already = false;

                    let maybe_disabled = if is_disabled {
                        MaybeDisabled::Disabled
                    } else {
                        MaybeDisabled::Enabled(expectations)
                    };
                    if let Some(_old) = recorded_test_outcomes.metadata.replace(maybe_disabled) {
                        freak_out_do_nothing(
                            &lazy_format!(
                                "duplicate entry for {test_path:?}, discarding previous entries with this and further dupes"
                            )
                        );
                        reported_dupe_already = true;
                    }

                    for (SectionHeader(subtest_name), subtest) in subtests {
                        let Subtest {
                            properties:
                                AnalyzeableProps {
                                    is_disabled,
                                    expectations,
                                },
                        } = subtest;
                        let recorded_subtest_outcomes =
                            recorded_subtests.entry(subtest_name.clone()).or_default();
                        let maybe_disabled = if is_disabled {
                            MaybeDisabled::Disabled
                        } else {
                            MaybeDisabled::Enabled(expectations)
                        };
                        if let Some(_old) =
                            recorded_subtest_outcomes.metadata.replace(maybe_disabled)
                        {
                            if !reported_dupe_already {
                                freak_out_do_nothing(&lazy_format!(
                                    "duplicate subtest in {test_path:?} named {subtest_name:?}, discarding previous entries with this and further dupes"
                                ));
                            }
                        }
                    }
                }
            }

            log::info!("gathering reported test outcomes for reconciliation with metadata…");

            let (exec_reports_sender, exec_reports_receiver) = channel();
            exec_report_paths
                .into_par_iter()
                .for_each_with(exec_reports_sender, |sender, path| {
                    let res = fs::File::open(&path)
                        .map(BufReader::new)
                        .map_err(Report::msg)
                        .wrap_err("failed to open file")
                        .and_then(|reader| {
                            serde_json::from_reader::<_, ExecutionReport>(reader)
                                .into_diagnostic()
                                .wrap_err("failed to parse JSON")
                        })
                        .wrap_err_with(|| {
                            format!(
                                "failed to read WPT execution report from {}",
                                path.display()
                            )
                        })
                        .map(|parsed| (path, parsed))
                        .map_err(|e| {
                            log::error!("{e:?}");
                            AlreadyReportedToCommandline
                        });
                    let _ = sender.send(res);
                });

            for res in exec_reports_receiver {
                let (_path, exec_report) = match res {
                    Ok(ok) => ok,
                    Err(AlreadyReportedToCommandline) => return ExitCode::FAILURE,
                };

                let ExecutionReport {
                    run_info:
                        RunInfo {
                            platform,
                            build_profile,
                        },
                    entries,
                } = exec_report;

                for entry in entries {
                    let TestExecutionEntry { test_name, result } = entry;

                    let test_path = TestPath::from_execution_report(&test_name).unwrap();
                    // TODO: ignore things outside of the WPT path(s) we care about
                    let TestOutcomes {
                        test_outcomes: recorded_test_outcomes,
                        subtests: recorded_subtests,
                    } = outcomes_by_test
                        .entry(test_path.clone().into_owned())
                        .or_default();

                    let (reported_outcome, reported_subtests) = match result {
                        TestExecutionResult::Complete {
                            duration: _,
                            outcome,
                            subtests,
                        } => (outcome, subtests),
                        TestExecutionResult::JobMaybeTimedOut { status, subtests } => {
                            if !status.is_empty() {
                                // TODO: probably don't keep this
                                log::warn!(
                                    concat!(
                                        "expected an empty `status` field for {:?}, ",
                                        "but found the {:?} status"
                                    ),
                                    test_path,
                                    status,
                                )
                            }
                            (TestOutcome::Timeout, subtests)
                        }
                    };

                    fn accumulate<Out>(
                        recorded: &mut BTreeMap<Platform, BTreeMap<BuildProfile, Expectation<Out>>>,
                        platform: Platform,
                        build_profile: BuildProfile,
                        reported_outcome: Out,
                    ) where
                        Out: EnumSetType + Hash,
                    {
                        match recorded.entry(platform).or_default().entry(build_profile) {
                            std::collections::btree_map::Entry::Vacant(entry) => {
                                entry.insert(Expectation::permanent(reported_outcome));
                            }
                            std::collections::btree_map::Entry::Occupied(mut entry) => {
                                *entry.get_mut() |= reported_outcome
                            }
                        }
                    }
                    accumulate(
                        &mut recorded_test_outcomes.reported,
                        platform,
                        build_profile,
                        reported_outcome,
                    );

                    for reported_subtest in reported_subtests {
                        let SubtestExecutionResult {
                            subtest_name: reported_subtest_name,
                            outcome: reported_outcome,
                        } = reported_subtest;

                        accumulate(
                            &mut recorded_subtests
                                .entry(reported_subtest_name.clone())
                                .or_default()
                                .reported,
                            platform,
                            build_profile,
                            reported_outcome,
                        );
                    }
                }
            }

            log::info!("metadata and reports gathered, now reconciling outcomes…");

            let mut found_reconciliation_err = false;
            let recombined_tests_iter =
                outcomes_by_test
                    .into_iter()
                    .filter_map(|(test_path, outcomes)| {
                        fn reconcile<Out>(
                            outcomes: OutcomesForComparison<Out>,
                            preset: ReportProcessingPreset,
                        ) -> AnalyzeableProps<Out>
                        where
                            Out: Debug + Default + EnumSetType,
                        {
                            let OutcomesForComparison { metadata, reported } = outcomes;

                            let metadata = metadata
                                .map(|maybe_disabled| {
                                    maybe_disabled.map_enabled(|opt| opt.unwrap_or_default())
                                })
                                .unwrap_or_default();

                            let normalize = NormalizedExpectationPropertyValue::from_fully_expanded;

                            let reconciled_expectations = metadata.map_enabled(|metadata| {
                                let resolve = match preset {
                                    ReportProcessingPreset::ResetAll => return normalize(reported),
                                    ReportProcessingPreset::ResetContradictory => {
                                        |meta: Expectation<_>, rep: Option<Expectation<_>>| {
                                            rep.filter(|rep| !meta.is_superset(rep)).unwrap_or(meta)
                                        }
                                    }
                                    ReportProcessingPreset::Merge => |meta, rep| match rep {
                                        Some(rep) => meta | rep,
                                        None => meta,
                                    },
                                };

                                normalize(
                                    Platform::iter()
                                        .map(|platform| {
                                            let build_profiles = BuildProfile::iter()
                                                .map(|build_profile| {
                                                    (
                                                        build_profile,
                                                        resolve(
                                                            metadata.get(platform, build_profile),
                                                            reported
                                                                .get(&platform)
                                                                .and_then(|rep| {
                                                                    rep.get(&build_profile)
                                                                })
                                                                .copied(),
                                                        ),
                                                    )
                                                })
                                                .collect();
                                            (platform, build_profiles)
                                        })
                                        .collect(),
                                )
                            });

                            match reconciled_expectations {
                                MaybeDisabled::Disabled => AnalyzeableProps {
                                    is_disabled: true,
                                    expectations: Default::default(),
                                },
                                MaybeDisabled::Enabled(expectations) => AnalyzeableProps {
                                    is_disabled: false,
                                    expectations: Some(expectations),
                                },
                            }
                        }

                        let TestOutcomes {
                            test_outcomes,
                            subtests: recorded_subtests,
                        } = outcomes;

                        let properties = reconcile(test_outcomes, preset);

                        let mut subtests = BTreeMap::new();
                        for (subtest_name, subtest) in recorded_subtests {
                            let subtest_name = SectionHeader(subtest_name);
                            if subtests.get(&subtest_name).is_some() {
                                found_reconciliation_err = true;
                                log::error!("internal error: duplicate test path {test_path:?}");
                            }
                            subtests.insert(
                                subtest_name,
                                Subtest {
                                    properties: reconcile(subtest, preset),
                                },
                            );
                        }

                        if subtests.is_empty() && properties == Default::default() {
                            None
                        } else {
                            Some((test_path, (properties, subtests)))
                        }
                    });

            log::info!(
                "outcome reconciliation complete, gathering tests back into new metadata files…"
            );

            let mut files = BTreeMap::<PathBuf, File>::new();
            for (test_path, (properties, subtests)) in recombined_tests_iter {
                let name = test_path.test_name().to_string();
                let path = gecko_checkout.join(test_path.rel_metadata_path_fx().to_string());
                let file = files.entry(path).or_default();
                file.tests.insert(
                    SectionHeader(name),
                    Test {
                        properties,
                        subtests,
                    },
                );
            }

            log::info!("gathering of new metadata files completed, writing to file system…");

            for (path, file) in files {
                log::debug!("writing new metadata to {}", path.display());
                match write_to_file(&path, metadata::format_file(&file)) {
                    Ok(()) => (),
                    Err(AlreadyReportedToCommandline) => {
                        found_reconciliation_err = true;
                    }
                }
            }

            if found_reconciliation_err {
                log::error!(concat!(
                    "one or more errors found while reconciling, ",
                    "exiting with failure; see above for more details"
                ));
                return ExitCode::FAILURE;
            }

            ExitCode::SUCCESS
        }
        Subcommand::Format => {
            let raw_test_files_by_path = match read_metadata() {
                Ok(paths) => paths,
                Err(AlreadyReportedToCommandline) => return ExitCode::FAILURE,
            };
            log::info!("formatting metadata in-place…");
            let mut fmt_err_found = false;
            for (path, file_contents) in raw_test_files_by_path {
                match chumsky::Parser::parse(&File::parser(), &*file_contents).into_result() {
                    Err(errors) => {
                        fmt_err_found = true;
                        render_metadata_parse_errors(&path, &file_contents, errors);
                    }
                    Ok(file) => match write_to_file(&path, metadata::format_file(&file)) {
                        Ok(()) => (),
                        Err(AlreadyReportedToCommandline) => {
                            fmt_err_found = true;
                        }
                    },
                }
            }

            if fmt_err_found {
                log::error!(concat!(
                    "found one or more failures while formatting metadata, ",
                    "see above for more details"
                ));
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Subcommand::Triage => {
            #[derive(Debug)]
            struct TaggedTest {
                #[allow(unused)]
                orig_path: Arc<PathBuf>,
                inner: Test,
            }
            let tests_by_name = {
                let mut found_parse_err = false;
                let raw_test_files_by_path = match read_metadata() {
                    Ok(paths) => paths,
                    Err(AlreadyReportedToCommandline) => return ExitCode::FAILURE,
                };
                let extracted = raw_test_files_by_path
                    .iter()
                    .filter_map(|(path, file_contents)| {
                        match chumsky::Parser::parse(&metadata::File::parser(), file_contents)
                            .into_result()
                        {
                            Ok(File { tests }) => Some(tests.into_iter().map(|(name, inner)| {
                                let SectionHeader(name) = &name;
                                (
                                    SectionHeader(
                                        name.strip_prefix("cts.https.html?q=").unwrap().to_owned(),
                                    ),
                                    TaggedTest {
                                        inner,
                                        orig_path: path.clone(),
                                    },
                                )
                            })),
                            Err(errors) => {
                                found_parse_err = true;
                                render_metadata_parse_errors(path, file_contents, errors);
                                None
                            }
                        }
                    })
                    .flatten()
                    .collect::<BTreeMap<_, _>>();
                if found_parse_err {
                    log::error!(concat!(
                        "found one or more failures while parsing metadata, ",
                        "see above for more details"
                    ));
                    return ExitCode::FAILURE;
                }
                extracted
            };

            log::info!(concat!(
                "finished parsing of interesting properties ",
                "from metadata files, analyzing results…"
            ));

            #[derive(Clone, Debug, Default)]
            struct PerPlatformAnalysis {
                tests_with_runner_errors: BTreeSet<Arc<SectionHeader>>,
                tests_with_disabled_or_skip: BTreeSet<Arc<SectionHeader>>,
                tests_with_crashes: BTreeSet<Arc<SectionHeader>>,
                subtests_with_failures_by_test:
                    BTreeMap<Arc<SectionHeader>, IndexSet<Arc<SectionHeader>>>,
                subtests_with_timeouts_by_test:
                    BTreeMap<Arc<SectionHeader>, IndexSet<Arc<SectionHeader>>>,
            }

            #[derive(Clone, Debug, Default)]
            struct Analysis {
                windows: PerPlatformAnalysis,
                linux: PerPlatformAnalysis,
                mac_os: PerPlatformAnalysis,
            }

            impl Analysis {
                pub fn for_each_platform_mut<F>(&mut self, mut f: F)
                where
                    F: FnMut(&mut PerPlatformAnalysis),
                {
                    let Self {
                        windows,
                        linux,
                        mac_os,
                    } = self;
                    for analysis in [windows, linux, mac_os] {
                        f(analysis)
                    }
                }

                pub fn for_each_platform<F>(&self, mut f: F)
                where
                    F: FnMut(Platform, &PerPlatformAnalysis),
                {
                    let Self {
                        windows,
                        linux,
                        mac_os,
                    } = self;
                    for (platform, analysis) in [
                        (Platform::Windows, windows),
                        (Platform::Linux, linux),
                        (Platform::MacOs, mac_os),
                    ] {
                        f(platform, analysis)
                    }
                }

                pub fn for_platform_mut<F>(&mut self, platform: Platform, mut f: F)
                where
                    F: FnMut(&mut PerPlatformAnalysis),
                {
                    match platform {
                        Platform::Windows => f(&mut self.windows),
                        Platform::Linux => f(&mut self.linux),
                        Platform::MacOs => f(&mut self.mac_os),
                    }
                }
            }

            let mut analysis = Analysis::default();
            for (SectionHeader(test_name), test) in tests_by_name {
                let TaggedTest {
                    orig_path: _,
                    inner: test,
                } = test;

                let Test {
                    properties,
                    subtests,
                } = test;

                let AnalyzeableProps {
                    is_disabled,
                    expectations,
                } = properties;

                let test_name = test_name
                    .strip_prefix("cts.https.html?q=")
                    .map(|n| n.to_owned())
                    .unwrap_or(test_name);
                let test_name = Arc::new(SectionHeader(test_name));

                if is_disabled {
                    analysis.for_each_platform_mut(|analysis| {
                        analysis
                            .tests_with_disabled_or_skip
                            .insert(test_name.clone());
                    })
                }

                fn apply_expectation<Out, F>(expectation: Expectation<Out>, f: F)
                where
                    F: FnMut(Out),
                    Out: EnumSetType,
                {
                    expectation.into_iter().for_each(f)
                }
                if let Some(expectations) = expectations {
                    fn analyze_test_outcome<F>(
                        test_name: &Arc<SectionHeader>,
                        outcome: TestOutcome,
                        mut receiver: F,
                    ) where
                        F: FnMut(&mut dyn FnMut(&mut PerPlatformAnalysis)),
                    {
                        match outcome {
                            TestOutcome::Ok => (),
                            // We skip this because this test _should_ contain subtests with
                            // `TIMEOUT` and `NOTRUN`, so we shouldn't actually miss anything.
                            TestOutcome::Timeout => (),
                            TestOutcome::Crash => receiver(&mut |analysis| {
                                analysis.tests_with_crashes.insert(test_name.clone());
                            }),
                            TestOutcome::Error => receiver(&mut |analysis| {
                                analysis.tests_with_runner_errors.insert(test_name.clone());
                            }),
                            TestOutcome::Skip => receiver(&mut |analysis| {
                                analysis
                                    .tests_with_disabled_or_skip
                                    .insert(test_name.clone());
                            }),
                        }
                    }

                    let apply_to_all_platforms = |analysis: &mut Analysis, expectation| {
                        apply_expectation(expectation, |outcome| {
                            analyze_test_outcome(&test_name, outcome, |f| {
                                analysis.for_each_platform_mut(f)
                            })
                        })
                    };
                    let apply_to_specific_platforms =
                        |analysis: &mut Analysis, platform, expectation| {
                            apply_expectation(expectation, |outcome| {
                                analyze_test_outcome(&test_name, outcome, |f| {
                                    analysis.for_platform_mut(platform, f)
                                })
                            })
                        };

                    match expectations.into_inner() {
                        MaybeCollapsed::Collapsed(exps) => match exps {
                            MaybeCollapsed::Collapsed(exp) => {
                                apply_to_all_platforms(&mut analysis, exp)
                            }
                            MaybeCollapsed::Expanded(by_build_profile) => {
                                for (_build_profile, exp) in by_build_profile {
                                    apply_to_all_platforms(&mut analysis, exp)
                                }
                            }
                        },
                        MaybeCollapsed::Expanded(by_platform) => {
                            for (platform, exp_by_build_profile) in by_platform {
                                // TODO: has a lot in common with above cases. Refactor out?
                                match exp_by_build_profile {
                                    MaybeCollapsed::Collapsed(exp) => {
                                        apply_to_specific_platforms(&mut analysis, platform, exp)
                                    }
                                    MaybeCollapsed::Expanded(by_build_profile) => {
                                        for (_build_profile, exp) in by_build_profile {
                                            apply_to_specific_platforms(
                                                &mut analysis,
                                                platform,
                                                exp,
                                            )
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                for (subtest_name, subtest) in subtests {
                    let subtest_name = Arc::new(subtest_name);

                    let Subtest { properties } = subtest;
                    let AnalyzeableProps {
                        is_disabled,
                        expectations,
                    } = properties;

                    if is_disabled {
                        analysis
                            .windows
                            .tests_with_disabled_or_skip
                            .insert(test_name.clone());
                    }

                    if let Some(expectations) = expectations {
                        fn analyze_subtest_outcome<Fo>(
                            test_name: &Arc<SectionHeader>,
                            subtest_name: &Arc<SectionHeader>,
                            outcome: SubtestOutcome,
                            mut receiver: Fo,
                        ) where
                            Fo: FnMut(&mut dyn FnMut(&mut PerPlatformAnalysis)),
                        {
                            match outcome {
                                SubtestOutcome::Pass => (),
                                SubtestOutcome::Timeout | SubtestOutcome::NotRun => {
                                    receiver(&mut |analysis| {
                                        analysis
                                            .subtests_with_timeouts_by_test
                                            .entry(test_name.clone())
                                            .or_default()
                                            .insert(subtest_name.clone());
                                    })
                                }
                                SubtestOutcome::Crash => receiver(&mut |analysis| {
                                    analysis.tests_with_crashes.insert(test_name.clone());
                                }),
                                SubtestOutcome::Fail => receiver(&mut |analysis| {
                                    analysis
                                        .subtests_with_failures_by_test
                                        .entry(test_name.clone())
                                        .or_default()
                                        .insert(subtest_name.clone());
                                }),
                            }
                        }

                        let apply_to_all_platforms = |analysis: &mut Analysis, expectation| {
                            apply_expectation(expectation, |outcome| {
                                analyze_subtest_outcome(&test_name, &subtest_name, outcome, |f| {
                                    analysis.for_each_platform_mut(f)
                                })
                            })
                        };
                        let apply_to_specific_platforms =
                            |analysis: &mut Analysis, platform, expectation| {
                                apply_expectation(expectation, |outcome| {
                                    analyze_subtest_outcome(
                                        &test_name,
                                        &subtest_name,
                                        outcome,
                                        |f| analysis.for_platform_mut(platform, f),
                                    )
                                })
                            };

                        match expectations.into_inner() {
                            MaybeCollapsed::Collapsed(exps) => match exps {
                                MaybeCollapsed::Collapsed(exp) => {
                                    apply_to_all_platforms(&mut analysis, exp)
                                }
                                MaybeCollapsed::Expanded(by_build_profile) => {
                                    for (_build_profile, exp) in by_build_profile {
                                        apply_to_all_platforms(&mut analysis, exp)
                                    }
                                }
                            },
                            MaybeCollapsed::Expanded(by_platform) => {
                                for (platform, exp_by_build_profile) in by_platform {
                                    // TODO: has a lot in common with above cases. Refactor out?
                                    match exp_by_build_profile {
                                        MaybeCollapsed::Collapsed(exp) => {
                                            apply_to_specific_platforms(
                                                &mut analysis,
                                                platform,
                                                exp,
                                            )
                                        }
                                        MaybeCollapsed::Expanded(by_build_profile) => {
                                            for (_build_profile, exp) in by_build_profile {
                                                apply_to_specific_platforms(
                                                    &mut analysis,
                                                    platform,
                                                    exp,
                                                )
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            log::info!("finished analysis, printing to `stdout`…");
            analysis.for_each_platform(|platform, analysis| {
                let PerPlatformAnalysis {
                    tests_with_runner_errors,
                    tests_with_disabled_or_skip,
                    tests_with_crashes,
                    subtests_with_failures_by_test,
                    subtests_with_timeouts_by_test,
                } = analysis;

                let num_tests_with_runner_errors = tests_with_runner_errors.len();
                let num_tests_with_disabled = tests_with_disabled_or_skip.len();
                let num_tests_with_crashes = tests_with_crashes.len();
                let num_tests_with_failures_somewhere = subtests_with_failures_by_test.len();
                let num_subtests_with_failures_somewhere = subtests_with_failures_by_test
                    .iter()
                    .flat_map(|(_name, subtests)| subtests.iter())
                    .count();
                let num_tests_with_timeouts_somewhere = subtests_with_timeouts_by_test.len();
                let num_subtests_with_timeouts_somewhere = subtests_with_timeouts_by_test
                    .iter()
                    .flat_map(|(_name, subtests)| subtests.iter())
                    .count();
                println!(
                    "\
{platform:?}:
  HIGH PRIORITY:
    {num_tests_with_runner_errors} test with execution reporting `ERROR`
    {num_tests_with_disabled} tests with some portion marked as `disabled`
    {num_tests_with_crashes} tests with some portion expecting `CRASH`
  MEDIUM PRIORITY:
    {num_tests_with_failures_somewhere} tests with some portion `FAIL`ing, {num_subtests_with_failures_somewhere} subtests total
    {num_tests_with_timeouts_somewhere} tests with some portion returning `TIMEOUT`/`NOTRUN`, {num_subtests_with_timeouts_somewhere} subtests total
"
                );
            });
            println!("Full analysis: {analysis:#?}");
            ExitCode::SUCCESS
        }
        Subcommand::ReadTestVariants => {
            let webgpu_cts_test_parent_dir = {
                path!(
                    &gecko_checkout
                        | "testing"
                        | "web-platform"
                        | "mozilla"
                        | "tests"
                        | "webgpu"
                        | "chunked"
                )
            };

            let tests_by_path = match read_gecko_files_at(
                &gecko_checkout,
                &webgpu_cts_test_parent_dir,
                "**/cts.https.html",
            ) {
                Ok(paths) => {
                    let mut found_err = false;
                    let collected = paths
                        .filter_map(|res| {
                            found_err |= res.is_ok();
                            res.ok()
                        })
                        .collect::<IndexMap<_, _>>();
                    if found_err {
                        return ExitCode::FAILURE;
                    }
                    collected
                }
                Err(AlreadyReportedToCommandline) => return ExitCode::FAILURE,
            };

            let meta_variant_re =
                Regex::new(r"^<meta name=variant content='\?q=(?P<variant_path>.*?)'>$").unwrap();
            let meta_variant_re = &meta_variant_re;
            let variants = tests_by_path
                .iter()
                .flat_map(|(test_path, contents)| {
                    contents.lines().filter_map(move |line| {
                        meta_variant_re.captures(line).map(move |captures| {
                            (captures.name("variant_path").unwrap().as_str(), test_path)
                        })
                    })
                })
                .collect::<BTreeMap<_, _>>();
            println!("{variants:#?}");
            ExitCode::SUCCESS
        }
    }
}

/// Returns a "naturally" sorted list of files found by searching for `glob_pattern` in `base`.
/// `gecko_checkout` is stripped as a prefix from the absolute paths recorded into `log` entries
/// emitted by this function.
///
/// # Panics
///
/// This function will panick if `gecko_checkout` cannot be stripped as a prefix of `base`.
fn read_gecko_files_at(
    gecko_checkout: &Path,
    base: &Path,
    glob_pattern: &str,
) -> Result<
    impl Iterator<Item = Result<(PathBuf, String), AlreadyReportedToCommandline>>,
    AlreadyReportedToCommandline,
> {
    log::info!("reading {glob_pattern} files at {}", base.display());
    let mut found_read_err = false;
    let mut paths = Glob::new(glob_pattern)
        .unwrap()
        .walk(base)
        .filter_map(|entry| match entry {
            Ok(entry) => Some(entry.path().to_owned()),
            Err(e) => {
                let path_disp = e
                    .path()
                    .map(|p| format!(" in {}", p.strip_prefix(gecko_checkout).unwrap().display()));
                let path_disp: &dyn Display = match path_disp.as_ref() {
                    Some(disp) => disp,
                    None => &"",
                };
                log::error!(
                    "failed to enumerate {glob_pattern} files{}\n  caused by: {e}",
                    path_disp
                );
                found_read_err = true;
                None
            }
        })
        .collect::<Vec<_>>();

    paths.sort_by(|a, b| natord::compare(a.to_str().unwrap(), b.to_str().unwrap()));
    let paths = paths;

    log::debug!(
        "working with these files: {:#?}",
        paths
            .iter()
            .map(|f| f.strip_prefix(gecko_checkout).unwrap())
            .collect::<std::collections::BTreeSet<_>>()
    );

    if found_read_err {
        return Err(AlreadyReportedToCommandline);
    }

    Ok(paths.into_iter().map(|path| -> Result<_, _> {
        log::debug!("reading from {}…", path.display());
        fs::read_to_string(&path)
            .map_err(|e| {
                log::error!("failed to read {path:?}: {e}");
                AlreadyReportedToCommandline
            })
            .map(|file_contents| (path, file_contents))
    }))
}

/// Search for a `mozilla-central` checkout either via Mercurial or Git, iterating from the CWD to
/// its parent directories.
///
/// This function reports to `log` automatically, so no meaningful [`Err`] value is returned.
fn search_for_moz_central_ckt() -> Result<PathBuf, AlreadyReportedToCommandline> {
    use lets_find_up::{find_up_with, FindUpKind, FindUpOptions};

    let find_up_opts = || FindUpOptions {
        cwd: Path::new("."),
        kind: FindUpKind::Dir,
    };
    let find_up = |repo_tech_name, root_dir_name| {
        log::debug!("searching for {repo_tech_name} checkout of `mozilla-central`…");
        let err = || {
            miette!(
                "failed to find a {} repository ({:?}) in {}",
                repo_tech_name,
                root_dir_name,
                "any of current working directory and its parent directories",
            )
        };
        find_up_with(root_dir_name, find_up_opts())
            .map_err(Report::msg)
            .wrap_err_with(err)
            .and_then(|loc_opt| loc_opt.ok_or_else(err))
            .map(|mut dir| {
                dir.pop();
                dir
            })
    };
    let gecko_source_root =
        find_up("Mercurial", ".hg").or_else(|e| match find_up("Git", ".git") {
            Ok(path) => {
                log::debug!("{e:?}");
                Ok(path)
            }
            Err(e2) => {
                log::warn!("{e:?}");
                log::warn!("{e2:?}");
                log::error!("failed to find a Gecko repository root");
                Err(AlreadyReportedToCommandline)
            }
        })?;

    log::info!(
        "detected Gecko repository root at {}",
        gecko_source_root.display()
    );

    Ok(gecko_source_root)
}

struct AlreadyReportedToCommandline;

fn write_to_file(path: &Path, contents: impl Display) -> Result<(), AlreadyReportedToCommandline> {
    let mut out = match fs::File::create(path)
        .map_err(Report::msg)
        .wrap_err_with(|| format!("error while reading file `{}`", path.display()))
    {
        Ok(f) => BufWriter::new(f),
        Err(e) => {
            log::error!("{e}");
            return Err(AlreadyReportedToCommandline);
        }
    };
    use io::Write;
    match write!(&mut out, "{contents}")
        .map_err(Report::msg)
        .wrap_err_with(|| format!("error while writing to `{}`", path.display()))
    {
        Ok(()) => (),
        Err(e) => {
            log::error!("{e}");
            return Err(AlreadyReportedToCommandline);
        }
    }

    Ok(())
}
