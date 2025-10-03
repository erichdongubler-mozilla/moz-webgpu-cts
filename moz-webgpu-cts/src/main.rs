mod file_spec;
mod process_reports;
mod report;
mod wpt;

use self::{
    file_spec::FileSpec,
    wpt::{
        metadata::{
            self,
            properties::{ExpandedPropertyValue, Expected},
            File, ImplementationStatus, Platform, Subtest, SubtestOutcome, Test, TestOutcome,
            TestProps,
        },
        path::TestEntryPath,
    },
};

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Debug, Display, Formatter},
    fs,
    io::{self, BufWriter},
    iter,
    ops::Add,
    path::{Path, PathBuf},
    process::ExitCode,
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
};

use crate::wpt::path::Browser;
use camino::Utf8PathBuf;
use clap::{Parser, ValueEnum};
use enumset::{EnumSet, EnumSetType};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use joinery::JoinableIterator;
use lazy_format::{lazy_format, make_lazy_format};
use miette::{miette, Diagnostic, IntoDiagnostic, NamedSource, Report, SourceSpan, WrapErr};
use path_dsl::path;
use process_reports::{
    should_update_expected::{self, ShouldUpdateExpected},
    ProcessReportsArgs,
};
use wax::{walk::Entry as _, Glob};
use whippit::{
    metadata::SectionHeader,
    reexport::chumsky::{self, prelude::Rich},
};

/// An extremely fast (but opinionated) tool for working with WPT metadata while developing an
/// implementation of WebGPU in a web browser.
#[derive(Debug, Parser)]
#[command(about, version)]
struct Cli {
    #[clap(long, alias = "gecko-checkout")]
    checkout: Option<PathBuf>,
    #[clap(value_enum, long, default_value = "firefox")]
    browser: Browser,
    #[clap(subcommand)]
    subcommand: Subcommand,
}

#[derive(Debug, Parser)]
enum Subcommand {
    /// Migrate old test structure in metadata to that found in `wptreport.json` reports.
    ///
    /// When a new version of CTS is run by your implementation of WebGPU, new execution reports
    /// may differ from old ones in various ways:
    ///
    /// 1. Test may have been added, deleted, or moved.
    ///
    ///    It requires human judgment to determine what additions and deletions are actually
    ///    movements of the same test coverage.
    /// 2. Tests' actual outcomes on your implementation may change, since implementations of
    ///    existing tests may have changed.
    ///
    /// This command implements (only) the changes from (1) in your metadata by using the reports
    /// you provide to:
    ///
    /// 1. Remove _all_ metadata from test paths that are currently in metadata, but not observedn
    ///    execution reports.
    /// 2. Add empty metadata entries for test paths that are observed in execution reports, but
    ///    absent from current metadata.
    ///
    /// The diff produced by the above changes makes it easier to determine what tests may have
    /// moved, and, by extension, whether you should attempt to migrate metadata for subsequent
    /// test runs.
    Migrate {
        #[clap(flatten)]
        exec_report_spec: ExecReportSpec,
    },
    /// Adjust expected test outcomes in metadata, optionally using `wptreport.json` reports from
    /// CI runs covering your browser's implementation of WebGPU.
    ///
    /// As your browser's behavior changes, one generally expects CTS test outcomes to change. When
    /// you are testing your own changes in CI, you can use this subcommand to update expected
    /// outcomes automatically with the following steps:
    ///
    /// 1. Run `moz-webgpu-cts update-expected --preset=new-build …` against the first complete set
    ///    of reports you gather from CI with your new browser build. This will adjust for new
    ///    permanent outcomes, and may capture some (but not all) intermittent outcomes.
    ///
    /// 2. There may still exist intermittent issues that you do not discover in CI run(s) from the
    ///    previous step. As you discover them in further CI runs on the same build of Firefox,
    ///    adjust expected outcomes to match by running `moz-webgpu-cts update-expected
    ///    --preset=same-build …` against the runs' new reports. Repeat as necessary.
    ///
    /// With both steps, you may delete the local copies of these reports after being processed
    /// with `update-expected`. You should not need to re-process them unless you have made an
    /// error in following these steps.
    #[clap(alias = "process-reports")]
    UpdateExpected {
        #[clap(flatten)]
        exec_report_spec: ExecReportSpec,
        /// The heuristic for resolving differences between current metadata and processed reports.
        #[clap(value_enum, long, default_value_t = UpdateExpectedPreset::ResetContradictory)]
        preset: UpdateExpectedPreset,
        /// `implementation-status`es that changes should be applied to.
        #[clap(value_enum, long, default_value = "backlog")]
        implementation_status: Vec<ImplementationStatus>,
        /// What do when only `SKIP` outcomes are found for tests and subtests.
        #[clap(value_enum, long, default_value_t = OnSkipOnly::Reconcile)]
        on_skip_only: OnSkipOnly,
    },
    /// Parse test metadata, apply automated fixups, and re-emit it in normalized form.
    #[clap(name = "fixup", alias = "fmt")]
    Fixup,
    Triage {
        #[clap(value_enum, long, default_value_t = Default::default())]
        on_zero_item: OnZeroItem,
    },
    /// Set `implementation-status` properties to `backlog` or `implementing` based on a
    /// given `criteria`, allowing changes only in the permitted `direction`.
    UpdateBacklog {
        /// Direction(s) permitted for `implementation-status` changes.
        direction: UpdateBacklogDirection,
        /// The criteria to use to determine whether something is ready to come out of `backlog`.
        #[clap(subcommand)]
        criteria: UpdateBacklogCriteria,
    },
    /// Dump all metadata as JSON. Do so at your own risk; no guarantees are made about the
    /// schema of this JSON, for now.
    DumpJson,
}

#[derive(Clone, Debug, clap::Args)]
struct ExecReportSpec {
    /// Direct paths to report files to be processed.
    report_paths: Vec<PathBuf>,
    /// Cross-platform [`wax` globs] to enumerate report files to be processed.
    ///
    /// N.B. for Windows users: backslashes are used strictly for escaped characters, and
    /// forward slashes (`/`) are the only valid path separator for these globs.
    ///
    /// [`wax` globs]: https://github.com/olson-sean-k/wax/blob/master/README.md#patterns
    #[clap(long = "glob", value_name = "REPORT_GLOB")]
    report_globs: Vec<String>,
}

impl ExecReportSpec {
    fn paths(self) -> Result<Vec<PathBuf>, AlreadyReportedToCommandline> {
        let Self {
            report_paths,
            report_globs,
        } = self;

        FileSpec {
            paths: report_paths,
            globs: report_globs,
        }
        .into_paths("WPT report(s)")
    }
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum UpdateExpectedPreset {
    /// alias: `new-fx`, `new-build`
    #[value(alias("new-fx"))]
    #[value(alias("new-build"))]
    ResetContradictory,
    /// alias: `same-fx`, `same-build`
    #[value(alias("same-fx"))]
    #[value(alias("same-build"))]
    Merge,
    ResetAll,
}

impl From<UpdateExpectedPreset> for process_reports::ReportProcessingPreset {
    fn from(value: UpdateExpectedPreset) -> Self {
        match value {
            UpdateExpectedPreset::ResetContradictory => Self::ResetContradictoryOutcomes,
            UpdateExpectedPreset::Merge => Self::MergeOutcomes,
            UpdateExpectedPreset::ResetAll => Self::ResetAllOutcomes,
        }
    }
}

/// See [`Subcommand::UpdateExpected::on_skip_only`].
#[derive(Clone, Copy, Debug, ValueEnum)]
pub(crate) enum OnSkipOnly {
    /// Use reconcilation from the provided `--preset` with `SKIP` outcomes.
    Reconcile,
    /// Do not change metadata.
    Ignore,
}

impl From<OnSkipOnly> for process_reports::OnSkipOnly {
    fn from(value: OnSkipOnly) -> Self {
        match value {
            OnSkipOnly::Ignore => Self::Ignore,
            OnSkipOnly::Reconcile => Self::Reconcile,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, ValueEnum)]
enum OnZeroItem {
    Show,
    #[default]
    Hide,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum UpdateBacklogDirection {
    /// Allows promotions from `backlog` to `implementing`.
    Promote,
    Demote,
    Sync,
}

impl UpdateBacklogDirection {
    pub fn can_promote(self) -> bool {
        match self {
            Self::Promote | Self::Sync => true,
            Self::Demote => false,
        }
    }

    pub fn can_demote(self) -> bool {
        match self {
            Self::Demote | Self::Sync => true,
            Self::Promote => false,
        }
    }
}

#[derive(Clone, Copy, Debug, Parser)]
enum UpdateBacklogCriteria {
    /// Determine status based on `PASS` outcomes on all platforms from `backlog`.
    PermaPassing {
        #[clap(long)]
        only_across_all_platforms: bool,
    },
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
        browser,
        checkout,
        subcommand,
    } = cli;

    let checkout = match checkout.map(Ok).unwrap_or_else(search_for_repo_root) {
        Ok(ckt_path) => ckt_path,
        Err(AlreadyReportedToCommandline) => return ExitCode::FAILURE,
    };

    match subcommand {
        Subcommand::Migrate { exec_report_spec } => match process_reports(
            browser,
            &checkout,
            exec_report_spec,
            process_reports::ReportProcessingPreset::MigrateTestStructure,
            &mut should_update_expected::NeverUpdateExpected,
            OnSkipOnly::Ignore.into(),
        ) {
            Ok(()) => ExitCode::SUCCESS,
            Err(AlreadyReportedToCommandline) => ExitCode::FAILURE,
        },
        Subcommand::UpdateExpected {
            exec_report_spec,
            preset,
            implementation_status,
            on_skip_only,
        } => {
            assert!(
                !implementation_status.is_empty(),
                concat!(
                    "internal error: should not be possible ",
                    "to have an empty `--implementation-status` option from CLI"
                )
            );
            let allowed_implementation_statuses = EnumSet::from_iter(implementation_status);
            match process_reports(
                browser,
                &checkout,
                exec_report_spec,
                preset.into(),
                &mut should_update_expected::ImplementationStatusFilter {
                    allowed: allowed_implementation_statuses,
                },
                on_skip_only.into(),
            ) {
                Ok(()) => ExitCode::SUCCESS,
                Err(AlreadyReportedToCommandline) => ExitCode::FAILURE,
            }
        }
        Subcommand::Fixup => {
            log::info!("fixing up metadata in-place…");
            let err_found = read_and_parse_all_metadata(browser, &checkout)
                .map(|res| {
                    res.and_then(|(path, mut file)| {
                        for test in file.tests.values_mut() {
                            for subtest in &mut test.subtests.values_mut() {
                                if let Some(expected) = subtest.properties.expected.as_mut() {
                                    for (_, expected) in expected.iter_mut() {
                                        taint_subtest_timeouts_by_suspicion(expected);
                                    }
                                }
                            }
                        }

                        write_to_file(&path, metadata::format_file(&file))
                    })
                })
                .fold(false, |err_found, res| match res {
                    Ok(()) => err_found,
                    Err(AlreadyReportedToCommandline) => true,
                });
            if err_found {
                log::error!(concat!(
                    "found one or more failures while fixing up metadata, ",
                    "see above for more details"
                ));
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Subcommand::Triage { on_zero_item } => {
            #[derive(Debug)]
            struct TaggedTest {
                #[expect(unused)]
                orig_path: Arc<PathBuf>,
                inner: Test,
            }
            let mut err_found = false;
            let tests_by_name = read_and_parse_all_metadata(browser, &checkout)
                .map_ok(
                    |(
                        path,
                        metadata::File {
                            properties: _,
                            tests,
                        },
                    )| {
                        tests.into_iter().map({
                            let checkout = &checkout;
                            move |(name, inner)| {
                                let SectionHeader(name) = &name;
                                let test_entry_path = TestEntryPath::from_metadata_test(
                                    browser,
                                    path.strip_prefix(checkout).unwrap(),
                                    name,
                                )
                                .unwrap();
                                let url_path = test_entry_path.runner_url_path().to_string();
                                (
                                    url_path,
                                    TaggedTest {
                                        inner,
                                        orig_path: path.clone(),
                                    },
                                )
                            }
                        })
                    },
                )
                .flatten_ok()
                .filter_map(|res| match res {
                    Ok(ok) => Some(ok),
                    Err(AlreadyReportedToCommandline) => {
                        err_found = true;
                        None
                    }
                })
                .collect::<BTreeMap<_, _>>();
            if err_found {
                return ExitCode::FAILURE;
            }

            log::info!(concat!(
                "finished parsing of interesting properties ",
                "from metadata files, analyzing results…"
            ));

            #[derive(Clone, Default)]
            struct PermaAndIntermittent<T> {
                perma: T,
                intermittent: T,
            }

            impl<T> Debug for PermaAndIntermittent<T>
            where
                T: Debug,
            {
                fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                    let Self {
                        perma,
                        intermittent,
                    } = self;
                    f.debug_struct("") // the name is distracting, blank it out plz
                        .field("perma", perma)
                        .field("intermittent", intermittent)
                        .finish()
                }
            }

            impl<T: Add<Output = T>> Add for PermaAndIntermittent<T> {
                type Output = PermaAndIntermittent<T>;

                fn add(self, rhs: Self) -> Self::Output {
                    Self {
                        perma: self.perma + rhs.perma,
                        intermittent: self.intermittent + rhs.intermittent,
                    }
                }
            }

            impl<T> PermaAndIntermittent<T> {
                pub fn as_ref(&self) -> PermaAndIntermittent<&T> {
                    let Self {
                        perma,
                        intermittent,
                    } = self;
                    PermaAndIntermittent {
                        perma,
                        intermittent,
                    }
                }

                pub fn map<U>(self, f: impl Fn(T) -> U) -> PermaAndIntermittent<U> {
                    let Self {
                        perma,
                        intermittent,
                    } = self;
                    PermaAndIntermittent {
                        perma: f(perma),
                        intermittent: f(intermittent),
                    }
                }
            }

            type TestSet = PermaAndIntermittent<BTreeSet<Arc<String>>>;
            type SubtestByTestSet =
                PermaAndIntermittent<BTreeMap<Arc<String>, IndexSet<Arc<String>>>>;

            #[derive(Clone, Debug, Default)]
            struct PerPlatformAnalysis {
                tests_with_runner_errors: TestSet,
                tests_with_disabled_or_skip: TestSet,
                tests_with_crashes: TestSet,
                tests_with_failures: TestSet,
                subtests_with_failures_by_test: SubtestByTestSet,
                subtests_with_timeouts_by_test: SubtestByTestSet,
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

            let test_count = u64::try_from(tests_by_name.len()).unwrap();
            let subtest_count = tests_by_name
                .values()
                .map(|test| u64::try_from(test.inner.subtests.len()).unwrap())
                .sum::<u64>();
            let mut analysis = Analysis::default();

            for (test_name, test) in tests_by_name {
                let TaggedTest {
                    orig_path: _,
                    inner:
                        Test {
                            properties,
                            subtests,
                        },
                } = test;

                let TestProps {
                    disabled,
                    expected,
                    implementation_status: _,
                    tags: _,
                } = properties;

                let test_name = Arc::new(test_name);

                if disabled.is_some_and(|d| d.iter().any(|(_, val)| val.is_disabled())) {
                    analysis.for_each_platform_mut(|analysis| {
                        analysis
                            .tests_with_disabled_or_skip
                            .perma
                            .insert(test_name.clone());
                    })
                }

                fn insert_in_test_set<Out>(
                    poi: &mut TestSet,
                    test_name: &Arc<String>,
                    expected: Expected<Out>,
                    outcome: Out,
                ) where
                    Out: Debug + Default + EnumSetType,
                {
                    if expected.is_superset(&Expected::permanent(outcome)) {
                        if expected.is_permanent() {
                            &mut poi.perma
                        } else {
                            &mut poi.intermittent
                        }
                        .insert(test_name.clone());
                    }
                }

                fn insert_in_subtest_by_test_set<Out>(
                    poi: &mut SubtestByTestSet,
                    test_name: &Arc<String>,
                    subtest_name: &Arc<String>,
                    expected: Expected<Out>,
                    outcome: Out,
                ) where
                    Out: Debug + Default + EnumSetType,
                {
                    if expected.is_superset(&Expected::permanent(outcome)) {
                        if expected.is_permanent() {
                            &mut poi.perma
                        } else {
                            &mut poi.intermittent
                        }
                        .entry(test_name.clone())
                        .or_default()
                        .insert(subtest_name.clone());
                    }
                }

                if let Some(expected) = expected {
                    fn analyze_test_outcome<F>(
                        test_name: &Arc<String>,
                        expected: Expected<TestOutcome>,
                        mut receiver: F,
                    ) where
                        F: FnMut(&mut dyn FnMut(&mut PerPlatformAnalysis)),
                    {
                        for outcome in expected.iter() {
                            match outcome {
                                TestOutcome::Ok => (),
                                // We skip this because this test _should_ contain subtests with
                                // `TIMEOUT` and `NOTRUN`, so we shouldn't actually miss anything.
                                TestOutcome::Timeout => (),
                                TestOutcome::Crash => receiver(&mut |analysis| {
                                    insert_in_test_set(
                                        &mut analysis.tests_with_crashes,
                                        test_name,
                                        expected,
                                        outcome,
                                    )
                                }),
                                TestOutcome::Error => receiver(&mut |analysis| {
                                    insert_in_test_set(
                                        &mut analysis.tests_with_runner_errors,
                                        test_name,
                                        expected,
                                        outcome,
                                    )
                                }),
                                TestOutcome::Skip => receiver(&mut |analysis| {
                                    insert_in_test_set(
                                        &mut analysis.tests_with_disabled_or_skip,
                                        test_name,
                                        expected,
                                        outcome,
                                    )
                                }),
                                TestOutcome::Pass => (),
                                TestOutcome::Fail => receiver(&mut |analysis| {
                                    insert_in_test_set(
                                        &mut analysis.tests_with_failures,
                                        test_name,
                                        expected,
                                        outcome,
                                    )
                                }),
                            }
                        }
                    }

                    let apply_to_specific_platforms =
                        |analysis: &mut Analysis, platform, expected| {
                            analyze_test_outcome(&test_name, expected, |f| {
                                analysis.for_platform_mut(platform, f)
                            })
                        };

                    for ((platform, _build_profile), expected) in expected.into_iter() {
                        apply_to_specific_platforms(&mut analysis, platform, expected)
                    }
                }

                for (subtest_name, subtest) in subtests {
                    let SectionHeader(subtest_name) = subtest_name;
                    let subtest_name = Arc::new(subtest_name);

                    let Subtest { properties } = subtest;
                    let TestProps {
                        disabled,
                        expected,
                        implementation_status: _,
                        tags: _,
                    } = properties;

                    if disabled.is_some_and(|d| d.iter().any(|(_, val)| val.is_disabled())) {
                        analysis
                            .windows
                            .tests_with_disabled_or_skip
                            .perma
                            .insert(test_name.clone());
                    }

                    if let Some(expected) = expected {
                        fn analyze_subtest_outcome<Fo>(
                            test_name: &Arc<String>,
                            subtest_name: &Arc<String>,
                            expected: Expected<SubtestOutcome>,
                            mut receiver: Fo,
                        ) where
                            Fo: FnMut(&mut dyn FnMut(&mut PerPlatformAnalysis)),
                        {
                            for outcome in expected.iter() {
                                match outcome {
                                    SubtestOutcome::Pass => (),
                                    SubtestOutcome::Timeout | SubtestOutcome::NotRun => {
                                        receiver(&mut |analysis| {
                                            insert_in_subtest_by_test_set(
                                                &mut analysis.subtests_with_timeouts_by_test,
                                                test_name,
                                                subtest_name,
                                                expected,
                                                outcome,
                                            )
                                        })
                                    }
                                    SubtestOutcome::Fail => receiver(&mut |analysis| {
                                        insert_in_subtest_by_test_set(
                                            &mut analysis.subtests_with_failures_by_test,
                                            test_name,
                                            subtest_name,
                                            expected,
                                            outcome,
                                        )
                                    }),
                                }
                            }
                        }

                        let apply_to_specific_platforms =
                            |analysis: &mut Analysis, platform, expected| {
                                analyze_subtest_outcome(&test_name, &subtest_name, expected, |f| {
                                    analysis.for_platform_mut(platform, f)
                                })
                            };

                        for ((platform, _build_profile), expected) in expected.into_iter() {
                            apply_to_specific_platforms(&mut analysis, platform, expected)
                        }
                    }
                }
            }
            log::info!("finished analysis, printing to `stdout`…");
            println!("Total: {test_count} test(s), {subtest_count} subtest(s)");
            analysis.for_each_platform(|platform, analysis| {
                let show_zero_count_item = match on_zero_item {
                    OnZeroItem::Show => true,
                    OnZeroItem::Hide => false,
                };
                let PerPlatformAnalysis {
                    tests_with_runner_errors,
                    tests_with_disabled_or_skip,
                    tests_with_crashes,
                    tests_with_failures,
                    subtests_with_failures_by_test,
                    subtests_with_timeouts_by_test,
                } = analysis;

                let PermaAndIntermittent {
                    perma: num_tests_with_perma_runner_errors,
                    intermittent: num_tests_with_intermittent_runner_errors,
                } = tests_with_runner_errors.as_ref().map(|tests| tests.len());

                let tests_with_perma_runner_errors = (show_zero_count_item
                    || num_tests_with_perma_runner_errors > 0)
                    .then_some(lazy_format!(
                        "{} test(s) with execution reporting permanent `ERROR`",
                        num_tests_with_perma_runner_errors,
                    ));

                let tests_with_intermittent_runner_errors = (show_zero_count_item
                    || num_tests_with_intermittent_runner_errors > 0)
                    .then_some(lazy_format!(
                        "{} test(s) with execution reporting intermittent `ERROR`",
                        num_tests_with_intermittent_runner_errors
                    ));

                let PermaAndIntermittent {
                    perma: num_tests_with_disabled,
                    intermittent: num_tests_with_intermittent_disabled,
                } = tests_with_disabled_or_skip
                    .as_ref()
                    .map(|tests| tests.len());
                let tests_with_disabled = (show_zero_count_item || num_tests_with_disabled > 0)
                    .then_some(lazy_format!(
                        "{num_tests_with_disabled} test(s) with some portion marked as `disabled`"
                    ));
                if num_tests_with_intermittent_disabled > 0 {
                    log::warn!(
                        concat!(
                            "found {} intermittent `SKIP` outcomes, which we don't understand ",
                            "yet; figure it out! The tests: {:#?}"
                        ),
                        num_tests_with_intermittent_disabled,
                        tests_with_disabled_or_skip,
                    )
                }

                let PermaAndIntermittent {
                    perma: num_tests_with_perma_crashes,
                    intermittent: num_tests_with_intermittent_crashes,
                } = tests_with_crashes.as_ref().map(|tests| tests.len());
                let tests_with_perma_crashes = (show_zero_count_item
                    || num_tests_with_perma_crashes > 0)
                    .then_some(lazy_format!(
                        "{} test(s) with some portion expecting permanent `CRASH`",
                        num_tests_with_perma_crashes
                    ));
                let tests_with_intermittent_crashes = (show_zero_count_item
                    || num_tests_with_intermittent_crashes > 0)
                    .then_some(lazy_format!(
                        "{} tests(s) with some portion expecting intermittent `CRASH`",
                        num_tests_with_intermittent_crashes
                    ));

                let PermaAndIntermittent {
                    perma: num_tests_with_perma_failures_somewhere,
                    intermittent: num_tests_with_intermittent_failures_somewhere,
                } = subtests_with_failures_by_test
                    .as_ref()
                    .map(|tests| tests.len())
                    + tests_with_failures.as_ref().map(|tests| tests.len());
                let PermaAndIntermittent {
                    perma: num_subtests_with_perma_failures_somewhere,
                    intermittent: num_subtests_with_intermittent_failures_somewhere,
                } = subtests_with_failures_by_test.as_ref().map(|tests| {
                    tests
                        .iter()
                        .flat_map(|(_name, subtests)| subtests.iter())
                        .count()
                });
                let tests_with_perma_failures = (show_zero_count_item
                    || num_tests_with_perma_failures_somewhere > 0
                    || num_subtests_with_perma_failures_somewhere > 0)
                    .then_some(lazy_format!(
                        "{} test(s) with some portion perma-`FAIL`ing, {} subtests total",
                        num_tests_with_perma_failures_somewhere,
                        num_subtests_with_perma_failures_somewhere,
                    ));
                let tests_with_intermittent_failures = (show_zero_count_item
                    || num_tests_with_intermittent_failures_somewhere > 0
                    || num_subtests_with_intermittent_failures_somewhere > 0)
                    .then_some(make_lazy_format!(|f| {
                        write!(
                            f,
                            concat!(
                                "{} test(s) with some portion intermittently `FAIL`ing, ",
                                "{} subtests total"
                            ),
                            num_tests_with_intermittent_failures_somewhere,
                            num_subtests_with_intermittent_failures_somewhere
                        )
                    }));

                let PermaAndIntermittent {
                    perma: num_tests_with_perma_timeouts_somewhere,
                    intermittent: num_tests_with_intermittent_timeouts_somewhere,
                } = subtests_with_timeouts_by_test
                    .as_ref()
                    .map(|tests| tests.len());
                let PermaAndIntermittent {
                    perma: num_subtests_with_perma_timeouts_somewhere,
                    intermittent: num_subtests_with_intermittent_timeouts_somewhere,
                } = subtests_with_timeouts_by_test.as_ref().map(|tests| {
                    tests
                        .iter()
                        .flat_map(|(_name, subtests)| subtests.iter())
                        .count()
                });
                let tests_with_perma_timeouts_somewhere = (show_zero_count_item
                    || num_tests_with_perma_timeouts_somewhere > 0)
                    .then_some(make_lazy_format!(|f| {
                        write!(
                            f,
                            concat!(
                                "{} test(s) with some portion returning permanent ",
                                "`TIMEOUT`/`NOTRUN`, {} subtests total"
                            ),
                            num_tests_with_perma_timeouts_somewhere,
                            num_subtests_with_perma_timeouts_somewhere
                        )
                    }));
                let tests_with_intermittent_timeouts_somewhere = (show_zero_count_item
                    || num_tests_with_intermittent_timeouts_somewhere > 0)
                    .then_some(make_lazy_format!(|f| {
                        write!(
                            f,
                            concat!(
                                "{} test(s) with some portion intermittently returning ",
                                "`TIMEOUT`/`NOTRUN`, {} subtest(s) total",
                            ),
                            num_tests_with_intermittent_timeouts_somewhere,
                            num_subtests_with_intermittent_timeouts_somewhere
                        )
                    }));

                fn priority_section<'a, const SIZE: usize>(
                    name: &'static str,
                    items: [Option<&'a dyn Display>; SIZE],
                ) -> Option<Box<dyn Display + 'a>> {
                    items.iter().any(Option::is_some).then(move || {
                        Box::new(make_lazy_format!(|f| {
                            let items = items
                                .iter()
                                .filter_map(|opt| *opt)
                                .map(|item| lazy_format!("\n    {item}"))
                                .join_with("");
                            write!(f, "\n  {name} PRIORITY:{items}")
                        })) as Box<dyn Display>
                    })
                }
                fn item<T>(item: Option<&T>) -> Option<&dyn Display>
                where
                    T: Display,
                {
                    item.map(|disp| disp as &dyn Display)
                }
                let sections = [
                    priority_section(
                        "HIGH",
                        [
                            item(tests_with_perma_runner_errors.as_ref()),
                            item(tests_with_disabled.as_ref()),
                            item(tests_with_perma_crashes.as_ref()),
                        ],
                    ),
                    priority_section(
                        "MEDIUM",
                        [
                            item(tests_with_perma_failures.as_ref()),
                            item(tests_with_perma_timeouts_somewhere.as_ref()),
                            item(tests_with_intermittent_crashes.as_ref()),
                            item(tests_with_intermittent_runner_errors.as_ref()),
                        ],
                    ),
                    priority_section(
                        "LOW",
                        [
                            item(tests_with_intermittent_timeouts_somewhere.as_ref()),
                            item(tests_with_intermittent_failures.as_ref()),
                        ],
                    ),
                ];
                let sections = sections.iter().filter_map(Option::as_ref).join_with("");
                println!("{platform:?}:{sections}")
            });
            println!("Full analysis: {analysis:#?}");
            ExitCode::SUCCESS
        }
        Subcommand::UpdateBacklog {
            direction,
            criteria,
        } => {
            let mut files = {
                let mut found_parse_err = false;
                let extracted = read_and_parse_all_metadata(browser, &checkout)
                    .filter_map(|res| match res {
                        Ok(ok) => Some(ok),
                        Err(AlreadyReportedToCommandline) => {
                            found_parse_err = true;
                            None
                        }
                    })
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

            #[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
            enum Case {
                #[default]
                PermaPass,
                Other,
            }
            let mut found_write_err = false;
            for (file_path, file) in files.iter_mut() {
                let File {
                    properties: _,
                    tests,
                } = file;

                for test in tests.values_mut() {
                    let Test {
                        properties,
                        subtests,
                    } = test;
                    let mut aggregated_case_for_test =
                        properties.expected.unwrap_or_default().map(|expected| {
                            match expected.as_permanent() {
                                Some(TestOutcome::Ok | TestOutcome::Pass) => Case::PermaPass,
                                _ => Case::Other,
                            }
                        });
                    let mut cases_by_subtest = BTreeMap::new();
                    if !subtests.is_empty() {
                        cases_by_subtest = subtests
                            .iter()
                            .map(|subtest| {
                                let (name, Subtest { properties }) = subtest;
                                let case =
                                    properties.expected.unwrap_or_default().map(|expected| {
                                        match expected.as_permanent() {
                                            Some(SubtestOutcome::Pass) => Case::PermaPass,
                                            _ => Case::Other,
                                        }
                                    });
                                (name, case)
                            })
                            .collect::<BTreeMap<_, _>>();
                        aggregated_case_for_test =
                            ExpandedPropertyValue::from_query(|platform, build_profile| {
                                let consistent_expected = cases_by_subtest
                                    .values()
                                    .map(|case| case[(platform, build_profile)])
                                    .chain(iter::once(
                                        aggregated_case_for_test[(platform, build_profile)],
                                    ))
                                    .all_equal_value()
                                    .ok();
                                consistent_expected.unwrap_or(Case::Other)
                            });
                    }
                    // TODO: Just compare this multiple times (here _and_ above), and compare
                    // subtests afterwards.
                    let value_across_all_platforms = || {
                        aggregated_case_for_test
                            .into_iter()
                            .map(|(_, case)| case)
                            .all_equal_value()
                    };
                    fn apply_criteria(
                        direction: UpdateBacklogDirection,
                        case: Case,
                    ) -> Option<ImplementationStatus> {
                        match case {
                            Case::PermaPass if direction.can_promote() => {
                                Some(ImplementationStatus::Implementing)
                            }
                            Case::Other if direction.can_demote() => {
                                Some(ImplementationStatus::Backlog)
                            }
                            _ => None,
                        }
                    }
                    match criteria {
                        UpdateBacklogCriteria::PermaPassing {
                            only_across_all_platforms,
                        } => {
                            if only_across_all_platforms {
                                properties.implementation_status = apply_criteria(
                                    direction,
                                    value_across_all_platforms().unwrap_or(Case::Other),
                                )
                                .map(ExpandedPropertyValue::unconditional)
                                .or(properties.implementation_status);
                            } else {
                                let old_impl_status =
                                    properties.implementation_status.unwrap_or_default();
                                properties.implementation_status.replace(
                                    aggregated_case_for_test.map_with(|key, case| {
                                        apply_criteria(direction, case)
                                            .unwrap_or_else(|| old_impl_status[key])
                                    }),
                                );
                            }
                        }
                    }
                }
                match write_to_file(file_path, metadata::format_file(file)) {
                    Ok(()) => (),
                    Err(AlreadyReportedToCommandline) => found_write_err = true,
                }
            }
            if found_write_err {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Subcommand::DumpJson => {
            let mut read_err_found = false;
            let metadata = read_and_parse_all_metadata(browser, &checkout)
                .map_ok(|(path, file)| {
                    let path = match Utf8PathBuf::from_path_buf(Arc::try_unwrap(path).unwrap()) {
                        Ok(path) => path,
                        Err(path) => panic!("ofrick, this ain't a UTF-8 path: {}", path.display()),
                    };
                    (path, file)
                })
                .filter_map(|res| match res {
                    Ok(ok) => Some(ok),
                    Err(AlreadyReportedToCommandline) => {
                        read_err_found = true;
                        None
                    }
                })
                .collect::<BTreeMap<_, _>>();
            if read_err_found {
                log::error!(concat!(
                    "found one or more failures while reading metadata, ",
                    "see above for more details"
                ));
                return ExitCode::FAILURE;
            }

            log::debug!("dumping all metadata to JSON…");
            match serde_json::to_writer(io::stdout().lock(), &metadata)
                .into_diagnostic()
                .wrap_err("error while writing JSON to `stdout`")
            {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => {
                    log::error!("{e:?}");
                    ExitCode::FAILURE
                }
            }
        }
    }
}

fn read_and_parse_all_metadata(
    browser: Browser,
    checkout: &Path,
) -> impl Iterator<Item = Result<(Arc<PathBuf>, metadata::File), AlreadyReportedToCommandline>> {
    let webgpu_cts_meta_parent_dir = match browser {
        Browser::Firefox => {
            path!(&checkout | "testing" | "web-platform" | "mozilla" | "meta" | "webgpu")
        }
        Browser::Servo => path!(&checkout | "tests" | "wpt" | "webgpu" | "meta" | "webgpu"),
    };

    let raw_metadata_files = read_files_at(checkout, &webgpu_cts_meta_parent_dir, "**/*.ini");

    let mut started_parsing = false;
    raw_metadata_files.filter_map(move |res| {
        res.and_then(|(path, file_contents)| {
            if path.ends_with("__dir__.ini") {
                return Ok(None);
            }

            let path = Arc::new(path);
            let file_contents = Arc::new(file_contents);

            if !started_parsing {
                log::debug!("parsing metadata…");
                started_parsing = true;
            }

            log::debug!("parsing metadata at {}", path.display());
            let res = match chumsky::Parser::parse(&metadata::File::parser(), &*file_contents)
                .into_result()
            {
                Err(errors) => {
                    render_metadata_parse_errors(&path, &file_contents, errors);
                    Err(AlreadyReportedToCommandline)
                }
                Ok(file) => Ok((path, file)),
            };
            Some(res).transpose()
        })
        .transpose()
    })
}

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
        source_code: NamedSource<Arc<String>>,
        inner: Rich<'static, char>,
    }
    let source_code = file_contents.clone();
    for error in errors {
        let span = error.span();
        let error = ParseError {
            source_code: NamedSource::new(path.to_str().unwrap(), source_code.clone()),
            inner: error.clone().into_owned(),
            span: SourceSpan::new(span.start.into(), span.end - span.start),
        };
        let error = Report::new(error);
        eprintln!("{error:?}");
    }
}

/// Returns a "naturally" sorted list of files found by searching for `glob_pattern` in `base`.
/// `checkout` is stripped as a prefix from the absolute paths recorded into `log` entries emitted
/// by this function.
///
/// # Returns
///
/// An iterator over [`Result`]s containing either a checkout file's path and contents as a UTF-8
/// string, or the sentinel of an error encountered for the same file that is already reported to
/// the command line.
///
/// # Panics
///
/// This function will panick if `checkout` cannot be stripped as a prefix of `base`.
fn read_files_at(
    checkout: &Path,
    base: &Path,
    glob_pattern: &str,
) -> impl Iterator<Item = Result<(PathBuf, String), AlreadyReportedToCommandline>> {
    log::debug!("reading {glob_pattern} files at {}", base.display());
    let mut found_read_err = false;
    let mut paths = Glob::new(glob_pattern)
        .unwrap()
        .walk(base)
        .filter_map(|entry| match entry {
            Ok(entry) => Some(entry.path().to_owned()),
            Err(e) => {
                let path_disp = e
                    .path()
                    .map(|p| format!(" in {}", p.strip_prefix(checkout).unwrap().display()));
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
            .map(|f| f.strip_prefix(checkout).unwrap())
            .collect::<std::collections::BTreeSet<_>>()
    );

    let iter = paths.into_iter().map(|path| -> Result<_, _> {
        log::debug!("reading from {}…", path.display());
        fs::read_to_string(&path)
            .map_err(|e| {
                log::error!("failed to read {path:?}: {e}");
                AlreadyReportedToCommandline
            })
            .map(|file_contents| (path, file_contents))
    });

    let (read_err_iter, file_read_iter) = if found_read_err {
        (Some(Err(AlreadyReportedToCommandline)), None)
    } else {
        (None, Some(iter))
    };

    read_err_iter
        .into_iter()
        .chain(file_read_iter.into_iter().flatten())
}

/// Search for source code repository root either via Mercurial or Git, iterating from the CWD to
/// its parent directories.
///
/// This function reports to `log` automatically, so no meaningful [`Err`] value is returned.
fn search_for_repo_root() -> Result<PathBuf, AlreadyReportedToCommandline> {
    use lets_find_up::{find_up_with, FindUpKind, FindUpOptions};

    let find_up_opts = || FindUpOptions {
        cwd: Path::new("."),
        kind: FindUpKind::Dir,
    };
    let find_up = |repo_tech_name, root_dir_name| {
        log::debug!("searching for {repo_tech_name} repository root…");
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
    let source_root = find_up("Mercurial", ".hg").or_else(|hg_err| {
        find_up("Git", ".git").or_else(|git_err| match find_up("Jujutsu", ".jj") {
            Ok(path) => {
                log::debug!("{hg_err:?}");
                Ok(path)
            }
            Err(jj_err) => {
                log::warn!("{hg_err:?}");
                log::warn!("{git_err:?}");
                log::warn!("{jj_err:?}");
                log::error!("failed to automatically find a repository root");
                Err(AlreadyReportedToCommandline)
            }
        })
    })?;

    log::debug!("detected repository root at {}", source_root.display());

    Ok(source_root)
}

struct AlreadyReportedToCommandline;

fn write_to_file(path: &Path, contents: impl Display) -> Result<(), AlreadyReportedToCommandline> {
    let report_to_cmd_line = |e| {
        log::error!("{e}");
        AlreadyReportedToCommandline
    };
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(Report::msg)
            .wrap_err_with(|| {
                format!(
                    "error while ensuring parent directories exist for `{}`",
                    path.display()
                )
            })
            .map_err(report_to_cmd_line)?;
    }
    let mut out = fs::File::create(path)
        .map(BufWriter::new)
        .map_err(Report::msg)
        .wrap_err_with(|| format!("error while creating new file at `{}`", path.display()))
        .map_err(report_to_cmd_line)?;
    use io::Write;
    write!(&mut out, "{contents}")
        .map_err(Report::msg)
        .wrap_err_with(|| format!("error while writing to `{}`", path.display()))
        .map_err(report_to_cmd_line)
}

fn process_reports(
    browser: Browser,
    checkout: &Path,
    exec_report_spec: ExecReportSpec,
    preset: process_reports::ReportProcessingPreset,
    should_update_expected: &mut dyn ShouldUpdateExpected,
    on_skip_only: process_reports::OnSkipOnly,
) -> Result<(), AlreadyReportedToCommandline> {
    let exec_report_paths = exec_report_spec.paths()?;

    let meta_files_by_path =
        read_and_parse_all_metadata(browser, checkout).collect::<Result<IndexMap<_, _>, _>>()?;

    let files = process_reports::process_reports(ProcessReportsArgs {
        browser,
        checkout,
        exec_report_paths,
        preset,
        should_update_expected,
        meta_files_by_path,
        on_skip_only,
    })?;

    log::debug!("processing complete, writing new metadata to file system…");

    let mut writeback_err = false;

    for (path, file) in files {
        log::debug!("writing new metadata to {}", path.display());
        match write_to_file(&path, metadata::format_file(&file)) {
            Ok(()) => (),
            Err(AlreadyReportedToCommandline) => {
                writeback_err = true;
            }
        }
    }

    if writeback_err {
        log::error!(concat!(
            "one or more errors found while writing metadata back to disk, ",
            "exiting with failure; see above for more details"
        ));
        return Err(AlreadyReportedToCommandline);
    }

    Ok(())
}

/// Ensure that _both_ `TIMEOUT` and `NOTRUN` are in outcomes if at least one of them are present.
///
/// This transformation is desirable for reaching convergence quickly in tests where it may require
/// a high number of test runs to empirically observe all places where `TIMEOUT`s may occur. The
/// motivating example in Firefox's test runs are tests with a large matrix of subtests that are
/// deterministic if executed, but consistently exceed the timeout window offered by the test
/// runner.
fn taint_subtest_timeouts_by_suspicion(expected: &mut Expected<SubtestOutcome>) {
    let timeout_and_notrun =
        Expected::intermittent(SubtestOutcome::Timeout | SubtestOutcome::NotRun).unwrap();
    if !expected.is_disjoint(timeout_and_notrun.inner())
        && !expected.is_superset(&timeout_and_notrun)
    {
        static PRINTED_WARNING: AtomicBool = AtomicBool::new(false);
        let already_printed_warning = PRINTED_WARNING.swap(true, atomic::Ordering::Relaxed);
        if !already_printed_warning {
            log::debug!("encountered at least one case where taint-by-suspicion is being applied…")
        }
        *expected |= SubtestOutcome::Timeout | SubtestOutcome::NotRun;
    }
}
