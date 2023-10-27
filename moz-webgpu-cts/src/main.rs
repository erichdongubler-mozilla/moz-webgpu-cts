mod metadata;
mod shared;

use self::{
    metadata::{AnalyzeableProps, Platform, SubtestOutcome, Test, TestOutcome},
    shared::{Expectation, MaybeCollapsed},
};

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    fs,
    io::{self, BufWriter},
    path::{Path, PathBuf},
    process::ExitCode,
    sync::Arc,
};

use clap::Parser;
use indexmap::{IndexMap, IndexSet};
use miette::{miette, Diagnostic, NamedSource, Report, SourceSpan, WrapErr};
use path_dsl::path;

use regex::Regex;
use wax::Glob;
use whippit::{
    metadata::{SectionHeader, Subtest},
    reexport::chumsky::prelude::Rich,
};

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
    #[clap(name = "fmt")]
    Format,
    Triage,
    ReadTestVariants,
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
        Subcommand::Format => {
            let raw_test_files_by_path = match read_metadata() {
                Ok(paths) => paths,
                Err(AlreadyReportedToCommandline) => return ExitCode::FAILURE,
            };
            log::info!("formatting metadata in-place…");
            let mut fmt_err_found = false;
            for (path, file_contents) in raw_test_files_by_path {
                match chumsky::Parser::parse(&metadata::File::parser(), &*file_contents)
                    .into_result()
                {
                    Err(errors) => {
                        fmt_err_found = true;
                        render_metadata_parse_errors(&path, &file_contents, errors);
                    }
                    Ok(file) => {
                        let mut out =
                            match fs::File::create(&*path).map_err(Report::msg).wrap_err_with(
                                || format!("error while reading file `{}`", path.display()),
                            ) {
                                Ok(f) => BufWriter::new(f),
                                Err(e) => {
                                    fmt_err_found = true;
                                    log::error!("{e}");
                                    continue;
                                }
                            };
                        use io::Write;
                        match write!(&mut out, "{}", metadata::format_file(&file))
                            .map_err(Report::msg)
                            .wrap_err_with(|| {
                                format!("error while writing to `{}`", path.display())
                            }) {
                            Ok(()) => (),
                            Err(e) => {
                                log::error!("{e}");
                                continue;
                            }
                        }
                    }
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
                inner: metadata::Test,
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
                            Ok(metadata::File { tests }) => Some(tests.into_iter().map(|inner| {
                                let SectionHeader(name) = &inner.name;
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
            for (_nice_name, test) in tests_by_name {
                let TaggedTest {
                    orig_path: _,
                    inner: test,
                } = test;

                let Test {
                    name: SectionHeader(test_name),
                    properties,
                    subtests,
                    ..
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

                fn apply_expectation<Out, F>(expectation: Expectation<Out>, mut f: F)
                where
                    F: FnMut(Out),
                {
                    match expectation {
                        Expectation::Permanent(outcome) => f(outcome),
                        Expectation::Intermittent(outcomes) => outcomes.into_iter().for_each(f),
                    }
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
