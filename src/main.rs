use std::{
    collections::BTreeMap,
    fmt::Display,
    fs,
    path::{Path, PathBuf},
    process::ExitCode,
    sync::Arc,
};

use chumsky::{prelude::Rich, Parser as _};
use clap::Parser;
use indexmap::IndexMap;
use miette::{Diagnostic, NamedSource, SourceSpan};
use path_dsl::path;

use regex::Regex;
use whippit::metadata;

#[derive(Debug, Parser)]
struct Cli {
    #[clap(long)]
    gecko_checkout: PathBuf,
    #[clap(subcommand)]
    subcommand: Subcommand,
}

#[derive(Debug, Parser)]
enum Subcommand {
    DumpTestExps,
    ReadTestVariants,
}

fn main() -> ExitCode {
    env_logger::init();
    run(Cli::parse())
}

fn run(cli: Cli) -> ExitCode {
    let Cli {
        gecko_checkout,
        subcommand,
    } = cli;
    match subcommand {
        Subcommand::DumpTestExps => {
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

            #[derive(Debug)]
            struct Test<'a> {
                orig_path: &'a Path,
                inner: metadata::Test<'a>,
            }

            let raw_test_files_by_path =
                match read_gecko_files_at(&gecko_checkout, &webgpu_cts_meta_parent_dir, "**/*.ini")
                {
                    Ok(paths) => paths,
                    Err(()) => return ExitCode::FAILURE,
                };
            let tests_by_name = {
                let mut found_parse_err = false;
                let extracted = raw_test_files_by_path
                    .iter()
                    .filter_map(|(path, file_contents)| {
                        match metadata::File::parser().parse(file_contents).into_result() {
                            Ok(metadata::File { tests }) => Some(tests.into_iter().map(|inner| {
                                (
                                    inner
                                        .name
                                        .strip_prefix("cts.https.html?q=")
                                        .unwrap()
                                        .to_owned(),
                                    Test {
                                        inner,
                                        orig_path: path,
                                    },
                                )
                            })),
                            Err(errors) => {
                                #[derive(Debug, Diagnostic, thiserror::Error)]
                                #[error("{inner}")]
                                struct ParseError {
                                    #[label]
                                    span: SourceSpan,
                                    #[source_code]
                                    source_code: NamedSource,
                                    inner: Rich<'static, char>,
                                }
                                found_parse_err = true;
                                let source_code = file_contents.clone();
                                for error in errors {
                                    let span = error.span();
                                    let error = ParseError {
                                        source_code: NamedSource::new(
                                            path.to_str().unwrap(),
                                            source_code.clone(),
                                        ),
                                        inner: error.clone().into_owned(),
                                        span: SourceSpan::new(
                                            span.start.into(),
                                            (span.end - span.start).into(),
                                        ),
                                    };
                                    let error = miette::Report::new(error);
                                    eprintln!("{error:?}");
                                }
                                None
                            }
                        }
                    })
                    .flatten()
                    .collect::<BTreeMap<_, _>>();
                if found_parse_err {
                    return ExitCode::FAILURE;
                }
                extracted
            };
            println!("{tests_by_name:#?}");
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
                Ok(paths) => paths,
                Err(()) => return ExitCode::FAILURE,
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
) -> Result<IndexMap<PathBuf, Arc<String>>, ()> {
    let mut found_read_err = false;
    let mut paths = wax::Glob::new(glob_pattern)
        .unwrap()
        .walk(&base)
        .filter_map(|entry| match entry {
            Ok(entry) => Some(entry.path().to_owned()),
            Err(e) => {
                let path_disp = e
                    .path()
                    .map(|p| format!(" in {}", p.strip_prefix(&gecko_checkout).unwrap().display()));
                let path_disp: &dyn Display = match path_disp.as_ref() {
                    Some(disp) => disp,
                    None => &"",
                };
                log::error!(
                    "failed to enumerate `cts.https.html` files{}\n  caused by: {e}",
                    path_disp
                );
                found_read_err = true;
                None
            }
        })
        .collect::<Vec<_>>();

    paths.sort_by(|a, b| natord::compare(a.to_str().unwrap(), b.to_str().unwrap()));
    let paths = paths;

    log::info!(
        "working with these files: {:#?}",
        paths
            .iter()
            .map(|f| f.strip_prefix(&gecko_checkout).unwrap())
            .collect::<std::collections::BTreeSet<_>>()
    );

    if found_read_err {
        return Err(());
    }

    let files = paths
        .into_iter()
        .filter_map(|file| {
            log::debug!("reading from {}…", file.display());
            match fs::read_to_string(&file) {
                Err(e) => {
                    log::error!("failed to read {file:?}: {e}");
                    found_read_err = true;
                    None
                }
                Ok(contents) => Some((file, Arc::new(contents))),
            }
        })
        .collect::<IndexMap<_, _>>();

    if found_read_err {
        return Err(());
    }

    Ok(files)
}
