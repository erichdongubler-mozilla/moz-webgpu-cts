#![allow(special_module_name)]
mod lib;

use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

use chumsky::Parser as _;
use clap::Parser;
use indexmap::IndexMap;
use path_dsl::path;

use lib::wpt::{self, expectations::TestExp};
use regex::Regex;

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

fn main() {
    env_logger::init();
    run(Cli::parse())
}

fn run(cli: Cli) {
    let Cli {
        gecko_checkout,
        subcommand,
    } = cli;
    match subcommand {
        Subcommand::DumpTestExps => {
            let raw_test_exps_by_path = (1..=51)
                .into_iter()
                .map(|chunk| {
                    let wpt_expectation_file_path = {
                        let chunk = chunk.to_string();
                        path!(
                            &gecko_checkout
                                | "testing"
                                | "web-platform"
                                | "mozilla"
                                | "meta"
                                | "webgpu"
                                | "chunked"
                                | &chunk
                                | "cts.https.html.ini"
                        )
                    };
                    eprintln!("{}", wpt_expectation_file_path.display());
                    let contents = fs::read_to_string(&wpt_expectation_file_path).unwrap();
                    (
                        wpt_expectation_file_path
                            .strip_prefix(&gecko_checkout)
                            .unwrap()
                            .to_owned(),
                        contents,
                    )
                })
                .collect::<IndexMap<_, _>>();
            #[derive(Debug)]
            struct TestExpEntry<'a> {
                orig_path: &'a Path,
                inner: TestExp<'a>,
            }
            let test_exps_by_name = raw_test_exps_by_path
                .iter()
                .flat_map(|(path, file_contents)| {
                    wpt::expectations::test_exps()
                        .parse(file_contents)
                        .unwrap()
                        .into_iter()
                        .map(|inner| {
                            (
                                inner.name.strip_prefix("cts.https.html?q=").unwrap(),
                                TestExpEntry {
                                    inner: inner,
                                    orig_path: path,
                                },
                            )
                        })
                })
                .collect::<BTreeMap<_, _>>();
            dbg!(test_exps_by_name);
        }
        Subcommand::ReadTestVariants => {
            let tests_by_path = (1..=51)
                .into_iter()
                .map(|chunk| {
                    let wpt_file_path = {
                        let chunk = chunk.to_string();
                        path!(
                            &gecko_checkout
                                | "testing"
                                | "web-platform"
                                | "mozilla"
                                | "tests"
                                | "webgpu"
                                | "chunked"
                                | &chunk
                                | "cts.https.html"
                        )
                    };
                    eprintln!("{}", wpt_file_path.display());
                    let contents = fs::read_to_string(&wpt_file_path).unwrap();
                    (
                        wpt_file_path
                            .strip_prefix(&gecko_checkout)
                            .unwrap()
                            .to_owned(),
                        contents,
                    )
                })
                .collect::<IndexMap<_, _>>();

            let meta_variant_re =
                Regex::new(r#"^<meta name=variant content='\?q=(?P<variant_path>.*?)'>$"#).unwrap();
            let meta_variant_re = &meta_variant_re;
            let variants = tests_by_path
                .iter()
                .flat_map(|(test_path, file_path)| {
                    file_path.lines().filter_map(move |line| {
                        meta_variant_re.captures(line).map(move |captures| {
                            (captures.name("variant_path").unwrap().as_str(), test_path)
                        })
                    })
                })
                .collect::<BTreeMap<_, _>>();
            dbg!(variants);
        }
    }
}
