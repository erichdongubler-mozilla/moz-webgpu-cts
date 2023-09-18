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
}

fn main() {
    env_logger::init();

    let Cli {
        gecko_checkout,
        subcommand,
    } = Cli::parse();
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
                contents: &'a str,
                orig_path: &'a Path,
            }
            let test_exps_by_name = raw_test_exps_by_path
                .iter()
                .flat_map(|(path, file_contents)| {
                    wpt::expectations::test_exps()
                        .parse(file_contents)
                        .unwrap()
                        .into_iter()
                        .map(|test_exp| {
                            let TestExp { name, contents } = test_exp;
                            (
                                name.strip_prefix("cts.https.html?q=").unwrap(),
                                TestExpEntry {
                                    contents,
                                    orig_path: path,
                                },
                            )
                        })
                })
                .collect::<BTreeMap<_, _>>();
            dbg!(test_exps_by_name);
        }
    }
}
