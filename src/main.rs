#![allow(special_module_name)]
mod lib;

use std::{fs, path::PathBuf};

use chumsky::Parser as _;
use clap::Parser;
use path_dsl::path;

use lib::wpt::{self, expectations::TestExp};

#[derive(Debug, Parser)]
struct Cli {
    #[clap(long)]
    gecko_checkout: PathBuf,
}

fn main() {
    env_logger::init();

    let Cli { gecko_checkout } = Cli::parse();
    let mut test_names = (1..=51)
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
            let wpt_expectations = fs::read_to_string(&wpt_expectation_file_path).unwrap();
            eprintln!("{}", wpt_expectation_file_path.display());
            let test_names = wpt::expectations::parser()
                .parse(&wpt_expectations)
                .unwrap()
                .into_iter()
                .map(|TestExp { name, contents: _ }| name.to_owned())
                .collect::<Vec<_>>();
            test_names
        })
        .flatten()
        .collect::<Vec<_>>();
    test_names.sort();
    dbg!(test_names);
}
