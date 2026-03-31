//! Data representation of WPT test reports. The conceptual entry point of this module is the
//! [`ExecutionReport`] API.

use serde::{
    de::{Deserializer, Error},
    Deserialize,
};

use crate::metadata::{BuildProfile, Platform, SubtestOutcome, TestOutcome};

/// A `wptreport.json` file emitted by `wptrunner` and parsed by `moz-webgpu-cts` (which means it
/// adheres to the strict subset that `moz-webgpu-cts` supports).
#[derive(Debug, Deserialize)]
pub(crate) struct ExecutionReport {
    pub run_info: RunInfo,
    #[serde(rename = "results")]
    pub entries: Vec<TestExecutionEntry>,
}

/// An [`ExecutionReport::run_info`].
#[derive(Debug)]
pub(crate) struct RunInfo {
    pub platform: Platform,
    pub build_profile: BuildProfile,
}

impl<'de> Deserialize<'de> for RunInfo {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Debug, Deserialize)]
        struct ActualRunInfo {
            os: String,
            processor: String,
            debug: bool,
        }

        let ActualRunInfo {
            os,
            processor,
            debug,
        } = ActualRunInfo::deserialize(deserializer)?;

        let platform = match &*os {
            "win" => {
                if processor == "x86_64" {
                    Platform::Windows
                } else {
                    return Err(D::Error::custom(
                        "platform was `win`, but `processor` was not `x86_64`",
                    ));
                }
            }
            "mac" => Platform::MacOs,
            "linux" => Platform::Linux,
            other => return Err(D::Error::custom(format!("unrecognized platform {other:?}"))),
        };

        let build_profile = if debug {
            BuildProfile::Debug
        } else {
            BuildProfile::Optimized
        };

        Ok(RunInfo {
            platform,
            build_profile,
        })
    }
}

/// An entry in [`ExecutionReport::entries`].
#[derive(Debug, Deserialize)]
pub(crate) struct TestExecutionEntry {
    #[serde(rename = "test")]
    pub test_name: String,
    #[serde(flatten)]
    pub result: TestExecutionResult,
}

/// A [`TestExecutionEntry::result`].
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub(crate) enum TestExecutionResult {
    Complete {
        #[serde(rename = "status")]
        outcome: TestOutcome,
        subtests: Vec<SubtestExecutionResult>,
    },
    JobMaybeTimedOut {
        status: String,
        subtests: Vec<SubtestExecutionResult>,
    },
}

/// A subtest entry in [`TestExecutionResult`].
#[derive(Debug, Deserialize)]
pub(crate) struct SubtestExecutionResult {
    #[serde(rename = "name")]
    pub subtest_name: String,
    #[serde(rename = "status")]
    pub outcome: SubtestOutcome,
}
