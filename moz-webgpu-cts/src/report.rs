use serde::{
    de::{Deserializer, Error},
    Deserialize,
};

use crate::metadata::{BuildProfile, Platform, SubtestOutcome, TestOutcome};

#[derive(Debug, Deserialize)]
pub(crate) struct ExecutionReport {
    pub run_info: RunInfo,
    #[serde(rename = "results")]
    pub entries: Vec<TestExecutionEntry>,
}

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
            win11_2009: bool,
            debug: bool,
        }

        let ActualRunInfo {
            os,
            processor,
            win11_2009,
            debug,
        } = ActualRunInfo::deserialize(deserializer)?;

        let platform = match &*os {
            "win" => {
                if processor == "x86_64" && win11_2009 {
                    Platform::Windows
                } else {
                    return Err(D::Error::custom("unrecognized Windows version"));
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

#[derive(Debug, Deserialize)]
pub(crate) struct TestExecutionEntry {
    #[serde(rename = "test")]
    pub test_name: String,
    #[serde(flatten)]
    pub result: TestExecutionResult,
}

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

#[derive(Debug, Deserialize)]
pub(crate) struct SubtestExecutionResult {
    #[serde(rename = "name")]
    pub subtest_name: String,
    #[serde(rename = "status")]
    pub outcome: SubtestOutcome,
}
