use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    path::Path,
};

use camino::{Utf8Component, Utf8Path};

use clap::ValueEnum;
use format::lazy_format;
use joinery::JoinableIterator;

/// A browser supported by [crate::main], used for [`TestEntryPath`]s.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, ValueEnum)]
pub(crate) enum Browser {
    Firefox,
    Servo,
}

impl Browser {
    /// NOTE: Keep this implementation in sync with [`RootDir::url_prefix`].
    pub(crate) fn strip_wpt_url_prefix<'a>(&self, url_path: &'a str) -> Option<(RootDir, &'a str)> {
        let strip_prefix = |prefix, root_dir| {
            url_path
                .strip_prefix(prefix)
                .map(|stripped| (root_dir, stripped))
        };
        match self {
            Browser::Firefox => strip_prefix("/_mozilla/", FirefoxRootDir::Mozilla.into())
                .or_else(|| strip_prefix("/", FirefoxRootDir::Upstream.into())),
            Browser::Servo => strip_prefix("/_webgpu/", ServoRootDir::WebGpu.into()),
        }
    }

    /// NOTE: Keep this implementation in sync with [`RootDir::components`].
    pub(crate) fn strip_wpt_root_dir_prefix<'a>(
        &self,
        path: &'a Utf8Path,
    ) -> Result<(RootDir, &'a Utf8Path), std::path::StripPrefixError> {
        let strip_prefix = |prefix, root_dir| {
            path.strip_prefix(prefix)
                .map(|stripped| (root_dir, stripped))
        };
        match self {
            Browser::Firefox => {
                strip_prefix(ROOT_DIR_FX_MOZILLA_STR, FirefoxRootDir::Mozilla.into()).or_else(
                    |_| strip_prefix(ROOT_DIR_FX_UPSTREAM_STR, FirefoxRootDir::Upstream.into()),
                )
            }
            Browser::Servo => strip_prefix(ROOT_DIR_SERVO_WEBGPU_STR, ServoRootDir::WebGpu.into()),
        }
    }
}

/// A symbolic path to a WPT test specification, which may contain one or more executed test
/// entries (see also [`TestEntry`]). Includes methods for rendering paths to test and metadata
/// files.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct SpecPath<'a> {
    pub root_dir: RootDir,
    /// A relative offset into `root_dir`.
    pub path: Cow<'a, Utf8Path>,
}

/// A symbolic path to an executed WPT test entry and its metadata, contained in a test
/// specification (see also [`SpecPath`]). In combination with [`SpecPath`], this is useful for
/// correlating entries from [`ExecutionReport`]s and [`metadata::File`]s.
///
/// [`ExecutionReport`]: crate::report::ExecutionReport
/// [`metadata::File`]: crate::wpt::metadata::File
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct TestEntry<'a> {
    /// The variant of this particular test from this test's source code. If set, you should be
    /// able to correlate this with
    ///
    /// Generally, a test in WPT is _either_ a single test, or a set of test variants. That is, for
    /// a given `path`, there will be a single `variant: None`, or multiple tests with `variant:
    /// Some(…)`.
    pub variant: Option<Cow<'a, str>>,
}

const ROOT_DIR_FX_MOZILLA_STR: &str = "testing/web-platform/mozilla";
const ROOT_DIR_FX_MOZILLA_COMPONENTS: &[&str] = &["testing", "web-platform", "mozilla"];
const ROOT_DIR_FX_UPSTREAM_STR: &str = "testing/web-platform";
const ROOT_DIR_FX_UPSTREAM_COMPONENTS: &[&str] = &["testing", "web-platform"];
const ROOT_DIR_SERVO_WEBGPU_STR: &str = "tests/wpt/webgpu";
const ROOT_DIR_SERVO_WEBGPU_COMPONENTS: &[&str] = &["tests", "wpt", "webgpu"];

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct TestEntryPath<'a> {
    pub spec_path: SpecPath<'a>,
    pub test_entry: TestEntry<'a>,
}

impl<'a> TestEntryPath<'a> {
    pub fn from_execution_report(
        browser: Browser,
        test_url_path: &'a str,
    ) -> Result<Self, ExecutionReportPathError<'a>> {
        let err = || ExecutionReportPathError { test_url_path };

        let (root_dir, path) = browser
            .strip_wpt_url_prefix(test_url_path)
            .ok_or_else(err)?;

        if path.contains('\\') {
            return Err(err());
        }

        let (path, variant) = match path.split('/').next_back() {
            Some(path_and_maybe_variants) => match path_and_maybe_variants.find('?') {
                Some(query_params_start_idx) => (
                    &path[..path.len() - (path_and_maybe_variants.len() - query_params_start_idx)],
                    Some(&path_and_maybe_variants[query_params_start_idx..]),
                ),
                None => (path, None),
            },
            None => return Err(err()),
        };

        Ok(Self {
            spec_path: SpecPath {
                root_dir,
                path: Utf8Path::new(path).into(),
            },
            test_entry: TestEntry {
                variant: variant.map(Into::into),
            },
        })
    }

    pub fn from_metadata_test(
        browser: Browser,
        rel_meta_file_path: &'a Path,
        test_name: &'a str,
    ) -> Result<Self, MetadataTestEntryPathError<'a>> {
        let rel_meta_file_path = Utf8Path::new(rel_meta_file_path.to_str().ok_or(
            MetadataTestEntryPathError {
                rel_meta_file_path,
                test_name,
            },
        )?);
        let err = || MetadataTestEntryPathError {
            rel_meta_file_path: rel_meta_file_path.as_std_path(),
            test_name,
        };
        let rel_meta_file_path = Utf8Path::new(
            rel_meta_file_path
                .as_str()
                .strip_suffix(".ini")
                .ok_or_else(err)?,
        );

        let (root_dir, path) = browser
            .strip_wpt_root_dir_prefix(rel_meta_file_path)
            .map_err(|_e| err())?;

        let Ok(path) = path.strip_prefix("meta/") else {
            return Err(err());
        };

        let (base_name, variant) = Self::split_test_base_name_from_variant(test_name);

        if path.components().next_back() != Some(Utf8Component::Normal(base_name)) {
            return Err(err());
        }

        Ok(Self {
            spec_path: SpecPath {
                root_dir,
                path: path.into(),
            },
            test_entry: TestEntry {
                variant: variant.map(Into::into),
            },
        })
    }

    fn split_test_base_name_from_variant(url_ish_name: &'a str) -> (&'a str, Option<&'a str>) {
        match url_ish_name.find('?') {
            Some(query_params_start_idx) => (
                &url_ish_name[..url_ish_name.len() - (url_ish_name.len() - query_params_start_idx)],
                Some(&url_ish_name[query_params_start_idx..]),
            ),
            None => (url_ish_name, None),
        }
    }

    pub fn into_owned(self) -> TestEntryPath<'static> {
        let Self {
            spec_path: SpecPath { root_dir, path },
            test_entry: TestEntry { variant },
        } = self;

        TestEntryPath {
            spec_path: SpecPath {
                root_dir: root_dir.clone(),
                path: path.clone().into_owned().into(),
            },
            test_entry: TestEntry {
                variant: variant.clone().map(|v| v.into_owned().into()),
            },
        }
    }

    pub(crate) fn test_name(&self) -> impl Display + '_ {
        let Self {
            spec_path: SpecPath { root_dir: _, path },
            test_entry: TestEntry { variant },
        } = self;
        let base_name = path.file_name().unwrap();

        lazy_format!(move |f| {
            write!(f, "{base_name}")?;
            if let Some(variant) = variant {
                write!(f, "{variant}")?;
            }
            Ok(())
        })
    }

    pub(crate) fn runner_url_path(&self) -> impl Display + '_ {
        let Self {
            spec_path: SpecPath { root_dir, path },
            test_entry: TestEntry { variant },
        } = self;
        lazy_format!(move |f| {
            write!(
                f,
                "{}{}",
                root_dir.url_prefix(),
                path.components().join_with('/')
            )?;
            if let Some(variant) = variant.as_ref() {
                write!(f, "{}", variant)?;
            }
            Ok(())
        })
    }

    pub(crate) fn rel_metadata_path(&self) -> impl Display + '_ {
        let Self {
            spec_path: SpecPath { root_dir, path },
            test_entry: TestEntry { variant: _ },
        } = self;

        let root_dir_dir = root_dir
            .components()
            .chain(["meta"].iter().cloned())
            .join_with(std::path::MAIN_SEPARATOR);

        lazy_format!(move |f| {
            write!(f, "{root_dir_dir}{}{path}.ini", std::path::MAIN_SEPARATOR)
        })
    }
}

/// An error encountered during [`TestEntryPath::from_execution_report`].
#[derive(Debug)]
pub struct ExecutionReportPathError<'a> {
    test_url_path: &'a str,
}

impl Display for ExecutionReportPathError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { test_url_path } = self;
        write!(
            f,
            concat!(
                "failed to derive test path from execution report's entry ",
                "for a test at URL path {:?}"
            ),
            test_url_path
        )
    }
}

/// An error encountered during [`TestEntryPath::from_metadata_test`].
#[derive(Debug)]
pub struct MetadataTestEntryPathError<'a> {
    rel_meta_file_path: &'a Path,
    test_name: &'a str,
}

impl Display for MetadataTestEntryPathError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            rel_meta_file_path,
            test_name,
        } = self;
        write!(
            f,
            "failed to derive test path from relative metadata path {:?} and test name {:?}",
            rel_meta_file_path, test_name
        )
    }
}

/// A root directory from which WPT tests and metadata are based. Based on a specific [`Browser`].
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum RootDir {
    Firefox(FirefoxRootDir),
    Servo(ServoRootDir),
}

impl RootDir {
    /// NOTE: Keep this implementation in sync with [`Browser::strip_wpt_url_prefix`].
    fn url_prefix(&self) -> &str {
        match self {
            RootDir::Firefox(root_dir) => match root_dir {
                FirefoxRootDir::Upstream => "",
                FirefoxRootDir::Mozilla => "_mozilla/",
            },
            RootDir::Servo(ServoRootDir::WebGpu) => "_webgpu/",
        }
    }

    /// NOTE: Keep this implementation in sync with [`Browser::strip_wpt_root_dir_prefix`].
    fn components(&self) -> impl Iterator<Item = &str> + Clone {
        match self {
            RootDir::Firefox(root_dir) => match root_dir {
                FirefoxRootDir::Upstream => ROOT_DIR_FX_UPSTREAM_COMPONENTS,
                FirefoxRootDir::Mozilla => ROOT_DIR_FX_MOZILLA_COMPONENTS,
            },
            RootDir::Servo(ServoRootDir::WebGpu) => ROOT_DIR_SERVO_WEBGPU_COMPONENTS,
        }
        .iter()
        .cloned()
    }
}

/// Subset of [`RootDir`] for [`Browser::Firefox`].
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum FirefoxRootDir {
    /// A public test available at some point in the history of [WPT upstream]. Note that while
    /// a test may be public, metadata associated with it is in a private location.
    ///
    /// [WPT upstream]: https://github.com/web-platform-tests/wpt
    Upstream,
    /// A private test specific to Firefox.
    Mozilla,
}

impl From<FirefoxRootDir> for RootDir {
    fn from(value: FirefoxRootDir) -> Self {
        Self::Firefox(value)
    }
}

/// Subset of [`RootDir`] for [`Browser::Servo`].
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum ServoRootDir {
    /// A WebGPU CTS test vendored into Servo's source tree.
    WebGpu,
}

impl From<ServoRootDir> for RootDir {
    fn from(value: ServoRootDir) -> Self {
        Self::Servo(value)
    }
}

#[test]
fn parse_test_entry_path() {
    assert_eq!(
        TestEntryPath::from_metadata_test(
            Browser::Firefox,
            Path::new("testing/web-platform/mozilla/meta/blarg/cts.https.html.ini"),
            "cts.https.html?stuff=things"
        )
        .unwrap(),
        TestEntryPath {
            spec_path: SpecPath {
                root_dir: FirefoxRootDir::Mozilla.into(),
                path: Utf8Path::new("blarg/cts.https.html").into(),
            },
            test_entry: TestEntry {
                variant: Some("?stuff=things".into()),
            }
        }
    );

    assert_eq!(
        TestEntryPath::from_metadata_test(
            Browser::Firefox,
            Path::new("testing/web-platform/meta/stuff/things/cts.https.html.ini"),
            "cts.https.html"
        )
        .unwrap(),
        TestEntryPath {
            spec_path: SpecPath {
                root_dir: FirefoxRootDir::Upstream.into(),
                path: Utf8Path::new("stuff/things/cts.https.html").into(),
            },
            test_entry: TestEntry { variant: None }
        }
    );

    assert_eq!(
        TestEntryPath::from_metadata_test(
            Browser::Servo,
            Path::new("tests/wpt/webgpu/meta/webgpu/cts.https.html.ini"),
            "cts.https.html?stuff=things"
        )
        .unwrap(),
        TestEntryPath {
            spec_path: SpecPath {
                root_dir: ServoRootDir::WebGpu.into(),
                path: Utf8Path::new("webgpu/cts.https.html").into(),
            },
            test_entry: TestEntry {
                variant: Some("?stuff=things".into()),
            }
        }
    );
}

#[test]
fn report_meta_match() {
    macro_rules! assert_test_matches_meta {
        ($browser:expr, $test_run_path:expr, $rel_meta_path:expr, $test_section_header:expr) => {
            assert_eq!(
                TestEntryPath::from_execution_report($browser, $test_run_path).unwrap(),
                TestEntryPath::from_metadata_test(
                    $browser,
                    Path::new($rel_meta_path),
                    $test_section_header
                )
                .unwrap()
            );
            assert_eq!(
                format!(
                    "/{}",
                    TestEntryPath::from_execution_report($browser, $test_run_path)
                        .unwrap()
                        .runner_url_path()
                ),
                $test_run_path,
            );
        };
    }

    assert_test_matches_meta!(
        Browser::Firefox,
        "/_mozilla/blarg/cts.https.html?stuff=things",
        "testing/web-platform/mozilla/meta/blarg/cts.https.html.ini",
        "cts.https.html?stuff=things"
    );

    assert_test_matches_meta!(
        Browser::Firefox,
        "/blarg/cts.https.html?stuff=things",
        "testing/web-platform/meta/blarg/cts.https.html.ini",
        "cts.https.html?stuff=things"
    );

    assert_test_matches_meta!(
        Browser::Servo,
        "/_webgpu/webgpu/cts.https.html?stuff=things",
        "tests/wpt/webgpu/meta/webgpu/cts.https.html.ini",
        "cts.https.html?stuff=things"
    );
}

#[test]
fn report_meta_reject() {
    macro_rules! assert_test_rejects_meta {
        (
            $browser: expr,
            $test_run_path:expr,
            $rel_meta_path:expr,
            $test_section_header:expr
        ) => {
            assert_ne!(
                TestEntryPath::from_execution_report($browser, $test_run_path).unwrap(),
                TestEntryPath::from_metadata_test(
                    $browser,
                    Path::new($rel_meta_path),
                    $test_section_header
                )
                .unwrap()
            )
        };
    }

    assert_test_rejects_meta!(
        Browser::Firefox,
        "/blarg/cts.https.html?stuff=things",
        // Wrong: the `mozilla` component shouldn't be after `web-platform`
        "testing/web-platform/mozilla/meta/blarg/cts.https.html.ini",
        "cts.https.html?stuff=things"
    );

    assert_test_rejects_meta!(
        Browser::Firefox,
        "/_mozilla/blarg/cts.https.html?stuff=things",
        // Wrong: missing the `mozilla` component after `web-platform`
        "testing/web-platform/meta/blarg/cts.https.html.ini",
        "cts.https.html?stuff=things"
    );
}

#[test]
fn runner_url_path() {
    assert_eq!(
        TestEntryPath::from_metadata_test(
            Browser::Firefox,
            Path::new("testing/web-platform/meta/blarg/stuff.https.html.ini"),
            "stuff.https.html"
        )
        .unwrap()
        .runner_url_path()
        .to_string(),
        "blarg/stuff.https.html",
    );

    assert_eq!(
        TestEntryPath::from_metadata_test(
            Browser::Firefox,
            Path::new("testing/web-platform/meta/blarg/stuff.https.html.ini"),
            "stuff.https.html?win"
        )
        .unwrap()
        .runner_url_path()
        .to_string(),
        "blarg/stuff.https.html?win",
    );

    assert_eq!(
        TestEntryPath::from_metadata_test(
            Browser::Firefox,
            Path::new("testing/web-platform/mozilla/meta/blarg/stuff.https.html.ini"),
            "stuff.https.html"
        )
        .unwrap()
        .runner_url_path()
        .to_string(),
        "_mozilla/blarg/stuff.https.html",
    );

    assert_eq!(
        TestEntryPath::from_metadata_test(
            Browser::Firefox,
            Path::new("testing/web-platform/mozilla/meta/blarg/stuff.https.html.ini"),
            "stuff.https.html?win"
        )
        .unwrap()
        .runner_url_path()
        .to_string(),
        "_mozilla/blarg/stuff.https.html?win",
    );

    assert_eq!(
        TestEntryPath::from_metadata_test(
            Browser::Servo,
            Path::new("tests/wpt/webgpu/meta/webgpu/cts.https.html.ini"),
            "cts.https.html?win"
        )
        .unwrap()
        .runner_url_path()
        .to_string(),
        "_webgpu/webgpu/cts.https.html?win",
    );
}
