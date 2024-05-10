use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt::{self, Debug, Display, Formatter},
    num::NonZeroUsize,
    ops::{BitOr, BitOrAssign, Index, IndexMut},
    path::Path,
};

use camino::{Utf8Component, Utf8Path};

use clap::ValueEnum;
use enum_map::EnumMap;
use enumset::{EnumSet, EnumSetType};
use format::lazy_format;
use itertools::Itertools;
use joinery::JoinableIterator;
use serde::Serialize;

use crate::metadata::{BuildProfile, Platform};

/// A non-empty set of expected outcomes in a [`Test`] or [`Subtest`].
///
/// The default expected test outcome is a "good" outcome, where testing passes. The `Out` type
/// parameter should return this value in its implementation of `Default`.
///
/// [`Test`]: crate::metadata::Test
/// [`Subtest`]: crate::metadata::Subtest
#[derive(Clone, Copy, Serialize)]
pub struct Expected<Out>(EnumSet<Out>)
where
    Out: EnumSetType;

impl<Out> Default for Expected<Out>
where
    Out: Default + EnumSetType,
{
    fn default() -> Self {
        Self::permanent(Out::default())
    }
}

impl<Out> Expected<Out>
where
    Out: EnumSetType,
{
    /// Returns [`None`] if `outcomes` is empty.
    #[track_caller]
    pub fn new(outcomes: EnumSet<Out>) -> Option<Self> {
        (!outcomes.is_empty()).then_some(Self(outcomes))
    }

    pub fn permanent(outcome: Out) -> Self {
        Self(EnumSet::from_iter([outcome]))
    }

    #[track_caller]
    pub fn intermittent(outcomes: EnumSet<Out>) -> Option<Self> {
        if outcomes.len() <= 1 {
            None
        } else {
            Some(Self::new(outcomes).unwrap())
        }
    }

    pub fn inner(&self) -> EnumSet<Out> {
        let Self(inner) = self;
        *inner
    }

    pub fn len(&self) -> NonZeroUsize {
        self.inner()
            .len()
            .try_into()
            .expect("invariant violation: empty `Expected`")
    }

    pub fn is_permanent(&self) -> bool {
        self.len().get() == 1
    }

    pub fn as_permanent(&self) -> Option<Out> {
        self.is_permanent().then(|| self.iter().next().unwrap())
    }

    pub fn iter(&self) -> impl Iterator<Item = Out> {
        self.inner().iter()
    }

    pub fn is_disjoint(&self, rep: EnumSet<Out>) -> bool {
        self.inner().is_disjoint(rep)
    }

    pub fn is_superset(&self, rep: &Self) -> bool
    where
        Out: std::fmt::Debug + Default + EnumSetType,
    {
        self.inner().is_superset(rep.inner())
    }
}

impl<Out> Display for Expected<Out>
where
    Out: Display + EnumSetType,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(perma) = self.as_permanent() {
            write!(f, "{perma}")
        } else {
            f.debug_list()
                .entries(
                    self.iter()
                        .map(|out| format::Debug(move |f| write!(f, "{out}"))),
                )
                .finish()
        }
    }
}

impl<Out> Debug for Expected<Out>
where
    Out: Debug + EnumSetType,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<Out> BitOr for Expected<Out>
where
    Out: EnumSetType,
{
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        let Self(lhs) = self;
        let Self(rhs) = rhs;

        Self(lhs | rhs)
    }
}

impl<Out> BitOrAssign for Expected<Out>
where
    Out: EnumSetType,
{
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl<Out> BitOr<EnumSet<Out>> for Expected<Out>
where
    Out: EnumSetType,
{
    type Output = Self;

    fn bitor(self, rhs: EnumSet<Out>) -> Self::Output {
        let Self(lhs) = self;

        Self(lhs | rhs)
    }
}

impl<Out> BitOrAssign<EnumSet<Out>> for Expected<Out>
where
    Out: EnumSetType,
{
    fn bitor_assign(&mut self, rhs: EnumSet<Out>) {
        *self = *self | rhs;
    }
}

impl<Out> BitOr<Out> for Expected<Out>
where
    Out: EnumSetType,
{
    type Output = Self;

    fn bitor(self, rhs: Out) -> Self::Output {
        let Self(lhs) = self;

        Self(lhs | rhs)
    }
}

impl<Out> BitOrAssign<Out> for Expected<Out>
where
    Out: EnumSetType,
{
    fn bitor_assign(&mut self, rhs: Out) {
        *self = *self | rhs;
    }
}

impl<Out> PartialEq for Expected<Out>
where
    Out: EnumSetType + Eq,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl<Out> PartialEq<Out> for Expected<Out>
where
    Out: EnumSetType + Eq,
{
    fn eq(&self, other: &Out) -> bool {
        self.inner() == *other
    }
}

impl<Out> Eq for Expected<Out> where Out: EnumSetType + Eq {}

/// Similar to the ubiquitous `enum Either`, but with the implication that `Collapsed` values are
/// abbreviations of equivalent `Expanded` values.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MaybeCollapsed<C, E> {
    Collapsed(C),
    Expanded(E),
}

impl<C, E> Default for MaybeCollapsed<C, E>
where
    C: Default,
{
    fn default() -> Self {
        Self::Collapsed(Default::default())
    }
}

/// A completely flat representation of [`NormalizedPropertyValue`] suitable for byte
/// representation in memory.
#[derive(Debug, Default, Clone, Copy, Eq, PartialEq, Serialize)]
pub struct ExpandedPropertyValue<T>(ExpandedPropertyValueData<T>);

impl<T> Index<(Platform, BuildProfile)> for ExpandedPropertyValue<T> {
    type Output = T;

    fn index(&self, (platform, build_profile): (Platform, BuildProfile)) -> &Self::Output {
        &self.0[platform][build_profile]
    }
}

impl<Out> IndexMut<(Platform, BuildProfile)> for ExpandedPropertyValue<Out> {
    fn index_mut(
        &mut self,
        (platform, build_profile): (Platform, BuildProfile),
    ) -> &mut Self::Output {
        &mut self.0[platform][build_profile]
    }
}

impl<T> ExpandedPropertyValue<T>
where
    T: Clone,
{
    pub fn unconditional(value: T) -> Self {
        Self::from_query(|_, _| value.clone())
    }
}

impl<T> ExpandedPropertyValue<T> {
    pub(crate) fn into_iter(self) -> impl Iterator<Item = ((Platform, BuildProfile), T)> {
        let Self(inner) = self;
        inner.into_iter().flat_map(|(platform, outcomes)| {
            outcomes
                .into_iter()
                .map(move |(build_profile, outcomes)| ((platform, build_profile), outcomes))
        })
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = ((Platform, BuildProfile), &T)> {
        self.inner().iter().flat_map(|(platform, outcomes)| {
            outcomes
                .iter()
                .map(move |(build_profile, outcomes)| ((platform, build_profile), outcomes))
        })
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = ((Platform, BuildProfile), &mut T)> {
        self.inner_mut()
            .iter_mut()
            .flat_map(|(platform, outcomes)| {
                outcomes
                    .iter_mut()
                    .map(move |(build_profile, outcomes)| ((platform, build_profile), outcomes))
            })
    }

    pub(crate) fn inner(&self) -> &ExpandedPropertyValueData<T> {
        let Self(inner) = self;
        inner
    }

    fn inner_mut(&mut self) -> &mut ExpandedPropertyValueData<T> {
        let Self(inner) = self;
        inner
    }
}

impl<T> ExpandedPropertyValue<T> {
    pub fn from_query<F>(f: F) -> Self
    where
        F: FnMut(Platform, BuildProfile) -> T,
    {
        let mut f = f;
        Self(EnumMap::from_fn(|platform| {
            EnumMap::from_fn(|build_profile| f(platform, build_profile))
        }))
    }
}

pub type ExpandedPropertyValueData<T> = EnumMap<Platform, EnumMap<BuildProfile, T>>;

#[test]
fn expanded_expected_is_tiny() {
    use crate::metadata::{SubtestOutcome, TestOutcome};
    use std::mem::size_of;

    assert_eq!(size_of::<ExpandedPropertyValue<Expected<TestOutcome>>>(), 6);
    assert_eq!(
        size_of::<ExpandedPropertyValue<Expected<SubtestOutcome>>>(),
        6
    );
}

/// A normalized representation of a property in [`TestProps`], which collapses backwards along the
/// following branching factors:
///
/// * [`Platform`]
/// * [`BuildProfile`]
///
/// Yes, the type is _gnarly_. Sorry about that. This is some complex domain, okay? 😆😭
///
/// [`TestProps`]: crate::metadata::TestProps
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NormalizedPropertyValue<T>(NormalizedPropertyValueData<T>);

impl<T> Default for NormalizedPropertyValue<T>
where
    T: Default,
{
    fn default() -> Self {
        Self(MaybeCollapsed::Collapsed(MaybeCollapsed::Collapsed(
            Default::default(),
        )))
    }
}

impl<T> NormalizedPropertyValue<T> {
    pub fn inner(&self) -> &NormalizedPropertyValueData<T> {
        let Self(inner) = self;
        inner
    }
}

impl<T> NormalizedPropertyValue<T>
where
    T: Clone + Eq,
{
    pub(crate) fn from_expanded(outcomes: ExpandedPropertyValue<T>) -> Self {
        Self(
            if let Ok(uniform) = outcomes
                .iter()
                .map(|(_, outcome)| outcome)
                .all_equal_value()
            {
                MaybeCollapsed::Collapsed(MaybeCollapsed::Collapsed(uniform.clone()))
            } else {
                let per_bp = |outcomes_by_bp: &EnumMap<_, _>| {
                    outcomes_by_bp
                        .iter()
                        .map(|(bp, outcomes): (_, &T)| (bp, outcomes.clone()))
                        .collect()
                };
                if let Ok(uniform_per_platform) = outcomes
                    .inner()
                    .iter()
                    .map(|(_, outcomes)| outcomes)
                    .all_equal_value()
                {
                    MaybeCollapsed::Collapsed(MaybeCollapsed::Expanded(per_bp(
                        uniform_per_platform,
                    )))
                } else {
                    MaybeCollapsed::Expanded(
                        outcomes
                            .inner()
                            .iter()
                            .map(|(platform, outcomes_by_bp)| {
                                if let Ok(uniform_per_bp) = outcomes_by_bp
                                    .iter()
                                    .map(|(_, outcomes)| outcomes.clone())
                                    .all_equal_value()
                                {
                                    (platform, MaybeCollapsed::Collapsed(uniform_per_bp))
                                } else {
                                    (platform, MaybeCollapsed::Expanded(per_bp(outcomes_by_bp)))
                                }
                            })
                            .collect(),
                    )
                }
            },
        )
    }
}

/// Data from a [`NormalizedPropertyValue`].
pub type NormalizedPropertyValueData<T> = Normalized<Platform, Normalized<BuildProfile, T>>;

/// A value that is either `V` or a set of `V`s branched on by `K`.
pub type Normalized<K, V> = MaybeCollapsed<V, BTreeMap<K, V>>;

/// A single symbolic path to a test and its metadata.
///
/// This API is useful as a common representation of a path for [`ExecutionReport`]s and
/// [`metadata::File`]s.
///
/// [`ExecutionReport`]: crate::report::ExecutionReport
/// [`metadata::File`]: crate::metadata::File
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct TestPath<'a> {
    pub scope: TestScope,
    /// A relative offset into `scope`.
    pub path: Cow<'a, Utf8Path>,
    /// The variant of this particular test from this test's source code. If set, you should be
    /// able to correlate this with
    ///
    /// Generally, a test in WPT is _either_ a single test, or a set of test variants. That is, for
    /// a given `path`, there will be a single `variant: None`, or multiple tests with `variant:
    /// Some(…)`.
    pub variant: Option<Cow<'a, str>>,
}

const SCOPE_DIR_FX_MOZILLA_STR: &str = "testing/web-platform/mozilla";
const SCOPE_DIR_FX_MOZILLA_COMPONENTS: &[&str] = &["testing", "web-platform", "mozilla"];
const SCOPE_DIR_FX_UPSTREAM_STR: &str = "testing/web-platform";
const SCOPE_DIR_FX_UPSTREAM_COMPONENTS: &[&str] = &["testing", "web-platform"];
const SCOPE_DIR_SERVO_WEBGPU_STR: &str = "tests/wpt/webgpu";
const SCOPE_DIR_SERVO_WEBGPU_COMPONENTS: &[&str] = &["tests", "wpt", "webgpu"];

impl<'a> TestPath<'a> {
    pub fn from_execution_report(
        browser: Browser,
        test_url_path: &'a str,
    ) -> Result<Self, ExecutionReportPathError<'a>> {
        let err = || ExecutionReportPathError { test_url_path };

        let try_strip_with = |prefix, visibility| {
            test_url_path
                .strip_prefix(prefix)
                .map(|stripped| (visibility, stripped))
        };
        let vis_and_path = match browser {
            Browser::Firefox => try_strip_with("/_mozilla/", TestVisibility::Private),
            Browser::Servo => try_strip_with("/_webgpu/", TestVisibility::Public),
        }
        .or_else(|| try_strip_with("/", TestVisibility::Public));
        let Some((visibility, path)) = vis_and_path else {
            return Err(err());
        };
        let scope = TestScope {
            browser,
            visibility,
        };

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
            scope,
            path: Utf8Path::new(path).into(),
            variant: variant.map(Into::into),
        })
    }

    pub fn from_metadata_test(
        browser: Browser,
        rel_meta_file_path: &'a Path,
        test_name: &'a str,
    ) -> Result<Self, MetadataTestPathError<'a>> {
        let rel_meta_file_path =
            Utf8Path::new(rel_meta_file_path.to_str().ok_or(MetadataTestPathError {
                rel_meta_file_path,
                test_name,
            })?);
        let err = || MetadataTestPathError {
            rel_meta_file_path: rel_meta_file_path.as_std_path(),
            test_name,
        };
        let rel_meta_file_path = Utf8Path::new(
            rel_meta_file_path
                .as_str()
                .strip_suffix(".ini")
                .ok_or(err())?,
        );

        let (private_path, public_path) = match browser {
            Browser::Firefox => (SCOPE_DIR_FX_MOZILLA_STR, SCOPE_DIR_FX_UPSTREAM_STR),
            Browser::Servo => (SCOPE_DIR_FX_MOZILLA_STR, SCOPE_DIR_SERVO_WEBGPU_STR),
        };
        let (visibility, path) = if let Ok(path) = rel_meta_file_path.strip_prefix(private_path) {
            (TestVisibility::Private, path)
        } else if let Ok(path) = rel_meta_file_path.strip_prefix(public_path) {
            (TestVisibility::Public, path)
        } else {
            return Err(err());
        };
        let scope = TestScope {
            browser,
            visibility,
        };

        let Ok(path) = path.strip_prefix("meta/") else {
            return Err(err());
        };

        let (base_name, variant) = Self::split_test_base_name_from_variant(test_name);

        if path.components().next_back() != Some(Utf8Component::Normal(base_name)) {
            return Err(err());
        }

        Ok(Self {
            scope,
            path: Utf8Path::new(path).into(),
            variant: variant.map(Into::into),
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

    pub fn into_owned(self) -> TestPath<'static> {
        let Self {
            scope,
            path,
            variant,
        } = self;

        TestPath {
            scope: scope.clone(),
            path: path.clone().into_owned().into(),
            variant: variant.clone().map(|v| v.into_owned().into()),
        }
    }

    pub(crate) fn test_name(&self) -> impl Display + '_ {
        let Self {
            path,
            variant,
            scope: _,
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
            path,
            variant,
            scope,
        } = self;
        lazy_format!(move |f| {
            let TestScope {
                browser,
                visibility,
            } = scope;
            let scope_prefix = match (browser, visibility) {
                (Browser::Firefox, TestVisibility::Public) => "",
                (Browser::Firefox, TestVisibility::Private) => "_mozilla/",
                (Browser::Servo, TestVisibility::Public) => "_webgpu/",
                (Browser::Servo, TestVisibility::Private) => todo!(),
            };
            write!(f, "{scope_prefix}{}", path.components().join_with('/'))?;
            if let Some(variant) = variant.as_ref() {
                write!(f, "{}", variant)?;
            }
            Ok(())
        })
    }

    pub(crate) fn rel_metadata_path(&self) -> impl Display + '_ {
        let Self {
            path,
            variant: _,
            scope,
        } = self;

        let TestScope {
            browser,
            visibility,
        } = scope;
        let scope_dir = match (browser, visibility) {
            (Browser::Firefox, TestVisibility::Public) => SCOPE_DIR_FX_UPSTREAM_COMPONENTS,
            (Browser::Firefox, TestVisibility::Private) => SCOPE_DIR_FX_MOZILLA_COMPONENTS,
            (Browser::Servo, TestVisibility::Public) => SCOPE_DIR_SERVO_WEBGPU_COMPONENTS,
            (Browser::Servo, TestVisibility::Private) => todo!(),
        }
        .iter()
        .chain(&["meta"])
        .join_with(std::path::MAIN_SEPARATOR);

        lazy_format!(move |f| { write!(f, "{scope_dir}{}{path}.ini", std::path::MAIN_SEPARATOR) })
    }
}

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

#[derive(Debug)]
pub struct MetadataTestPathError<'a> {
    rel_meta_file_path: &'a Path,
    test_name: &'a str,
}

impl Display for MetadataTestPathError<'_> {
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

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, ValueEnum)]
pub(crate) enum Browser {
    Firefox,
    Servo,
}

#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum TestVisibility {
    /// A public test available at some point in the history of [WPT upstream]. Note that while
    /// a test may be public, metadata associated with it is in a private location.
    ///
    /// [WPT upstream]: https://github.com/web-platform-tests/wpt
    Public,
    /// A private test specific to browser.
    Private,
}

/// Symbolically represents a file root from which tests and metadata are based.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct TestScope {
    browser: Browser,
    visibility: TestVisibility,
}

#[test]
fn parse_test_path() {
    assert_eq!(
        TestPath::from_metadata_test(
            Browser::Firefox,
            Path::new("testing/web-platform/mozilla/meta/blarg/cts.https.html.ini"),
            "cts.https.html?stuff=things"
        )
        .unwrap(),
        TestPath {
            scope: TestScope {
                browser: Browser::Firefox,
                visibility: TestVisibility::Private
            },
            path: Utf8Path::new("blarg/cts.https.html").into(),
            variant: Some("?stuff=things".into()),
        }
    );

    assert_eq!(
        TestPath::from_metadata_test(
            Browser::Firefox,
            Path::new("testing/web-platform/meta/stuff/things/cts.https.html.ini"),
            "cts.https.html"
        )
        .unwrap(),
        TestPath {
            scope: TestScope {
                browser: Browser::Firefox,
                visibility: TestVisibility::Public
            },
            path: Utf8Path::new("stuff/things/cts.https.html").into(),
            variant: None,
        }
    );

    assert_eq!(
        TestPath::from_metadata_test(
            Browser::Servo,
            Path::new("tests/wpt/webgpu/meta/webgpu/cts.https.html.ini"),
            "cts.https.html?stuff=things"
        )
        .unwrap(),
        TestPath {
            scope: TestScope {
                browser: Browser::Servo,
                visibility: TestVisibility::Public
            },
            path: Utf8Path::new("webgpu/cts.https.html").into(),
            variant: Some("?stuff=things".into()),
        }
    );
}

#[test]
fn report_meta_match() {
    macro_rules! assert_test_matches_meta {
        ($browser:expr, $test_run_path:expr, $rel_meta_path:expr, $test_section_header:expr) => {
            assert_eq!(
                TestPath::from_execution_report($browser, $test_run_path).unwrap(),
                TestPath::from_metadata_test(
                    $browser,
                    Path::new($rel_meta_path),
                    $test_section_header
                )
                .unwrap()
            );
            assert_eq!(
                format!(
                    "/{}",
                    TestPath::from_execution_report($browser, $test_run_path)
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
                TestPath::from_execution_report($browser, $test_run_path).unwrap(),
                TestPath::from_metadata_test(
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
        TestPath::from_metadata_test(
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
        TestPath::from_metadata_test(
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
        TestPath::from_metadata_test(
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
        TestPath::from_metadata_test(
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
        TestPath::from_metadata_test(
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
