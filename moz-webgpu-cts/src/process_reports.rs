use std::collections::BTreeMap;

use enumset::EnumSetType;

use crate::{
    metadata::{BuildProfile, Platform, SubtestOutcome, TestOutcome},
    shared::{Expectation, NormalizedExpectationPropertyValue},
};

#[derive(Debug)]
pub(crate) struct OutcomesForComparison<Out>
where
    Out: EnumSetType,
{
    pub metadata: Option<MaybeDisabled<Option<NormalizedExpectationPropertyValue<Out>>>>,
    pub reported: BTreeMap<Platform, BTreeMap<BuildProfile, Expectation<Out>>>,
}

impl<Out> OutcomesForComparison<Out>
where
    Out: Default + EnumSetType,
{
    pub(crate) fn into_normalized(
        self,
    ) -> (
        MaybeDisabled<NormalizedExpectationPropertyValue<Out>>,
        NormalizedExpectationPropertyValue<Out>,
    ) {
        let Self { metadata, reported } = self;

        let metadata = metadata
            .map(|maybe_disabled| maybe_disabled.map_enabled(|opt| opt.unwrap_or_default()))
            .unwrap_or_default();

        let reported = NormalizedExpectationPropertyValue::from_fully_expanded(reported);

        (metadata, reported)
    }
}

impl<Out> Default for OutcomesForComparison<Out>
where
    Out: EnumSetType,
{
    fn default() -> Self {
        Self {
            metadata: None,
            reported: Default::default(),
        }
    }
}

#[derive(Debug)]
pub(crate) enum MaybeDisabled<T> {
    Disabled,
    Enabled(T),
}

impl<T> Default for MaybeDisabled<T>
where
    T: Default,
{
    fn default() -> Self {
        Self::Enabled(Default::default())
    }
}

impl<T> MaybeDisabled<T> {
    pub fn map_enabled<U, F>(self, f: F) -> MaybeDisabled<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Disabled => MaybeDisabled::Disabled,
            Self::Enabled(t) => MaybeDisabled::Enabled(f(t)),
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct TestOutcomes {
    pub test_outcomes: OutcomesForComparison<TestOutcome>,
    pub subtests: BTreeMap<String, OutcomesForComparison<SubtestOutcome>>,
}
