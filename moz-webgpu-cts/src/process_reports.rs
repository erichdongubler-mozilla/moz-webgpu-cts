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
