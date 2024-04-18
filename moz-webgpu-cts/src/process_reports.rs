use std::collections::BTreeMap;

use enumset::EnumSetType;

use crate::{
    metadata::{BuildProfile, Platform, SubtestOutcome, TestOutcome, TestProps},
    shared::Expected,
};

#[derive(Debug, Default)]
pub(crate) struct Entry<Out>
where
    Out: EnumSetType,
{
    pub meta_props: Option<TestProps<Out>>,
    pub reported: BTreeMap<Platform, BTreeMap<BuildProfile, Expected<Out>>>,
}

#[derive(Debug, Default)]
pub(crate) struct TestEntry {
    pub entry: Entry<TestOutcome>,
    pub subtests: BTreeMap<String, Entry<SubtestOutcome>>,
}
