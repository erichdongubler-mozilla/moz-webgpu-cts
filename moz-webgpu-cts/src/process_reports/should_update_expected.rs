use std::fmt::Debug;

use enumset::EnumSet;

use crate::wpt::metadata::{
    properties::{Expected, NonNormalizedPropertyValue},
    BuildProfile, ImplementationStatus, Platform, SubtestOutcome, TestOutcome, TestProps,
};

pub(crate) trait ShouldUpdateExpected: Debug {
    fn test(
        &mut self,
        meta_props: &TestProps<TestOutcome>,
        reported: &NonNormalizedPropertyValue<Expected<TestOutcome>>,
        key: (Platform, BuildProfile),
    ) -> bool;
    fn subtest(
        &mut self,
        meta_props: &TestProps<SubtestOutcome>,
        reported: &NonNormalizedPropertyValue<Expected<SubtestOutcome>>,
        parent_meta_props: &TestProps<TestOutcome>,
        key: (Platform, BuildProfile),
    ) -> bool;
}

#[derive(Debug)]
pub(crate) struct ImplementationStatusFilter {
    pub allowed: EnumSet<ImplementationStatus>,
}

impl ImplementationStatusFilter {
    fn is_allowed(&self, implementation_status: ImplementationStatus) -> bool {
        let Self { allowed } = self;
        allowed.contains(implementation_status)
    }
}

impl ShouldUpdateExpected for ImplementationStatusFilter {
    fn test(
        &mut self,
        meta_props: &TestProps<TestOutcome>,
        _reported: &NonNormalizedPropertyValue<Expected<TestOutcome>>,
        key: (Platform, BuildProfile),
    ) -> bool {
        let status = meta_props.implementation_status.unwrap_or_default()[key];
        self.is_allowed(status)
    }

    fn subtest(
        &mut self,
        meta_props: &TestProps<SubtestOutcome>,
        _reported: &NonNormalizedPropertyValue<Expected<SubtestOutcome>>,
        parent_meta_props: &TestProps<TestOutcome>,
        key: (Platform, BuildProfile),
    ) -> bool {
        let status = meta_props
            .implementation_status
            .or(parent_meta_props.implementation_status)
            .unwrap_or_default()[key];
        self.is_allowed(status)
    }
}

#[derive(Debug)]
pub(crate) struct NeverUpdateExpected;

impl ShouldUpdateExpected for NeverUpdateExpected {
    fn test(
        &mut self,
        _meta_props: &TestProps<TestOutcome>,
        _reported: &NonNormalizedPropertyValue<Expected<TestOutcome>>,
        _key: (Platform, BuildProfile),
    ) -> bool {
        false
    }

    fn subtest(
        &mut self,
        _meta_props: &TestProps<SubtestOutcome>,
        _reported: &NonNormalizedPropertyValue<Expected<SubtestOutcome>>,
        _parent_meta_props: &TestProps<TestOutcome>,
        _key: (Platform, BuildProfile),
    ) -> bool {
        false
    }
}
