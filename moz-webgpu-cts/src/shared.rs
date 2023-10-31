use std::collections::BTreeMap;

use enumset::{EnumSet, EnumSetType};

use crate::metadata::{BuildProfile, Platform};

/// A set of expected outcomes in a [`Test`] or [`Subtest`].
///
/// [`Test`]: crate::metadata::Test
/// [`Subtest`]: crate::metadata::Subtest
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expectation<Out>
where
    Out: EnumSetType,
{
    Permanent(Out),
    Intermittent(EnumSet<Out>),
}

impl<Out> Default for Expectation<Out>
where
    Out: Default + EnumSetType,
{
    fn default() -> Self {
        Self::Permanent(Default::default())
    }
}

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

/// A normalized representation of [`Expectation`]s in [`AnalyzeableProps`].
///
/// Yes, the type is _gnarly_. Sorry about that. This is some complex domain, okay? 😆😭
///
/// [`AnalyzeableProps`]: crate::metadata::AnalyzeableProps
#[derive(Clone, Debug)]
pub struct NormalizedExpectationPropertyValue<Out>(
    pub(crate) NormalizedExpectationPropertyValueData<Out>,
)
where
    Out: EnumSetType;

/// Data from a [`NormalizedExpectationPropertyValue`].
pub type NormalizedExpectationPropertyValueData<Out> = MaybeCollapsed<
    MaybeCollapsed<Expectation<Out>, BTreeMap<BuildProfile, Expectation<Out>>>,
    BTreeMap<Platform, MaybeCollapsed<Expectation<Out>, BTreeMap<BuildProfile, Expectation<Out>>>>,
>;

impl<Out> NormalizedExpectationPropertyValue<Out>
where
    Out: EnumSetType,
{
    pub fn inner(&self) -> &NormalizedExpectationPropertyValueData<Out> {
        let Self(inner) = self;
        inner
    }

    pub fn into_inner(self) -> NormalizedExpectationPropertyValueData<Out> {
        let Self(inner) = self;
        inner
    }
}
