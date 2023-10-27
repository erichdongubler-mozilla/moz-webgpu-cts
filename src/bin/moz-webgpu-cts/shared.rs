use std::{collections::BTreeMap};

use crate::metadata::{BuildProfile, Platform};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expectation<Out> {
    Permanent(Out),
    Intermittent(Vec<Out>),
}

impl<Out> Default for Expectation<Out>
where
    Out: Default,
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

/// A normalized representation of [`Expectation`]s in [`AnalyzeableProps`].
///
/// Yes, the type is _gnarly_. Sorry about that. This is some complex domain, okay? 😆😭
///
/// [`AnalyzeableProps`]: crate::metadata::AnalyzeableProps
#[derive(Clone, Debug)]
pub struct NormalizedExpectationPropertyValue<Out>(
    pub(crate)  MaybeCollapsed<
        MaybeCollapsed<Expectation<Out>, BTreeMap<BuildProfile, Expectation<Out>>>,
        BTreeMap<
            Platform,
            MaybeCollapsed<Expectation<Out>, BTreeMap<BuildProfile, Expectation<Out>>>,
        >,
    >,
);

/// Data from a [`NormalizedExpectationPropertyValue`].
pub type NormalizedExpectationPropertyValueData<Out> = MaybeCollapsed<
    MaybeCollapsed<Expectation<Out>, BTreeMap<BuildProfile, Expectation<Out>>>,
    BTreeMap<Platform, MaybeCollapsed<Expectation<Out>, BTreeMap<BuildProfile, Expectation<Out>>>>,
>;

impl<Out> NormalizedExpectationPropertyValue<Out> {
    pub fn into_inner(self) -> NormalizedExpectationPropertyValueData<Out> {
        let Self(inner) = self;
        inner
    }
}
