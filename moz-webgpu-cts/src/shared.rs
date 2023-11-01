use std::{
    collections::BTreeMap,
    fmt::{self, Display, Formatter},
    num::NonZeroUsize,
    ops::{BitOr, BitOrAssign},
};

use enumset::{EnumSet, EnumSetType};
use strum::IntoEnumIterator;

use crate::metadata::{BuildProfile, Platform};

/// A non-empty set of expected outcomes in a [`Test`] or [`Subtest`].
///
/// The default test expectation is a "good" outcome, where testing passes. The `Out` type
/// parameter should return this value in its implementation of `Default`.
///
/// [`Test`]: crate::metadata::Test
/// [`Subtest`]: crate::metadata::Subtest
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Expectation<Out>(EnumSet<Out>)
where
    Out: EnumSetType;

impl<Out> Default for Expectation<Out>
where
    Out: Default + EnumSetType,
{
    fn default() -> Self {
        Self::permanent(Out::default())
    }
}

impl<Out> Expectation<Out>
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

    fn inner(&self) -> &EnumSet<Out> {
        let Self(inner) = self;
        inner
    }

    pub fn len(&self) -> NonZeroUsize {
        self.inner()
            .len()
            .try_into()
            .expect("invariant violation: empty `Expectation`")
    }

    pub fn is_permanent(&self) -> bool {
        self.len().get() == 1
    }

    pub fn as_permanent(&self) -> Option<Out> {
        self.is_permanent().then(|| self.iter().next().unwrap())
    }

    pub fn into_iter(self) -> impl Iterator<Item = Out> {
        self.inner().into_iter()
    }

    pub fn iter(&self) -> impl Iterator<Item = Out> {
        self.inner().iter()
    }
}

impl<Out> Display for Expectation<Out>
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

impl<Out> BitOr for Expectation<Out>
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

impl<Out> BitOrAssign for Expectation<Out>
where
    Out: EnumSetType,
{
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl<Out> BitOr<Out> for Expectation<Out>
where
    Out: EnumSetType,
{
    type Output = Self;

    fn bitor(self, rhs: Out) -> Self::Output {
        let Self(lhs) = self;

        Self(lhs | rhs)
    }
}

impl<Out> BitOrAssign<Out> for Expectation<Out>
where
    Out: EnumSetType,
{
    fn bitor_assign(&mut self, rhs: Out) {
        *self = *self | rhs;
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

/// A normalized representation of [`Expectation`]s in [`AnalyzeableProps`], which collapses
/// backwards along the following branching factors:
///
/// * [`Platform`]
/// * [`BuildProfile`]
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

pub type NormalizedExpectationByBuildProfile<Out> =
    MaybeCollapsed<Expectation<Out>, BTreeMap<BuildProfile, Expectation<Out>>>;

/// Data from a [`NormalizedExpectationPropertyValue`].
pub type NormalizedExpectationPropertyValueData<Out> = MaybeCollapsed<
    NormalizedExpectationByBuildProfile<Out>,
    BTreeMap<Platform, NormalizedExpectationByBuildProfile<Out>>,
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

    pub(crate) fn from_fully_expanded(
        outcomes: BTreeMap<Platform, BTreeMap<BuildProfile, Expectation<Out>>>,
    ) -> Option<Self>
    where
        Out: Default,
    {
        if outcomes.is_empty() {
            return None;
        }

        fn normalize<K, V, T, F>(
            mut map: BTreeMap<K, V>,
            mut f: F,
        ) -> MaybeCollapsed<T, BTreeMap<K, T>>
        where
            F: FnMut(V) -> T,
            K: IntoEnumIterator + Ord,
            V: Default,
            T: Clone + Default + Eq + PartialEq,
        {
            fn skip_default<K, V, I>(iter: I) -> impl Iterator<Item = (K, V)>
            where
                I: IntoIterator<Item = (K, V)>,
                V: Default + Eq + PartialEq,
            {
                iter.into_iter().filter(|(_k, v)| v != &Default::default())
            }

            let mut iter = K::iter().map(|k| {
                let v = map.remove(&k).unwrap_or_default();
                (k, f(v))
            });

            let (first_key, first_t) = iter.next().unwrap();

            let mut inconsistency_found = false;
            let mut expanded = BTreeMap::default();
            for (k, t) in iter.by_ref() {
                if t == first_t {
                    expanded.extend(skip_default([(k, t)]));
                } else {
                    inconsistency_found = true;
                    expanded.extend(skip_default([(k, t)].into_iter().chain(iter)));
                    break;
                }
            }
            if inconsistency_found {
                expanded.extend(skip_default([(first_key, first_t)]));
                MaybeCollapsed::Expanded(expanded)
            } else {
                MaybeCollapsed::Collapsed(first_t)
            }
        }

        Some(NormalizedExpectationPropertyValue(normalize(
            outcomes,
            |by_build_profile| normalize(by_build_profile, std::convert::identity),
        )))
    }
}
