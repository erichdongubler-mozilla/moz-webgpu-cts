use std::fmt::Debug;

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
