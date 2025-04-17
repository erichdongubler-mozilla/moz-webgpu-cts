pub mod metadata;

pub mod reexport {
    pub use chumsky;
    #[cfg(feature = "unstructured-properties")]
    pub use indexmap;
}

// TODO: Remove this once `chumsky` 0.11 releases:
pub fn keyword<'a, I, E>(keyword: I::Slice) -> impl chumsky::Parser<'a, I, I::Slice, E> + Clone
where
    I: chumsky::input::Input<'a, Token = char>
        + chumsky::input::SliceInput<'a>
        + chumsky::input::StrInput<'a>,
    I::Token: PartialEq,
    I::Slice: PartialEq + PartialEq<&'a str>,
    E: chumsky::extra::ParserExtra<'a, I>,
    I::Slice: chumsky::container::OrderedSeq<'a, I::Token> + Clone,
{
    use chumsky::prelude::*;

    just(keyword).then_ignore(
        any()
            .filter(|c: &char| c.is_alphanumeric() || *c == '_')
            .rewind(),
    )
}
