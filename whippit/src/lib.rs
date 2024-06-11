pub mod metadata;

pub mod reexport {
    pub use chumsky;
    #[cfg(feature = "unstructured-properties")]
    pub use indexmap;
}
