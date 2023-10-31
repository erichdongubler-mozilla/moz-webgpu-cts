pub mod metadata;

pub mod reexport {
    pub use chumsky;
    #[cfg(feature = "unstructured")]
    pub use indexmap;
}
