//! `moz-webgpu-cts`' representation of WPT conditionals that it can handle. The focus of this API
//! is the [`Applicability`] data structure and its components.
//!
//! This is essentially the set of environments that Firefox's WebGPU team uses in CI for WebGPU.
//! When that changes, this changes accordingly.

use enum_map::Enum;
use exhaust::Exhaust;
use serde::Serialize;

/// The strict subset of WPT property conditionals that can be handled with `moz-webgpu-cts`.
///
/// The entire set of possible values here should be small enough that, when it is a key like in
/// [`ExpandedPropertyValue`], significant performance gains are possible.
#[derive(Clone, Debug, Default)]
pub struct Applicability {
    pub environment: Option<Environment>,
    pub build_profile: Option<BuildProfile>,
}

/// The subset of WPT run environments can be handled by `moz-webgpu-cts`. Part of
/// [`Applicability`].
///
/// This is essentially the set of OSes, software versions, and hardware that Firefox's WebGPU team
/// uses in CI for WebGPU. When that changes, this changes accordingly.
#[derive(Clone, Copy, Debug, Enum, Exhaust, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Environment {
    Windows,
    Linux,
    MacOs,
}

/// The subset of browser build profiles can be handled by `moz-webgpu-cts`. Part of
/// [`Applicability`].
///
/// This is essentially the set of profiles that Firefox's WebGPU team uses in CI for WebGPU. When
/// that changes, this changes accordingly.
#[derive(Clone, Copy, Debug, Enum, Exhaust, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum BuildProfile {
    Debug,
    Optimized,
}
