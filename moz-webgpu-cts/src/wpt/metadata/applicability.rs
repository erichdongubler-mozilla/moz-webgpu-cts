//! `moz-webgpu-cts`' representation of WPT conditionals that it can handle. The focus of this API
//! is the [`Applicability`] data structure and its components.
//!
//! This is essentially the set of environments that Firefox's WebGPU team uses in CI for WebGPU.
//! When that changes, this changes accordingly.

use enum_map::Enum;
use exhaust::Exhaust;
use serde::Serialize;
use whippit::reexport::chumsky::{error::Rich, input::Emitter, span::SimpleSpan};

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
    MacOsIntel,
    MacOsArm,
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

impl Environment {
    fn os(self) -> Os {
        match self {
            Self::Windows => Os::Windows,
            Self::Linux => Os::Linux,
            Self::MacOsIntel => Os::MacOs,
            Self::MacOsArm => Os::MacOs,
        }
    }

    fn processor(self) -> Option<Processor> {
        match self {
            Self::Windows | Self::Linux => None,
            Self::MacOsIntel => Some(Processor::X86_64),
            Self::MacOsArm => Some(Processor::Arm),
        }
    }

    fn from_mac_os(processor: Processor) -> Self {
        match processor {
            Processor::X86_64 => Self::MacOsIntel,
            Processor::Arm => Self::MacOsArm,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Processor {
    X86_64,
    Arm,
}

impl Processor {
    pub fn from_ident(s: &str) -> Option<Self> {
        match s {
            "x86_64" => Some(Self::X86_64),
            "aarch64" => Some(Self::Arm),
            _ => None,
        }
    }

    pub fn to_ident(&self) -> &'static str {
        match self {
            Self::X86_64 => "x86_64",
            Self::Arm => "aarch64",
        }
    }
}

// TODO: rename `Environment` to `Environment`
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Os {
    Linux,
    Windows,
    MacOs,
}

impl Os {
    pub fn from_ident(s: &str) -> Option<Self> {
        match s {
            "linux" => Some(Self::Linux),
            "win" => Some(Self::Windows),
            "mac" => Some(Self::MacOs),
            _ => None,
        }
    }

    pub fn to_ident(&self) -> &'static str {
        match self {
            Self::Linux => "linux",
            Self::Windows => "win",
            Self::MacOs => "mac",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ApplicabilityRule {
    Os(Os),
    Processor(Processor),
    Debug { inverted: bool },
}

enum IncompletePlatform {
    UnknownProcessor {
        span: SimpleSpan,
        processor: Processor,
    },
    MacOs {
        span: SimpleSpan,
    },
}

enum EnvironmentMatcher {
    Finalized {
        environment: Environment,
        os_span: SimpleSpan,
        processor_span: Option<SimpleSpan>,
    },
    Incomplete(IncompletePlatform),
}

enum ApplicabilityParseError {
    ProcessorFoundAfterIndependentOs {
        os: Os,
        os_span: SimpleSpan,
        processor: Processor,
        processor_span: SimpleSpan,
    },
}

struct ApplicabilityBuilder {
    environment: Option<EnvironmentMatcher>,
    build_profile: Option<(SimpleSpan, BuildProfile)>,
}

impl ApplicabilityBuilder {
    pub fn handle_rule<'a>(
        &mut self,
        rule: ApplicabilityRule,
        span: SimpleSpan,
        emitter: &mut Emitter<Rich<'_, char>>,
    ) -> Result<(), ()> {
        let Self {
            environment: platform,
            build_profile,
        } = &*self;
        let already_got_os_err = |prev_span, span| todo!();
        let processor_with_independent_os_err =
            |os: Os, os_span, processor: Processor, processor_span| {
                todo!(
                    "`processor` comparison is not allowed when `os == {:?}`",
                    os.to_ident()
                )
            };
        let check_for_dupe_processors = |first, first_span, second, second_span| {
            if first == second {
                todo!("emit warning")
            } else {
                todo!("contradictory `processor` statements")
            }
        };
        match rule {
            ApplicabilityRule::Os(name) => {
                self.environment = Some(match (platform, name) {
                    (
                        Some(EnvironmentMatcher::Finalized {
                            os_span: prev_span, ..
                        }),
                        _,
                    )
                    | (
                        Some(EnvironmentMatcher::Incomplete(IncompletePlatform::MacOs {
                            span: prev_span,
                        })),
                        _,
                    ) => return already_got_os_err(prev_span, span),
                    (None, platform @ (Os::Linux | Os::Windows)) => EnvironmentMatcher::Finalized {
                        environment: match platform {
                            Os::Linux => Environment::Linux,
                            Os::Windows => Environment::Windows,
                            Os::MacOs => unreachable!(),
                        },
                        os_span: span,
                        processor_span: None,
                    },
                    (None, Os::MacOs) => todo!(),
                    (
                        Some(EnvironmentMatcher::Incomplete(
                            IncompletePlatform::UnknownProcessor {
                                span: processor_span,
                                processor,
                            },
                        )),
                        Os::MacOs,
                    ) => EnvironmentMatcher::Finalized {
                        environment: match processor {
                            Processor::X86_64 => Environment::MacOsIntel,
                            Processor::Arm => Environment::MacOsArm,
                        },
                        os_span: span,
                        processor_span: Some(*processor_span),
                    },
                    (
                        Some(EnvironmentMatcher::Incomplete(
                            IncompletePlatform::UnknownProcessor {
                                span: processor_span,
                                processor,
                            },
                        )),
                        os @ (Os::Linux | Os::Windows),
                    ) => {
                        return processor_with_independent_os_err(
                            os,
                            span,
                            *processor,
                            *processor_span,
                        )
                    }
                })
            }
            ApplicabilityRule::Processor(processor) => {
                self.environment = Some(match platform {
                    Some(EnvironmentMatcher::Finalized {
                        environment: platform,
                        os_span,
                        processor_span,
                    }) => {
                        if let Some(processor_span) = processor_span {
                            check_for_dupe_processors(
                                platform.processor().unwrap(),
                                *processor_span,
                                processor,
                                span,
                            )?;
                            return Ok(());
                        } else {
                            return processor_with_independent_os_err(
                                platform.os(),
                                *os_span,
                                processor,
                                span,
                            );
                        }
                    }
                    Some(EnvironmentMatcher::Incomplete(IncompletePlatform::MacOs {
                        span: os_span,
                    })) => EnvironmentMatcher::Finalized {
                        environment: Environment::from_mac_os(processor),
                        os_span: *os_span,
                        processor_span: Some(span),
                    },
                    Some(EnvironmentMatcher::Incomplete(
                        IncompletePlatform::UnknownProcessor {
                            span: prev_span,
                            processor: prev_processor,
                        },
                    )) => {
                        check_for_dupe_processors(*prev_processor, *prev_span, processor, span)?;
                        return Ok(());
                    }
                    None => todo!(),
                })
            }
            ApplicabilityRule::Debug { inverted } => todo!(),
        }
        return Ok(());
    }
    pub fn into_applicability(self) -> Result<Applicability, ()> {}
}
