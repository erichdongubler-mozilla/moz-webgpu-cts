use std::{
    borrow::Cow,
    fmt::{self, Display, Formatter},
};

use serde::Serialize;

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DisabledString(Cow<'static, str>);

impl DisabledString {
    const FALSE: &'static str = "@False";

    pub fn new(inner: String) -> Self {
        Self(if inner == Self::FALSE {
            // OPT: It can be expensive to store `Default`-generated copies of this, so let's avoid
            // heap allocations for this case specifically.
            Self::FALSE.into()
        } else {
            inner.into()
        })
    }

    fn value(&self) -> &str {
        self.0.as_ref()
    }

    pub fn is_disabled(&self) -> bool {
        self.value() != Self::FALSE
    }
}

impl Default for DisabledString {
    fn default() -> Self {
        Self::new(Self::FALSE.into())
    }
}

impl Display for DisabledString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.value(), f)
    }
}

#[test]
fn disabled_string() {
    let asdf = DisabledString::new("asdf".into());
    assert_eq!(asdf, DisabledString("asdf".to_string().into()));
    assert_eq!(asdf.value(), "asdf");

    assert_eq!(
        &DisabledString::new("@False".into()).0,
        &DisabledString::FALSE
    );

    assert!(!DisabledString::new(DisabledString::FALSE.into()).is_disabled());
    assert!(DisabledString::new("asdf".into()).is_disabled());
    assert!(
        DisabledString::new("https://bugzilla.mozilla.org/show_bug.cgi?id=1958061".into())
            .is_disabled()
    );
}
