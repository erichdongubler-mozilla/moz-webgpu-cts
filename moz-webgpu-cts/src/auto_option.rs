#[derive(Clone, Copy, Debug)]
pub(crate) enum AutoOption<T> {
    Auto,
    Manual(T),
}

impl<T> AutoOption<T> {
    pub fn into_option(self) -> Option<T> {
        match self {
            AutoOption::Auto => None,
            AutoOption::Manual(t) => Some(t),
        }
    }
}

impl<T> From<AutoOption<T>> for Option<T> {
    fn from(value: AutoOption<T>) -> Self {
        value.into_option()
    }
}

#[macro_export]
macro_rules! auto_option_for {
    ($ident:ident) => {
        static VARIANTS: ::std::sync::OnceLock<Vec<$crate::auto_option::AutoOption<$ident>>> =
            ::std::sync::OnceLock::new();

        impl ::clap::ValueEnum for $crate::auto_option::AutoOption<$ident>
        where
            $ident: ::clap::ValueEnum + Clone,
        {
            fn value_variants<'a>() -> &'a [Self] {
                let variants = VARIANTS.get_or_init(|| {
                    let manual_variants = $ident::value_variants();
                    let mut variants = Vec::with_capacity(manual_variants.len() + 1);
                    variants.push(Self::Auto);
                    variants.extend(manual_variants.into_iter().cloned().map(Self::Manual));
                    variants
                });
                variants.as_slice()
            }

            fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
                todo!()
            }
        }
    };
}

pub(crate) use auto_option_for;
