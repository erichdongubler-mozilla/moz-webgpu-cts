pub(crate) mod wpt {
    pub(crate) mod expectations {
        use chumsky::{
            extra::Full,
            prelude::Rich,
            primitive::{any, choice, custom, end, group, just},
            span::SimpleSpan,
            text::{ident, inline_whitespace, keyword, newline},
            IterParser, Parser,
        };
        use indexmap::IndexMap;

        pub type ParseError<'a> = Full<Rich<'a, char>, (), ()>;

        pub fn test_exps<'a>() -> impl Parser<'a, &'a str, Vec<TestExp<'a>>, ParseError<'a>> {
            filler()
                .ignore_then(test_exp())
                .then_ignore(filler())
                .repeated()
                .collect()
        }

        fn filler<'a>() -> impl Parser<'a, &'a str, (), ParseError<'a>> {
            choice((comment().ignored(), newline())).repeated()
        }

        #[test]
        fn smoke_parser() {
            assert_eq!(test_exps().parse("").into_result(), Ok(vec![]));
            assert!(test_exps().parse("[hoot]").into_result().is_err()); // missing newline
            assert_eq!(
                test_exps().parse("[blarg]\n").into_result(),
                Ok(vec![TestExp {
                    name: "blarg",
                    properties: IndexMap::new(),
                    subtests: IndexMap::new(),
                    span: SimpleSpan::new(0, 8),
                }])
            );
            assert_eq!(
                test_exps().parse("[blarg]\n").into_result(),
                Ok(vec![TestExp {
                    name: "blarg",
                    properties: IndexMap::new(),
                    subtests: IndexMap::new(),
                    span: SimpleSpan::new(0, 8),
                }])
            );
            assert!(test_exps().parse("[blarg]\n[stuff]").into_result().is_err()); // missing newline
            assert_eq!(
                test_exps().parse("\n[blarg]\n[stuff]\n").into_result(),
                Ok(vec![
                    TestExp {
                        name: "blarg",
                        properties: IndexMap::new(),
                        subtests: IndexMap::new(),
                        span: SimpleSpan::new(1, 9),
                    },
                    TestExp {
                        name: "stuff",
                        properties: IndexMap::new(),
                        subtests: IndexMap::new(),
                        span: SimpleSpan::new(9, 17),
                    }
                ])
            );
            assert_eq!(
                test_exps().parse("\n[blarg]\n\n[stuff]\n").into_result(),
                Ok(vec![
                    TestExp {
                        name: "blarg",
                        properties: IndexMap::new(),
                        subtests: IndexMap::new(),
                        span: SimpleSpan::new(1, 10),
                    },
                    TestExp {
                        name: "stuff",
                        properties: IndexMap::new(),
                        subtests: IndexMap::new(),
                        span: SimpleSpan::new(10, 18),
                    },
                ])
            );
            assert_eq!(
                test_exps()
                    .parse("\n[blarg]\n  expected: PASS\n[stuff]\n")
                    .into_result(),
                Ok(vec![
                    TestExp {
                        name: "blarg",
                        properties: IndexMap::from_iter([(
                            "expected",
                            PropertyValue::Unconditional("PASS")
                        ),]),
                        subtests: IndexMap::new(),
                        span: SimpleSpan::new(1, 26),
                    },
                    TestExp {
                        name: "stuff",
                        properties: IndexMap::new(),
                        subtests: IndexMap::new(),
                        span: SimpleSpan::new(26, 34),
                    }
                ])
            );
        }

        #[derive(Debug, Eq, PartialEq)]
        pub struct TestExp<'a> {
            pub name: &'a str,
            pub properties: IndexMap<&'a str, PropertyValue<'a>>,
            pub subtests: IndexMap<&'a str, IndexMap<&'a str, PropertyValue<'a>>>, // TODO: use strongly typed subtest name key?
            span: SimpleSpan,
        }

        #[derive(Clone, Debug, Eq, PartialEq)]
        pub enum PropertyValue<'a> {
            Unconditional(&'a str),
            Conditional {
                conditions: Vec<(Condition<'a>, &'a str)>,
                fallback: &'a str,
            },
        }

        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct Condition<'a>(&'a str);

        fn comment<'a>() -> impl Parser<'a, &'a str, &'a str, ParseError<'a>> {
            just('#')
                .ignore_then(just(' ').or_not())
                .ignore_then(any().and_is(newline().not()).repeated().slice())
                .then_ignore(choice((newline(), end())))
        }

        #[test]
        fn smoke_comment() {
            assert!(comment().parse("asdf").into_result().is_err());
            assert_eq!(comment().parse("# asdf").into_result(), Ok("asdf"));
            assert_eq!(comment().parse("# ").into_result(), Ok(""));
            assert_eq!(comment().parse("#").into_result(), Ok(""));
            assert_eq!(
                comment().parse("# asdf # blarg").into_result(),
                Ok("asdf # blarg")
            );
            assert!(comment().parse(" # asdf # blarg").into_result().is_err());
        }

        fn test_exp<'a>() -> impl Parser<'a, &'a str, TestExp<'a>, ParseError<'a>> {
            #[derive(Clone, Debug)]
            enum Item<'a> {
                Subtest {
                    name: &'a str,
                    properties: IndexMap<&'a str, PropertyValue<'a>>,
                },
                Property {
                    key: &'a str,
                    value: PropertyValue<'a>,
                },
                Newline,
                Comment,
            }

            let property = |indentation: u8| {
                let unconditional_value = any()
                    .and_is(newline().not())
                    .repeated()
                    .slice()
                    .then_ignore(newline());
                let conditional_rule = group((
                    indent(
                        indentation
                            .checked_add(1)
                            .expect("unexpectedly high indentation level"),
                    ),
                    keyword("if"),
                    just(' '),
                ))
                .ignore_then(
                    // TODO: actual expression tree
                    any()
                        .and_is(choice((newline(), just(":").to(()))).not())
                        .repeated()
                        .slice()
                        .map(Condition)
                        .then_ignore(group((just(':'), inline_whitespace())))
                        .then(
                            any()
                                .and_is(choice((newline(), just(':').to(()))).not())
                                .repeated()
                                .slice(),
                        ),
                );
                let conditional_fallback = keyword("if")
                    .not()
                    .ignore_then(any().and_is(newline().not()))
                    .slice();

                indent(indentation)
                    .ignore_then(ident())
                    .then_ignore(just(':'))
                    .then_ignore(inline_whitespace())
                    .then(choice((
                        unconditional_value.map(PropertyValue::Unconditional),
                        newline()
                            .ignore_then(conditional_rule.repeated().collect())
                            .then(conditional_fallback)
                            .map(|(conditions, fallback)| PropertyValue::Conditional {
                                conditions,
                                fallback,
                            }),
                    )))
            };
            let items = choice((
                section_name(1)
                    .then_ignore(newline())
                    .then(property(2).repeated().collect::<Vec<_>>())
                    .map(|(subtest_name, properties)| Item::Subtest {
                        name: subtest_name,
                        properties: properties.into_iter().collect(),
                    }),
                property(1).map(|(key, value)| Item::Property { key, value }),
                newline().to(Item::Newline),
                comment().to(Item::Comment),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .validate(|items, _span, emitter| {
                let mut properties = IndexMap::new();
                let mut subtests = IndexMap::new();
                for item in items {
                    match item {
                        Item::Property { key, value } => {
                            if let Some(_old) = properties.insert(key, value) {
                                emitter
                                    .emit(Rich::custom(_span, format!("duplicate {key} property")))
                            }
                        }
                        Item::Subtest { name, properties } => {
                            if let Some(_old) = subtests.insert(name, properties) {
                                emitter
                                    .emit(Rich::custom(_span, format!("duplicate {name} subtest")))
                            }
                        }
                        Item::Newline | Item::Comment => (),
                    }
                }
                (properties, subtests)
            });

            section_name(0)
                .then_ignore(newline())
                .then(items)
                .map_with_span(|(name, (properties, subtests)), span| TestExp {
                    name,
                    span,
                    properties,
                    subtests,
                })
        }

        #[test]
        fn smoke_test_exp() {
            assert_eq!(
                test_exp().parse("[stuff and things]\n").into_result(),
                Ok(TestExp {
                    name: "stuff and things",
                    properties: IndexMap::new(),
                    subtests: IndexMap::new(),
                    span: SimpleSpan::new(0, 19),
                })
            );
            assert_eq!(
                test_exp()
                    .parse("[stuff and things]\n  expected: PASS\n")
                    .into_result(),
                Ok(TestExp {
                    name: "stuff and things",
                    properties: IndexMap::from_iter([(
                        "expected",
                        PropertyValue::Unconditional("PASS")
                    ),]),
                    subtests: IndexMap::new(),
                    span: SimpleSpan::new(0, 36),
                })
            );
        }

        fn indent<'a>(level: u8) -> impl Parser<'a, &'a str, (), ParseError<'a>> {
            just(' ').repeated().exactly(usize::from(level) * 2)
        }

        fn section_name<'a>(indentation: u8) -> impl Parser<'a, &'a str, &'a str, ParseError<'a>> {
            let name = custom::<_, &str, _, _>(|input| {
                let start_offset = input.offset();
                loop {
                    match input.peek() {
                        Some(c) => match c {
                            ']' => break,
                            // TODO: escapes
                            '\\' => input.parse(choice((just("\\]"), just("\\\""))).ignored())?,
                            c if c.is_control() => break,
                            _other => input.skip(),
                        },
                        None => break,
                    }
                }
                let slice = input.slice(start_offset..input.offset());
                Ok(slice)
            })
            .validate(|slice, span, emitter| {
                if slice.is_empty() {
                    emitter.emit(Rich::custom(
                        span,
                        "empty test name found; test names cannot be empty",
                    ));
                }
                slice
            });
            indent(indentation).ignore_then(name.delimited_by(just('['), just(']')))
        }

        #[test]
        fn smoke_section_name() {
            assert!(section_name(0).parse("hoot").into_result().is_err());
            assert_eq!(section_name(0).parse("[hoot]").into_result(), Ok("hoot"));
            assert_eq!(
                section_name(0).parse("[asdf\\]blarg]").into_result(),
                Ok("asdf\\]blarg")
            );
            assert!(section_name(0).parse("[asdf]blarg]").into_result().is_err());
        }
    }
}
