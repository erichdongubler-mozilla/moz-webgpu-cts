pub(crate) mod wpt {
    pub(crate) mod metadata {
        #[cfg(test)]
        use insta::assert_debug_snapshot;

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

        #[derive(Clone, Debug)]
        pub struct File<'a> {
            pub tests: Vec<Test<'a>>,
        }

        impl<'a> File<'a> {
            pub fn parser() -> impl Parser<'a, &'a str, File<'a>, ParseError<'a>> {
                filler()
                    .ignore_then(test())
                    .then_ignore(filler())
                    .repeated()
                    .collect()
                    .map(|tests| File { tests })
            }
        }

        fn filler<'a>() -> impl Parser<'a, &'a str, (), ParseError<'a>> {
            choice((comment().ignored(), newline())).repeated()
        }

        #[test]
        fn smoke_parser() {
            assert_debug_snapshot!(
                File::parser().parse(""),
                @r###"
            ParseResult {
                output: Some(
                    File {
                        tests: [],
                    },
                ),
                errs: [],
            }
            "###
            );
            assert_debug_snapshot!(File::parser().parse("[hoot]"), @r###"
            ParseResult {
                output: None,
                errs: [
                    found end of input at 6..6 expected ''\r'', or ''\n'',
                ],
            }
            "###);
            assert_debug_snapshot!(File::parser().parse("[blarg]\n"), @r###"
            ParseResult {
                output: Some(
                    File {
                        tests: [
                            Test {
                                name: "blarg",
                                properties: {},
                                subtests: {},
                                span: 0..8,
                            },
                        ],
                    },
                ),
                errs: [],
            }
            "###);
            assert_debug_snapshot!(File::parser().parse("[blarg]\n[stuff]"), @r###"
            ParseResult {
                output: None,
                errs: [
                    found end of input at 15..15 expected ''\r'', or ''\n'',
                ],
            }
            "###);
            assert_debug_snapshot!(File::parser().parse("\n[blarg]\n[stuff]\n"), @r###"
            ParseResult {
                output: Some(
                    File {
                        tests: [
                            Test {
                                name: "blarg",
                                properties: {},
                                subtests: {},
                                span: 1..9,
                            },
                            Test {
                                name: "stuff",
                                properties: {},
                                subtests: {},
                                span: 9..17,
                            },
                        ],
                    },
                ),
                errs: [],
            }
            "###);
            assert_debug_snapshot!(File::parser().parse("\n[blarg]\n\n[stuff]\n"), @r###"
            ParseResult {
                output: Some(
                    File {
                        tests: [
                            Test {
                                name: "blarg",
                                properties: {},
                                subtests: {},
                                span: 1..10,
                            },
                            Test {
                                name: "stuff",
                                properties: {},
                                subtests: {},
                                span: 10..18,
                            },
                        ],
                    },
                ),
                errs: [],
            }
            "###);
            assert_debug_snapshot!(File::parser().parse("\n[blarg]\n  expected: PASS\n[stuff]\n"), @r###"
            ParseResult {
                output: Some(
                    File {
                        tests: [
                            Test {
                                name: "blarg",
                                properties: {
                                    "expected": Unconditional(
                                        "PASS",
                                    ),
                                },
                                subtests: {},
                                span: 1..26,
                            },
                            Test {
                                name: "stuff",
                                properties: {},
                                subtests: {},
                                span: 26..34,
                            },
                        ],
                    },
                ),
                errs: [],
            }
            "###);
        }

        #[derive(Clone, Debug, Eq, PartialEq)]
        pub struct Test<'a> {
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
            assert_debug_snapshot!(comment().parse("asdf"), @r###"
            ParseResult {
                output: None,
                errs: [
                    found ''a'' at 0..1 expected ''#'',
                ],
            }
            "###);
            assert_debug_snapshot!(comment().parse("# asdf"), @r###"
            ParseResult {
                output: Some(
                    "asdf",
                ),
                errs: [],
            }
            "###);
            assert_debug_snapshot!(comment().parse("# "), @r###"
            ParseResult {
                output: Some(
                    "",
                ),
                errs: [],
            }
            "###);
            assert_debug_snapshot!(comment().parse("#"), @r###"
            ParseResult {
                output: Some(
                    "",
                ),
                errs: [],
            }
            "###);
            assert_debug_snapshot!(
                comment().parse("# asdf # blarg"),
                @r###"
            ParseResult {
                output: Some(
                    "asdf # blarg",
                ),
                errs: [],
            }
            "###
            );
            assert_debug_snapshot!(comment().parse(" # asdf # blarg"), @r###"
            ParseResult {
                output: None,
                errs: [
                    found '' '' at 0..1 expected ''#'',
                ],
            }
            "###);
        }

        fn test<'a>() -> impl Parser<'a, &'a str, Test<'a>, ParseError<'a>> {
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
                .map_with_span(|(name, (properties, subtests)), span| Test {
                    name,
                    span,
                    properties,
                    subtests,
                })
        }

        #[test]
        fn smoke_test() {
            assert_debug_snapshot!(
                test().parse("[stuff and things]\n"),
                @r###"
            ParseResult {
                output: Some(
                    Test {
                        name: "stuff and things",
                        properties: {},
                        subtests: {},
                        span: 0..19,
                    },
                ),
                errs: [],
            }
            "###
            );
            assert_debug_snapshot!(
                test()
                    .parse("[stuff and things]\n  expected: PASS\n"),
                @r###"
            ParseResult {
                output: Some(
                    Test {
                        name: "stuff and things",
                        properties: {
                            "expected": Unconditional(
                                "PASS",
                            ),
                        },
                        subtests: {},
                        span: 0..36,
                    },
                ),
                errs: [],
            }
            "###
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
            assert_debug_snapshot!(section_name(0).parse("hoot"), @r###"
            ParseResult {
                output: None,
                errs: [
                    found ''h'' at 0..1 expected ''['',
                ],
            }
            "###);
            assert_debug_snapshot!(section_name(0).parse("[hoot]"), @r###"
            ParseResult {
                output: Some(
                    "hoot",
                ),
                errs: [],
            }
            "###);
            assert_debug_snapshot!(section_name(0).parse("[asdf\\]blarg]"), @r###"
            ParseResult {
                output: Some(
                    "asdf\\]blarg",
                ),
                errs: [],
            }
            "###            );
            assert_debug_snapshot!(section_name(0).parse("[asdf]blarg]"), @r###"
            ParseResult {
                output: None,
                errs: [
                    found ''b'' at 6..7 expected something else,
                ],
            }
            "###);
        }
    }
}
