pub mod metadata {
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
        pub name: String,
        pub properties: IndexMap<&'a str, PropertyValue<'a>>,
        pub subtests: IndexMap<String, IndexMap<&'a str, PropertyValue<'a>>>,
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
            .labelled("comment")
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
                name: String,
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
                .labelled("subtest section header")
                .then(
                    property(2)
                        .labelled("subtest property")
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(subtest_name, properties)| Item::Subtest {
                    name: subtest_name,
                    properties: properties.into_iter().collect(),
                })
                .labelled("subtest"),
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
                            emitter.emit(Rich::custom(_span, format!("duplicate {key} property")))
                        }
                    }
                    Item::Subtest { name, properties } => {
                        if subtests.contains_key(&name) {
                            // TODO: use old and new item span, better msg.
                            emitter.emit(Rich::custom(_span, format!("duplicate {name} subtest")))
                        }
                        subtests.insert(name, properties);
                    }
                    Item::Newline | Item::Comment => (),
                }
            }
            (properties, subtests)
        });

        let test_header = section_name(0)
            .then_ignore(newline())
            .labelled("test section header");

        test_header
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
        let level_as_space_count = usize::from(level) * 2;
        just(' ')
            .repeated()
            .exactly(level_as_space_count)
            .then_ignore(just(" ").not())
            .labelled("indentation at the proper level")
    }

    fn section_name<'a>(indentation: u8) -> impl Parser<'a, &'a str, String, ParseError<'a>> {
        let name = custom::<_, &str, _, _>(|input| {
            let mut escaped_name = String::new();
            loop {
                match input.peek() {
                    None => {
                        let start = input.offset();
                        input.skip();
                        let span = input.span_since(start);
                        return Err(Rich::custom(
                            span,
                            "reached end of input before ending section header",
                        ));
                    }
                    Some(']') => break,
                    Some('\\') => {
                        let c = input.parse(choice((just("\\]").to(']'), just("\\\"").to('"'))))?;
                        escaped_name.push(c);
                    }
                    Some(other) => {
                        escaped_name.push(other);
                        input.skip();
                    }
                }
            }
            Ok(escaped_name)
        })
        .validate(|escaped_name, span, emitter| {
            for (idx, c) in escaped_name.char_indices() {
                if c.is_control() {
                    let span_idx = span.start.checked_add(idx).unwrap();
                    emitter.emit(Rich::custom(
                        SimpleSpan::new(span_idx, span_idx),
                        "found illegal character in section header",
                    ));
                }
            }
            escaped_name
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
                    "asdf]blarg",
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
