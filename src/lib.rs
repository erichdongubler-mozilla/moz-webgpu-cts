pub(crate) mod wpt {
    pub(crate) mod expectations {
        use chumsky::{
            extra::Full,
            prelude::Rich,
            primitive::{any, choice, custom, end, just},
            text::newline,
            IterParser, Parser,
        };

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
                    contents: ""
                }])
            );
            assert_eq!(
                test_exps().parse("[blarg]\n").into_result(),
                Ok(vec![TestExp {
                    name: "blarg",
                    contents: ""
                }])
            );
            assert!(test_exps().parse("[blarg]\n[stuff]").into_result().is_err()); // missing newline
            assert_eq!(
                test_exps().parse("\n[blarg]\n[stuff]\n").into_result(),
                Ok(vec![
                    TestExp {
                        name: "blarg",
                        contents: ""
                    },
                    TestExp {
                        name: "stuff",
                        contents: ""
                    }
                ])
            );
            assert_eq!(
                test_exps().parse("\n[blarg]\n\n[stuff]\n").into_result(),
                Ok(vec![
                    TestExp {
                        name: "blarg",
                        contents: "\n"
                    },
                    TestExp {
                        name: "stuff",
                        contents: ""
                    }
                ])
            );
            assert_eq!(
                test_exps()
                    .parse("\n[blarg]\n  expected: PASS\n[stuff]\n")
                    .into_result(),
                Ok(vec![
                    TestExp {
                        name: "blarg",
                        contents: "  expected: PASS\n"
                    },
                    TestExp {
                        name: "stuff",
                        contents: ""
                    }
                ])
            );
        }

        #[derive(Debug, Eq, PartialEq)]
        pub struct TestExp<'a> {
            pub name: &'a str,
            pub contents: &'a str,
        }

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
            let contents = choice((
                just("  ")
                    .ignore_then(any().and_is(newline().not()).repeated())
                    .then_ignore(choice((newline(), end())))
                    .ignored(),
                newline(),
            ))
            .repeated()
            .slice();

            section_name()
                .then_ignore(newline())
                .then(contents)
                .map(|(name, contents)| TestExp { name, contents })
        }

        #[test]
        fn smoke_test_exp() {
            assert_eq!(
                test_exp().parse("[stuff and things]\n").into_result(),
                Ok(TestExp {
                    name: "stuff and things",
                    contents: "",
                })
            );
            assert_eq!(
                test_exp()
                    .parse("[stuff and things]\n  expected: PASS\n")
                    .into_result(),
                Ok(TestExp {
                    name: "stuff and things",
                    contents: "  expected: PASS\n",
                })
            );
        }

        fn section_name<'a>() -> impl Parser<'a, &'a str, &'a str, ParseError<'a>> {
            let name = custom::<_, &str, _, _>(|input| {
                let start_offset = input.offset();
                loop {
                    match input.peek() {
                        Some(c) => match c {
                            ']' => break,
                            // TODO: escapes
                            '\\' => input.parse(just("\\]").ignored())?,
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
            name.delimited_by(just('['), just(']'))
        }

        #[test]
        fn smoke_section_name() {
            assert!(section_name().parse("hoot").into_result().is_err());
            assert_eq!(section_name().parse("[hoot]").into_result(), Ok("hoot"));
            assert_eq!(
                section_name().parse("[asdf\\]blarg]").into_result(),
                Ok("asdf\\]blarg")
            );
            assert!(section_name().parse("[asdf]blarg]").into_result().is_err());
        }
    }
}
