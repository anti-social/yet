use std::error::Error as StdError;
use std::str::FromStr;

use failure::{self, Fail, Compat, format_err};

use combine::{parser as combine_parser, Parser, ParseError, Stream};
use combine::{any, between, choice, eof, optional, one_of, many, many1,
              not_followed_by, satisfy, satisfy_map, sep_by, sep_by1, skip_many,
              skip_many1, token};
use combine::error::{Consumed, StreamError, UnexpectedParse};
use combine::parser::char::{alpha_num, char, digit, space, spaces, string};
use combine::parser::combinator::{attempt, recognize};
use combine::parser::range::take_while1;
use combine::parser::repeat::escaped;
use combine::stream::{Range, RangeStream, RangeStreamOnce, StreamOnce, StreamErrorFor};

#[derive(Debug, PartialEq)]
pub enum TemplatePart {
    Gap(String),
    Subst(SubstExpr),
}

#[derive(Debug, PartialEq)]
pub enum Arg {
    Var(Vec<String>),
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Fail)]
pub enum ParseSubstitutionError {
    #[fail(display = "error when parsing substitution expression")]
    ParseError
}

#[derive(Debug, PartialEq)]
struct OperatorFirstArg(pub Arg);

#[derive(Debug, PartialEq)]
pub enum TestFun {
    Defined,
    Undefined,
    Eq,
    NotEq,
}

impl TestFun {
    pub fn negate(&self) -> TestFun {
        use self::TestFun::*;

        match self {
            Defined => Undefined,
            Undefined => Defined,
            Eq => NotEq,
            NotEq => Eq,
        }
    }
}

impl FromStr for TestFun {
    type Err = failure::Compat<ParseSubstitutionError>;

    fn from_str(name: &str) -> Result<TestFun, Self::Err> {
        use self::TestFun::*;

        Ok(match name {
            "defined" => Defined,
            "undefined" => Undefined,
            "eq" | "equalto" => Eq,
            _ => {
                return Err(ParseSubstitutionError::ParseError.compat());
            },
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum SubstExpr {
    Var(Arg),
    Test { fun: TestFun, args: Vec<Arg> },
}

fn lex<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
    where
        P: Parser,
        P::Input: Stream<Item = char>,
        <P::Input as StreamOnce>::Error: ParseError<
            <P::Input as StreamOnce>::Item,
            <P::Input as StreamOnce>::Range,
            <P::Input as StreamOnce>::Position,
        >,
{
    p.skip(spaces())
}

fn integer<I>() -> impl Parser<Input = I, Output = i64>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex(many1(digit()))
        .map(|s: String| {
            let mut n = 0;
            for c in s.chars() {
                n = n * 10 + (c as i64 - '0' as i64);
            }
            n
        })
        .expected("integer")
}

fn float<I>() -> impl Parser<Input = I, Output = f64>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let i = char('0').map(|_| 0.0).or(integer().map(|x| x as f64));
    let fractional = many(digit()).map(|digits: String| {
        let mut magnitude = 1.0;
        digits.chars().fold(0.0, |acc, d| {
            magnitude /= 10.0;
            match d.to_digit(10) {
                Some(d) => acc + (d as f64) * magnitude,
                None => panic!("Not a digit"),
            }
        })
    });

    let exp = satisfy(|c| c == 'e' || c == 'E').with(optional(char('-')).and(integer()));
    lex(optional(char('-'))
        .and(i)
        .map(|(sign, n)| if sign.is_some() { -n } else { n })
        .and(optional(char('.')).with(fractional))
        .map(|(x, y)| if x >= 0.0 { x + y } else { x - y })
        .and(optional(exp))
        .map(|(n, exp_option)| match exp_option {
            Some((sign, e)) => {
                let e = if sign.is_some() { -e } else { e };
                n * 10.0f64.powi(e as i32)
            }
            None => n,
        }))
        .expected("number")
}

fn boolean<I>() -> impl Parser<Input = I, Output = bool>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        lex(string("true").map(|_| true)),
        lex(string("false").map(|_| false)),
    ))
}

fn string_char<I>() -> impl Parser<Input = I, Output = char>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    combine_parser(|input: &mut I| {
        let mut back_slash_char = satisfy_map(|c| {
            Some(match c {
                '"' => '"',
                '\\' => '\\',
                '/' => '/',
                'b' => '\u{0008}',
                'f' => '\u{000c}',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => return None,
            })
        });

        let char_res: Result<_, _> = any().parse_lazy(input).into();
        let (c, consumed) = char_res?;
        match c {
            '\\' => consumed.combine(|_| back_slash_char.parse_stream(input)),
            '"' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            _ => Ok((c, consumed)),
        }
    })
}

fn string_expr<I>() -> impl Parser<Input = I, Output = String>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(char('"'), lex(char('"')), many(string_char()))
        .expected("string")
}

fn var_name<I>() -> impl Parser<Output = String, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    many1(alpha_num().or(satisfy(|c| c == '-' || c == '_')))
}

fn var_path<I>() -> impl Parser<Output = Arg, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    sep_by(var_name(), token('.'))
        .map(Arg::Var)
}

fn whitespace<I>() -> impl Parser<Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    skip_many1(space())
}

fn skip_whitespaces<I>() -> impl Parser<Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    skip_many(space())
}

//fn test_fun_expr<I>() -> impl Parser<Output = TestFun, Input = I>
//    where
//        I: Stream<Item = char>,
//        I::Error: ParseError<I::Item, I::Range, I::Position>,
//        <<I as StreamOnce>::Error as
//            ParseError<
//                char,
//                <I as StreamOnce>::Range,
//                <I as StreamOnce>::Position
//            >
//        >::StreamError: std::convert::From<failure::Compat<ParseSubstitutionError>>,
//        <I as StreamOnce>::Error: ParseError<
//            char,
//            <I as StreamOnce>::Range,
//            <I as StreamOnce>::Position
//        >
//{
//    many1::<String, _>(alpha_num())
//        .and_then(|s| s.parse())
//}

//    /// `[I]` represents a normal type parameters and lifetime declaration for the function
//    /// It gets expanded to `<I>`
//    fn integer_ex[I]()(I) -> i32
//    where [
//        I: Stream<Item = char>,
//        I::Error: ParseError<char, I::Range, I::Position>,
//        <I::Error as ParseError<I::Item, I::Range, I::Position>>::StreamError:
//            From<::std::num::ParseIntError>,
//    ]
//    {
//        // The body must be a block body ( `{ <block body> }`) which ends with an expression
//        // which evaluates to a parser
//        from_str(many1::<String, _>(digit()))
//    }
//}

//parser!{
//    fn test_fun_expr2[I]()(I) -> String
//    where [ I: Stream<Item = char>, ]
//    {
//        many1(letter()).and_then(|word: String| {
//            if word == "combine" {
//                Ok(word)
//            } else {
//                // The alias makes it easy to refer to the `StreamError` type of `I`
//                Err(StreamErrorFor::<I>::expected_static_message("combine"))
//            }
//        })
//    }
//}

fn test_fun_expr<I>() -> impl Parser<Output = TestFun, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1::<String, _>(alpha_num())
        .and_then(|s| {
            s.parse()
                .map_err(|e| StreamErrorFor::<I>::expected_static_message(
                    "one of test function"
                ))
        })
}

fn negated_test_fun_expr<I>() -> impl Parser<Output = TestFun, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        string("not"),
        whitespace(),
        test_fun_expr(),
    )
        .map(|(_, _, fun): (_, _, TestFun)| fun.negate())
}

fn var_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    var_path()
        .map(SubstExpr::Var)
}

fn is_operator_expr<I>() -> impl Parser<Output = OperatorFirstArg, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        arg_expr(),
        whitespace(),
        string("is"),
        whitespace(),
    )
        .map(|(arg, _, _, _)| OperatorFirstArg(arg))
}

fn op_expr<I>() -> impl Parser<Output = TestFun, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        string("==").map(|_| TestFun::Eq),
        string("!=").map(|_| TestFun::NotEq),
    ))
}

fn eq_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        var_path().skip(skip_whitespaces()),
        op_expr().skip(skip_whitespaces()),
        arg_expr(),
    )
        .map(|(arg1, fun, arg2)| {
            SubstExpr::Test {fun, args: vec!(arg1, arg2)}
        })
}

fn test_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        is_operator_expr(),
        choice((
            attempt(negated_test_fun_expr()),
            attempt(test_fun_expr()),
        ))
    )
        .map(|(first_arg, fun)| SubstExpr::Test {fun, args: vec!(first_arg.0)})
}

fn arg_expr<I>() -> impl Parser<Output = Arg, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    choice((
        float().map(Arg::Float),
        integer().map(Arg::Int),
        boolean().map(Arg::Bool),
        string_expr().map(Arg::Str),
        var_path(),
    ))
}

fn args_expr<I>() -> impl Parser<Output = Vec<Arg>, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    sep_by1(
        arg_expr().skip(skip_whitespaces()),
        token(',').skip(skip_whitespaces())
    )
}

fn fun_args_expr<I>() -> impl Parser<Output = Vec<Arg>, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        string("(").skip(skip_whitespaces()),
        string(")"),
        args_expr().skip(skip_whitespaces())
    )
}

fn test_with_args_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        is_operator_expr(),
        choice((
            attempt(negated_test_fun_expr()),
            attempt(test_fun_expr()),
        )),
        fun_args_expr(),
    )
        .map(|(first_arg, fun, rest_args)| {
            let mut args = vec!(first_arg.0);
            args.extend(rest_args);
            SubstExpr::Test {fun, args}
        })
}

fn subst_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        attempt(test_with_args_expr()),
        attempt(test_expr()),
        attempt(eq_expr()),
        attempt(var_expr()),
    ))
}

fn subst_part_expr<I>() -> impl Parser<Input = I, Output = TemplatePart>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        string("${{").skip(skip_whitespaces()),
        string("}}"),
        subst_expr().skip(skip_whitespaces())
    )
         .map(|v| TemplatePart::Subst(v))
}

fn gap<I>() -> impl Parser<Output = TemplatePart, Input = I>
    where
        <I as combine::StreamOnce>::Range: combine::stream::Range,
        I: combine::RangeStreamOnce,
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    recognize::<String, _>(escaped(
        take_while1(|c| c != '$' && c != '\\'), '\\', one_of(r#"$\"#.chars())
    ))
        .map(|s| TemplatePart::Gap(s))
}

pub(crate) fn template_parser<I>() -> impl Parser<Input = I, Output = Vec<TemplatePart>>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        I: RangeStreamOnce,
        <I as StreamOnce>::Range: Range,
{
    many(
        subst_part_expr().or(not_followed_by(eof().map(|_| "")).with(gap()))
    )
}

#[cfg(test)]
mod tests {
    use combine::Parser;
    use combine::easy::{Error as CombineError};
    use combine::error::StringStreamError;
    use combine::stream::state::{State, SourcePosition};
    use combine::stream::easy::{Error, Errors, Info};

    use matches::assert_matches;

    use super::{Arg, ParseSubstitutionError, TemplatePart, SubstExpr, TestFun};

    #[test]
    fn test_string_expr() {
        use super::string_expr;

        assert_eq!(
            string_expr().parse("\"\""),
            Ok(("".to_string(), ""))
        );
        assert_eq!(
            string_expr().parse("\"123\""),
            Ok(("123".to_string(), ""))
        );
        assert_eq!(
            string_expr().parse("\"\\\"\""),
            Ok(("\"".to_string(), ""))
        );
    }

    #[test]
    fn test_var_name_parser() {
        use super::var_name;

        assert_eq!(
            var_name().parse(""),
            Err(StringStreamError::UnexpectedParse)
        );
        assert_eq!(
            var_name().parse("a"),
            Ok(("a".to_string(), ""))
        );
        assert_eq!(
            var_name().parse("a."),
            Ok(("a".to_string(), "."))
        );
        assert_eq!(
            var_name().parse("1"),
            Ok(("1".to_string(), ""))
        );
        assert_eq!(
            var_name().parse("a_1-b_2"),
            Ok(("a_1-b_2".to_string(), ""))
        );
    }

    #[test]
    fn test_test_fun_expr() {
        use super::test_fun_expr;

        assert_eq!(
            test_fun_expr().easy_parse(State::new("defined")),
            Ok((
                TestFun::Defined,
                State::with_positioner(
                    "", SourcePosition { line: 1, column: 8 }
                )
            ))
        );
        assert_eq!(
            test_fun_expr().easy_parse(State::new("undefined")),
            Ok((
                TestFun::Undefined,
               State::with_positioner(
                   "", SourcePosition { line: 1, column: 10 }
               )
            ))
        );
        assert_matches!(
            test_fun_expr().easy_parse(State::new("unknown")),
            Err(Errors {
                position: SourcePosition { line: 1, column: 1 },
                errors
            })
        );
    }

    #[test]
    fn test_test_expr() {
        use super::test_expr;

        assert_matches!(
            test_expr().easy_parse(State::new("a is defined")),
            Ok((
                SubstExpr::Test {
                    fun: TestFun::Defined,
                    ref args,
                },
                State { input, .. }
            )) if args.len() == 1
            && args[0] == Arg::Var(vec!("a".to_string()))
            && input == ""
        );
    }

    #[test]
    fn test_test_with_args_expr() {
        use super::test_with_args_expr;

        assert_matches!(
            test_with_args_expr().easy_parse(State::new("a is eq(b)")),
            Ok((
                SubstExpr::Test {
                    fun: TestFun::Eq,
                    ref args,
                },
                State { input, .. }
            )) if args == &[
                Arg::Var(vec!("a".to_string())),
                Arg::Var(vec!("b".to_string())),
            ]
            && input == ""
        );
    }

    #[test]
    fn test_subst_expr() {
        use super::subst_expr;

        assert_matches!(
            subst_expr().easy_parse(State::new("a")),
            Ok((
                SubstExpr::Var(Arg::Var(ref var_path)),
                State { input, .. }
            )) if var_path == &["a".to_string()] && input == ""
        );
        assert_matches!(
            subst_expr().easy_parse("a is defined"),
            Ok((
                SubstExpr::Test {
                    fun: TestFun::Defined,
                    ref args,
                },
                input
            )) if args.len() == 1
            && args[0] == Arg::Var(vec!("a".to_string()))
            && input == ""
        );
        assert_matches!(
            subst_expr().easy_parse("a is eq(b)"),
            Ok((
                SubstExpr::Test {
                    fun: TestFun::Eq,
                    ref args,
                },
                input
            )) if args == &[
                Arg::Var(vec!("a".to_string())),
                Arg::Var(vec!("b".to_string())),
            ]
            && input == ""
        );
        assert_matches!(
            subst_expr().easy_parse("a == b"),
            Ok((
                SubstExpr::Test {
                    fun: TestFun::Eq,
                    ref args,
                },
                input
            )) if args == &[
                Arg::Var(vec!("a".to_string())),
                Arg::Var(vec!("b".to_string())),
            ]
            && input == ""
        );
        assert_matches!(
            subst_expr().easy_parse("a != b"),
            Ok((
                SubstExpr::Test {
                    fun: TestFun::NotEq,
                    ref args,
                },
                input
            )) if args == &[
                Arg::Var(vec!("a".to_string())),
                Arg::Var(vec!("b".to_string())),
            ]
            && input == ""
        );
    }

//    #[test]
//    fn test_var_path_parser() {
//        use super::var_path_expr;
//
//        assert_eq!(
//            var_path_expr().parse(""),
//            Err(StringStreamError::UnexpectedParse)
//        );
//        assert_eq!(
//            var_path_expr().parse("a.b"),
//            Ok((vec!("a".to_string(), "b".to_string()), ""))
//        );
//        assert_eq!(
//            var_path_expr().parse("a.b_2 "),
//            Ok((vec!("a".to_string(), "b_2".to_string()), " "))
//        );
//    }

    #[test]
    fn test_subst() {
        use super::subst_part_expr;

        assert_matches!(
            subst_part_expr().easy_parse(State::new("")),
            Err(Errors {
                position: SourcePosition { line: 1, column: 1 },
                ref errors
            }) if errors == &[
                Error::Unexpected(Info::Borrowed("end of input")),
                Error::Expected(Info::Borrowed("${{")),
            ]
        );
        assert_matches!(
            subst_part_expr().easy_parse(State::new("${}")),
            Err(Errors {
                position: SourcePosition { line: 1, column: 1 },
                ref errors
            }) if errors == &[
                Error::Unexpected(Info::Token('}')),
                Error::Expected(Info::Borrowed("${{")),
            ]
        );
//        assert_matches!(
//            subst_part_expr().easy_parse(State::new("${{}}")),
//            Err(Errors {
//                position: SourcePosition { line: 1, column: 4 },
//                ref errors
//            }) if errors == &[
//                Error::Unexpected(Info::Token('}')),
//                Error::Expected(Info::Borrowed("letter or digit"))
//            ]
//        );
        assert_matches!(
            subst_part_expr().easy_parse(State::new("${{a}}")),
            Ok((
                TemplatePart::Subst(SubstExpr::Var(Arg::Var(ref var_path))),
                State { input, .. }
            )) if var_path == &["a".to_string()]
            && input == ""
        );
        assert_matches!(
            subst_part_expr().easy_parse(State::new("${{ a }}")),
            Ok((
                TemplatePart::Subst(SubstExpr::Var(Arg::Var(ref var_path))),
                State { input, .. }
            )) if var_path == &["a".to_string()]
            && input == ""
        );
        assert_matches!(
            subst_part_expr().easy_parse(State::new("${{a.b}} ")),
            Ok((
               TemplatePart::Subst(SubstExpr::Var(Arg::Var(ref var_path))),
               State { input, .. }
            )) if var_path == &["a".to_string(), "b".to_string()]
            && input == " "
        );
    }

    #[test]
    fn test_gap() {
        use super::gap;

        assert_eq!(
            gap().parse(""),
            Ok((TemplatePart::Gap("".to_string()), ""))
        );
        assert_eq!(
            gap().parse("123 $ "),
            Ok((TemplatePart::Gap("123 ".to_string()), "$ "))
        );
        assert_eq!(
            gap().parse("123 \\${{}} \\\\${{}}"),
            Ok((TemplatePart::Gap("123 \\${{}} \\\\".to_string()), "${{}}"))
        );
    }

    #[test]
    fn test_template_parser() {
        use super::template_parser;

        assert_matches!(
            template_parser().easy_parse(State::new("")),
            Ok((
                ref parts,
                State { input, .. }
            )) if parts == &[] && input == ""
        );
        assert_matches!(
            template_parser().easy_parse(State::new("abc")),
            Ok((
                ref parts,
                State { input, .. }
            )) if parts == &[
                TemplatePart::Gap("abc".to_string()),
            ] && input == ""
        );

        let var_path = vec!("abc".to_string());
        assert_matches!(
            template_parser().easy_parse(State::new("${{abc}}")),
            Ok((
                ref parts,
                State { input, .. }
            )) if parts == &[
                TemplatePart::Subst(SubstExpr::Var(Arg::Var(var_path))),
            ] && input == ""
        );
//        assert_eq!(
//            template().parse("\\$${{abc}}: ${{x.y.0}}"),
//            Ok((vec![
//                TemplatePart::Gap("\\$".to_string()),
//                TemplatePart::Subst(vec!["abc".to_string()]),
//                TemplatePart::Gap(": ".to_string()),
//                TemplatePart::Subst(vec!["x".to_string(), "y".to_string(), "0".to_string()]),
//            ], ""))
//        );
//        assert_eq!(
//            template().parse("$"),
//            Err(StringStreamError::Eoi)
//        );
//        assert_eq!(
//            template().parse("${{"),
//            Err(StringStreamError::UnexpectedParse)
//        );
    }
}