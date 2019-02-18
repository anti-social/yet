use std::error::Error as StdError;
use std::str::FromStr;

use failure::{self, Fail, Compat, format_err};
//use failure_derive::Fail;

use combine::{ParseError, Parser, Stream};
use combine::{between, eof, one_of, many, many1, not_followed_by, satisfy,
              sep_by, sep_by1, skip_many, skip_many1, token};
use combine::parser::char::{alpha_num, space, string};
use combine::parser::combinator::{attempt, recognize};
use combine::parser::range::take_while1;
use combine::parser::repeat::escaped;
use combine::stream::{Range, RangeStreamOnce, StreamOnce};

#[derive(Debug, PartialEq)]
pub enum TemplatePart {
    Gap(String),
    Subst(SubstExpr),
}

#[derive(Debug, PartialEq)]
pub enum Arg {
    Var(Vec<String>),
//    Str(String),
//    Int(i64),
//    Float(f64),
//    Bool(bool),
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

fn var_name_expr<I>() -> impl Parser<Output = String, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    many1(alpha_num().or(satisfy(|c| c == '-' || c == '_')))
}

fn var_path_expr<I>() -> impl Parser<Output = Arg, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    sep_by(var_name_expr(), token('.'))
        .map(|path| Arg::Var(path))
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

fn test_fun_expr<I>() -> impl Parser<Output = TestFun, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        <<I as StreamOnce>::Error as
            ParseError<
                char,
                <I as StreamOnce>::Range,
                <I as StreamOnce>::Position
            >
        >::StreamError: std::convert::From<failure::Compat<ParseSubstitutionError>>,
        <I as StreamOnce>::Error: ParseError<
            char,
            <I as StreamOnce>::Range,
            <I as StreamOnce>::Position
        >
{
    many1::<String, _>(alpha_num())
        .and_then(|s| s.parse())
}

fn var_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    var_path_expr()
        .map(|var| SubstExpr::Var(var))
}

fn is_operator_expr<I>() -> impl Parser<Output = OperatorFirstArg, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        var_path_expr(),
        whitespace(),
        string("is"),
        whitespace(),
    )
        .map(|(var, _, _, _)| OperatorFirstArg(var))
}

fn eq_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        var_path_expr().skip(skip_whitespaces()),
        string("==").skip(skip_whitespaces()),
        var_path_expr(),
    )
        .map(|(arg1, _, arg2)| {
            SubstExpr::Test {fun: TestFun::Eq, args: vec!(arg1, arg2)}
        })
}

fn test_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        <<I as StreamOnce>::Error as
            ParseError<
                char,
                <I as StreamOnce>::Range,
                <I as StreamOnce>::Position
            >
        >::StreamError: std::convert::From<failure::Compat<ParseSubstitutionError>>,
        <I as StreamOnce>::Error: ParseError<
            char,
            <I as StreamOnce>::Range,
            <I as StreamOnce>::Position
        >
{
    (
        is_operator_expr(),
        test_fun_expr(),
    )
        .map(|(first_arg, fun)| SubstExpr::Test {fun, args: vec!(first_arg.0)})
}

fn arg_expr<I>() -> impl Parser<Output = Arg, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    var_path_expr()
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
        <<I as StreamOnce>::Error as
        ParseError<
            char,
            <I as StreamOnce>::Range,
            <I as StreamOnce>::Position
        >
        >::StreamError: std::convert::From<failure::Compat<ParseSubstitutionError>>,
        <I as StreamOnce>::Error: ParseError<
            char,
            <I as StreamOnce>::Range,
            <I as StreamOnce>::Position
        >
{
    (
        is_operator_expr(),
        test_fun_expr(),
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
        <<I as StreamOnce>::Error as
            ParseError<
                char,
                <I as StreamOnce>::Range,
                <I as StreamOnce>::Position
            >
        >::StreamError: std::convert::From<failure::Compat<ParseSubstitutionError>>,
        <I as StreamOnce>::Error: ParseError<
            char,
            <I as StreamOnce>::Range,
            <I as StreamOnce>::Position
        >
{
    attempt(test_with_args_expr())
        .or(attempt(test_expr()))
        .or(attempt(eq_expr()))
        .or(var_expr())
}

fn subst_part_expr<I>() -> impl Parser<Input = I, Output = TemplatePart>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        <<I as StreamOnce>::Error as
            ParseError<
                char,
                <I as StreamOnce>::Range,
                <I as StreamOnce>::Position
            >
        >::StreamError: std::convert::From<failure::Compat<ParseSubstitutionError>>,
        <I as StreamOnce>::Error: ParseError<
            char,
            <I as StreamOnce>::Range,
            <I as StreamOnce>::Position
        >
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

pub fn template<I>() -> impl Parser<Input = I, Output = Vec<TemplatePart>>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
        I: RangeStreamOnce,
        <I as StreamOnce>::Range: Range,
        <<I as StreamOnce>::Error as ParseError<
            char, <I as StreamOnce>::Range, <I as StreamOnce>::Position>
        >::StreamError: std::convert::From<failure::Compat<ParseSubstitutionError>>,
        <I as StreamOnce>::Error: ParseError<
            char, <I as StreamOnce>::Range, <I as StreamOnce>::Position
        >,
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
    fn test_var_name_parser() {
        use super::var_name_expr;

        assert_eq!(
            var_name_expr().parse(""),
            Err(StringStreamError::UnexpectedParse)
        );
        assert_eq!(
            var_name_expr().parse("a"),
            Ok(("a".to_string(), ""))
        );
        assert_eq!(
            var_name_expr().parse("a."),
            Ok(("a".to_string(), "."))
        );
        assert_eq!(
            var_name_expr().parse("1"),
            Ok(("1".to_string(), ""))
        );
        assert_eq!(
            var_name_expr().parse("a_1-b_2"),
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
    fn test_template() {
        use super::template;

        assert_matches!(
            template().easy_parse(State::new("")),
            Ok((
                ref parts,
                State { input, .. }
            )) if parts == &[] && input == ""
        );
        assert_matches!(
            template().easy_parse(State::new("abc")),
            Ok((
                ref parts,
                State { input, .. }
            )) if parts == &[
                TemplatePart::Gap("abc".to_string()),
            ] && input == ""
        );

        let var_path = vec!("abc".to_string());
        assert_matches!(
            template().easy_parse(State::new("${{abc}}")),
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