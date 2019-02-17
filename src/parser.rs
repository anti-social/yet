use std::error::Error as StdError;
use std::str::FromStr;

use failure::{self, Fail, Compat, format_err};
//use failure_derive::Fail;

use combine::{ParseError, Parser, Stream};
use combine::{between, eof, one_of, many, many1, not_followed_by, satisfy,
              sep_by1, skip_many1, token};
use combine::parser::char::{alpha_num, space, string};
use combine::parser::combinator::{attempt, recognize};
use combine::parser::range::take_while1;
use combine::parser::repeat::escaped;
use combine::stream::StreamOnce;

#[derive(Debug, PartialEq)]
pub enum TemplatePart {
    Gap(String),
    Subst(Vec<String>),
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub path: Vec<String>,
}

#[derive(Debug, Fail)]
pub enum ParseSubstitutionError {
    #[fail(display = "error when parsing substitution expression")]
    ParseError
}

#[derive(Debug, PartialEq)]
pub enum TestFun {
    Defined,
    Undefined,
}

impl TestFun {
    fn test(var: Variable) -> bool {
        true
    }
}

impl FromStr for TestFun {
    type Err = failure::Compat<ParseSubstitutionError>;

    fn from_str(name: &str) -> Result<TestFun, Self::Err> {
        use self::TestFun::*;

        Ok(match name {
            "defined" => Defined,
            "undefined" => Undefined,
            _ => {
                return Err(ParseSubstitutionError::ParseError.compat());
            },
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum SubstExpr {
    Var(Variable),
    Test { var: Variable, fun: TestFun },
}

fn var_name<I>() -> impl Parser<Output = String, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    many1(alpha_num().or(satisfy(|c| c == '-' || c == '_')))
}

fn var_path<I>() -> impl Parser<Output = Vec<String>, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    sep_by1(var_name(), token('.'))
}

fn var_path_expr<I>() -> impl Parser<Output = Variable, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    sep_by1(var_name(), token('.'))
        .map(|path| Variable { path })
}

fn whitespace<I>() -> impl Parser<Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    skip_many1(space())
}

fn var_name_expr<I>() -> impl Parser<Output = String, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(alpha_num().or(satisfy(|c| c == '-' || c == '_')))
}

fn test_fun_expr<I>() -> impl Parser<Output = TestFun, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1::<String, _>(alpha_num())
        .map(|name| TestFun::Defined)
}

fn test_fun_expr2<I>() -> impl Parser<Output = TestFun, Input = I>
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

fn test_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        var_path_expr(),
        whitespace(),
        string("is"),
        whitespace(),
        test_fun_expr(),
    )
        .map(|(var, _, _, _, fun)| SubstExpr::Test {var, fun})
}

fn test_expr2<I>() -> impl Parser<Output = SubstExpr, Input = I>
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
        var_path_expr(),
        whitespace(),
        string("is"),
        whitespace(),
        test_fun_expr2(),
    )
        .map(|(var, _, _, _, fun)| SubstExpr::Test {var, fun})
}

fn subst_expr<I>() -> impl Parser<Output = SubstExpr, Input = I>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    attempt(test_expr())
        .or(var_expr())
}

fn subst_expr2<I>() -> impl Parser<Output = SubstExpr, Input = I>
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
    attempt(test_expr())
        .or(var_expr())
}

fn subst<I>() -> impl Parser<Input = I, Output = TemplatePart>
    where
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(string("${{"), string("}}"), var_path())
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
        <I as combine::stream::StreamOnce>::Range: combine::stream::Range,
        I: combine::stream::RangeStreamOnce,
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    many(
        subst().or(not_followed_by(eof().map(|_| "")).with(gap()))
    )
}

#[cfg(test)]
mod tests {
    use combine::Parser;
    use combine::easy::{Error as CombineError};
    use combine::error::StringStreamError;
    use combine::stream::state::{State, SourcePosition};
    use combine::stream::easy::Errors;

    use matches::assert_matches;

    use super::{ParseSubstitutionError, TemplatePart, SubstExpr, Variable, TestFun};
    use super::{var_name, var_name_expr, var_path, subst, gap, template};

    #[test]
    fn test_var_name_parser() {
        let tmpl = "".to_string();
        assert_eq!(
            var_name().parse(&*tmpl),
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
    fn test_var_name_expr() {
        assert_eq!(
            var_name_expr().parse("a"),
            Ok(("a".to_string(), ""))
        );
    }

    #[test]
    fn test_test_fun_expr() {
        use super::test_fun_expr;

        assert_eq!(
            test_fun_expr().parse("defined"),
            Ok((
                TestFun::Defined,
                ""
            ))
        );
        assert_eq!(
            test_fun_expr().parse("undefined"),
            Ok((
                TestFun::Undefined,
                ""
            ))
        );
    }

    #[test]
    fn test_test_fun_expr2() {
        use failure::Fail;
        use super::test_fun_expr2;

        assert_eq!(
            test_fun_expr2().easy_parse(State::new("defined")),
            Ok((
                TestFun::Defined,
                State::with_positioner(
                    "", SourcePosition { line: 1, column: 8 }
                )
            ))
        );
        assert_eq!(
            test_fun_expr2().easy_parse(State::new("undefined")),
            Ok((
                TestFun::Undefined,
               State::with_positioner(
                   "", SourcePosition { line: 1, column: 10 }
               )
            ))
        );
        assert_matches!(
            test_fun_expr2().easy_parse(State::new("unknown")),
            Err(Errors {
                position: SourcePosition { line: 1, column: 1 },
                errors
            })
        );
    }

    #[test]
    fn test_test_expr() {
        use super::test_expr;

        assert_eq!(
            test_expr().parse("a is defined"),
            Ok((
                SubstExpr::Test {
                    var: Variable { path: vec!("a".to_string()) },
                    fun: TestFun::Defined,
                },
                ""
            ))
        );
    }

    #[test]
    fn subst_expr() {
        use super::subst_expr;

        assert_eq!(
            subst_expr().parse("a"),
            Ok((
                SubstExpr::Var(Variable { path: vec!("a".to_string()) }),
                ""
            ))
        );
        assert_eq!(
            subst_expr().parse("a is defined"),
            Ok((
                SubstExpr::Test {
                    var: Variable { path: vec!("a".to_string()) },
                    fun: TestFun::Defined,
                },
                ""
            ))
        );
    }

    #[test]
    fn subst_expr2() {
        use super::subst_expr2;

        assert_matches!(
            subst_expr2().easy_parse(State::new("a")),
            Ok((
                SubstExpr::Var(Variable { path: ref var_path }),
                State { input, .. }
            )) if var_path == &["a".to_string()] && input == ""
        );
        assert_matches!(
            subst_expr2().easy_parse("a is defined"),
            Ok((
                SubstExpr::Test {
                    var: Variable { path: ref var_path },
                    fun: TestFun::Defined,
                },
                input
            )) if var_path == &["a".to_string()] && input == ""
        );
    }

    #[test]
    fn test_var_path_parser() {
        assert_eq!(
            var_name().parse(""),
            Err(StringStreamError::UnexpectedParse)
        );
        assert_eq!(
            var_path().parse("a.b"),
            Ok((vec!("a".to_string(), "b".to_string()), ""))
        );
        assert_eq!(
            var_path().parse("a.b_2 "),
            Ok((vec!("a".to_string(), "b_2".to_string()), " "))
        );
    }

    #[test]
    fn test_subst() {
        assert_eq!(
            subst().parse(""),
            Err(StringStreamError::Eoi)
        );
        assert_eq!(
            subst().parse("${}"),
            Err(StringStreamError::UnexpectedParse)
        );
        assert_eq!(
            subst().parse("${{}}"),
            Err(StringStreamError::UnexpectedParse)
        );
        assert_eq!(
            subst().parse("${{a}}"),
            Ok((TemplatePart::Subst(vec!("a".to_string())), ""))
        );
        assert_eq!(
            subst().parse("${{a.b}} "),
            Ok((TemplatePart::Subst(vec!("a".to_string(), "b".to_string())), " "))
        );
    }

    #[test]
    fn test_gap() {
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
        assert_eq!(
            template().parse(""),
            Ok((vec![], ""))
        );
        assert_eq!(
            template().parse("abc"),
            Ok((vec![TemplatePart::Gap("abc".to_string())], ""))
        );
        assert_eq!(
            template().parse("${{abc}}"),
            Ok((vec![TemplatePart::Subst(vec!["abc".to_string()])], ""))
        );
        assert_eq!(
            template().parse("\\$${{abc}}: ${{x.y.0}}"),
            Ok((vec![
                TemplatePart::Gap("\\$".to_string()),
                TemplatePart::Subst(vec!["abc".to_string()]),
                TemplatePart::Gap(": ".to_string()),
                TemplatePart::Subst(vec!["x".to_string(), "y".to_string(), "0".to_string()]),
            ], ""))
        );
        assert_eq!(
            template().parse("$"),
            Err(StringStreamError::Eoi)
        );
        assert_eq!(
            template().parse("${{"),
            Err(StringStreamError::UnexpectedParse)
        );
    }
}