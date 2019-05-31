use std::{char, i64, u32};
use std::num::ParseIntError;
use std::num::ParseFloatError;
use std::str::ParseBoolError;

use failure::{self, Fail, Error};

use pest::Parser;
use pest::error::Error as PestError;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};

use crate::template_grammar::{TemplateParser, Rule};

type TemplateResult<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq)]
pub enum TemplatePart {
    Gap(String),
    Subst(SubstExpr),
}

#[derive(Debug, PartialEq)]
pub enum SubstExpr {
    Atom(Arg),
    Test { fun: TestFun, args: Vec<SubstExpr> },
    Bool { op: BoolOp, args: Vec<SubstExpr> },
    Filter { fun: FilterFun, args: Vec<SubstExpr> }
}

#[derive(Debug, PartialEq)]
pub enum Arg {
    Var(String),
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
pub enum TestFun {
    Defined,
    Undefined,
    Eq,
    NotEq,
}

impl TestFun {
    fn negate(&self) -> TestFun {
        use self::TestFun::*;

        match self {
            Defined => Undefined,
            Undefined => Defined,
            Eq => NotEq,
            NotEq => Eq,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum FilterFun {
    CapFirst,
    Lower,
    Trim,
    Truncate,
    Upper,
}

#[derive(Debug, PartialEq)]
pub enum BoolOp {
    And,
    Or,
}

macro_rules! unexpected_rule {
    ($x:expr) => { unreachable!("unexpected rule: {:?}", $x); }
}

#[derive(Debug, Fail, PartialEq)]
pub enum ParseError {
    #[fail(display = "error when parsing template: {}", err)]
    Pest { err: PestError<Rule> },
    #[fail(display = "error parsing integer: {}", err)]
    Int { err: ParseIntError },
    #[fail(display = "error parsing float: {}", err)]
    Float { err: ParseFloatError },
    #[fail(display = "error parsing bool: {}", err)]
    Bool { err: ParseBoolError },
}

impl From<PestError<Rule>> for ParseError {
    fn from(err: PestError<Rule>) -> ParseError {
        return ParseError::Pest { err };
    }
}

impl From<ParseIntError> for ParseError {
    fn from(err: ParseIntError) -> ParseError {
        return ParseError::Int { err }
    }
}

impl From<ParseFloatError> for ParseError {
    fn from(err: ParseFloatError) -> ParseError {
        return ParseError::Float { err }
    }
}

impl From<ParseBoolError> for ParseError {
    fn from(err: ParseBoolError) -> ParseError {
        return ParseError::Bool { err }
    }
}

pub fn parse_template(tmpl: &str) -> TemplateResult<Vec<TemplatePart>> {
    let template_pairs = TemplateParser::parse(Rule::template, tmpl)?;
    for p in template_pairs {
        return match p.as_rule() {
            Rule::template => {
                Ok(process_template(p.into_inner())?)
            }
            rule => unreachable!("unexpected rule: {:?}", rule)
        }
    }
    unreachable!()
}

fn process_template(pairs: Pairs<Rule>) -> TemplateResult<Vec<TemplatePart>> {
    let mut template_parts = vec!();
    for p in pairs {
        let part = match p.as_rule() {
            Rule::EOI => continue,
            Rule::gap if p.as_str() == "" => continue,
            Rule::gap => TemplatePart::Gap(unescape_gap(p.as_str())),
            Rule::subst_expr => process_subst_expr(p.into_inner())?,
            rule => unreachable!("unexpected rule: {:?}", rule)
        };
        template_parts.push(part);
    }
    return Ok(template_parts);
}

fn unescape_gap(s: &str) -> String {
    let mut res = String::with_capacity(s.len());
    let mut is_backslashed = false;
    for c in s.chars() {
        match is_backslashed {
            false => {
                if c == '\\' {
                    is_backslashed = true;
                } else {
                    res.push(c);
                }
            },
            true => {
                match c {
                    '\\' | '$' => {
                        res.push(c);
                    }
                    _ => {
                        res.push('\\');
                        res.push(c);
                    }
                }
                is_backslashed = false;
            }
        }
    }
    res
}

fn process_subst_expr(pairs: Pairs<Rule>) -> TemplateResult<TemplatePart> {
    for p in pairs {
        return Ok(match p.as_rule() {
            Rule::expr => TemplatePart::Subst(process_expr(p)?),
            rule => unreachable!("unexpected rule: {:?}", rule),
        });
    }
    unreachable!()
}

fn process_expr(pair: Pair<Rule>) -> TemplateResult<SubstExpr> {
    let climber = PrecClimber::new(vec!(
        Operator::new(Rule::or_op, Assoc::Left),
        Operator::new(Rule::and_op, Assoc::Left),
    ));

    let infix = |lhs: TemplateResult<SubstExpr>, op: Pair<Rule>, rhs: TemplateResult<SubstExpr>| {
        let bool_op = match op.as_rule() {
            Rule::or_op => BoolOp::Or,
            Rule::and_op => BoolOp::And,
            rule => unreachable!(),
        };
        Ok(SubstExpr::Bool { op: bool_op, args: vec!(lhs?, rhs?) })
    };

    climber.climb(pair.into_inner(), process_bool_expr, infix)
}

fn process_bool_expr(pair: Pair<Rule>) -> TemplateResult<SubstExpr> {
    for p in pair.into_inner() {
        return match p.as_rule() {
            Rule::test_expr => process_test_expr(p),
            Rule::filter_expr => process_filter_expr(p),
            Rule::paren_bool_expr => process_paren_bool_expr(p),
            rule => unexpected_rule!(rule),
        }
    }
    unreachable!()
}

fn process_paren_bool_expr(pair: Pair<Rule>) -> TemplateResult<SubstExpr> {
    for p in pair.into_inner() {
        return match p.as_rule() {
            Rule::expr => process_expr(p),
            rule => unexpected_rule!(rule),
        }
    }
    unreachable!()
}

fn process_test_expr(pair: Pair<Rule>) -> TemplateResult<SubstExpr> {
    let mut inner = pair.into_inner();
    let lhs = process_filter_expr(
        inner.next().unwrap_or_else(|| unreachable!())
    )?;
    let op_pair = inner.next().unwrap_or_else(|| unreachable!());
    Ok(match op_pair.as_rule() {
        Rule::test_op => {
            let test_fun = process_test_op(op_pair)?;
            let rhs = process_filter_expr(
                inner.next().unwrap_or_else(|| unreachable!())
            )?;
            SubstExpr::Test { fun: test_fun, args: vec!(lhs, rhs) }
        },
        Rule::is_op => {
            let p = inner.next().unwrap_or_else(|| unreachable!());
            let test_fun = match p.as_rule() {
                Rule::not_op => {
                    process_test_fun(
                        inner.next().unwrap_or_else(|| unreachable!())
                    )?.negate()
                },
                Rule::test_fun => {
                    process_test_fun(p)?
                },
                rule => unexpected_rule!(rule),
            };
            let mut fun_args = vec!(lhs);
            if let Some(fargs) = inner.next() {
                fun_args.extend(process_fun_args(fargs)?);
            }
            SubstExpr::Test { fun: test_fun, args: fun_args }
        },
        rule => unexpected_rule!(rule),
    })
}

fn process_test_op(pair: Pair<Rule>) -> TemplateResult<TestFun> {
    Ok(match pair.as_str() {
        "==" => TestFun::Eq,
        "!=" => TestFun::NotEq,
        f => unreachable!("unknown test operator: {}", f),
    })
}

fn process_test_fun(pair: Pair<Rule>) -> TemplateResult<TestFun> {
    Ok(match pair.as_str() {
        "defined" => TestFun::Defined,
        "undefined" => TestFun::Undefined,
        "eq" | "equalto" => TestFun::Eq,
        "ne" => TestFun::NotEq,
        f => unreachable!("unknown test operator: {}", f),
    })
}

fn process_filter_expr(pair: Pair<Rule>) -> TemplateResult<SubstExpr> {
    fn get_fun_and_args(pairs: &mut Pairs<Rule>) -> TemplateResult<(Option<FilterFun>, Option<Vec<SubstExpr>>)> {
        Ok((
            match pairs.next() {
                Some(f) => Some(process_filter_fun(f)?),
                None => None,
            },
            match pairs.peek() {
                Some(a) => {
                    if let Rule::fun_args = a.as_rule() {
                        pairs.next();
                        Some(process_fun_args(a)?)
                    } else {
                        None
                    }
                },
                None => None,
            }
        ))
    }

    let mut inner = pair.into_inner();
    let mut res = SubstExpr::Atom(process_arg(
        inner.next().unwrap_or_else(|| unreachable!())
    )?);

    loop {
        match get_fun_and_args(&mut inner)? {
            (None, _) => break,
            (Some(fun), None) => {
                res = SubstExpr::Filter { fun, args: vec!(res) };
            },
            (Some(fun), Some(fun_args)) => {
                let mut args = vec!(res);
                args.extend(fun_args);
                res = SubstExpr::Filter { fun, args };
            },
        }
    }
    Ok(res)
}

fn process_filter_fun(pair: Pair<Rule>) -> TemplateResult<FilterFun> {
    Ok(match pair.as_str() {
        "cap_first" => FilterFun::CapFirst,
        "lower" => FilterFun::Lower,
        "trim" => FilterFun::Trim,
        "truncate" => FilterFun::Truncate,
        "upper" => FilterFun::Upper,
        f => unreachable!("unexpected filter function: {}", f),
    })
}

fn process_fun_args(pair: Pair<Rule>) -> TemplateResult<Vec<SubstExpr>> {
    let mut args = vec!();
    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::filter_expr => args.push(process_filter_expr(p)?),
            rule => unexpected_rule!(rule),
        }
    }
    Ok(args)
}

fn process_arg(pair: Pair<Rule>) -> TemplateResult<Arg> {
    for p in pair.into_inner() {
        return Ok(match p.as_rule() {
            Rule::value => Arg::Var(p.as_str().to_string()),
            Rule::string => Arg::Str(process_string(p)),
            Rule::int => Arg::Int(parse_int(p.as_str())?),
            Rule::float => Arg::Float(parse_float(p.as_str())?),
            Rule::bool => Arg::Bool(parse_bool(p.as_str())?),
            rule => unexpected_rule!(rule),
        });
    }
    unreachable!()
}

fn process_string(pair: Pair<Rule>) -> String {
    for p in pair.into_inner() {
        return match p.as_rule() {
            Rule::str => unescape(p.as_str()),
            rule => unreachable!(),
        };
    }
    unreachable!()
}

#[derive(Debug)]
enum UnescapeMode {
    Plain,
    Backslash,
    Unicode(String),
}

fn unescape(s: &str) -> String {
    use self::UnescapeMode::*;

    let mut unescaped = String::with_capacity(s.len());
    let mut mode = Plain;
    for c in s.chars() {
        match mode {
            Plain => {
                if c == '\\' {
                    mode = Backslash;
                } else {
                    unescaped.push(c);
                }
            },
            Backslash => {
                if c == 'u' {
                    mode = Unicode(String::with_capacity(4));
                } else {
                    unescaped.push(unescape_char(c));
                    mode = Plain;
                }
            }
            Unicode(ref mut acc) => {
                acc.push(c);
                if acc.len() == 4 {
                    unescaped.push(char::from_u32(
                        u32::from_str_radix(&acc, 16).unwrap()
                    ).unwrap());
                    mode = Plain;
                }
            }
        }
    }
    unescaped
}

fn unescape_char(c: char) -> char {
    match c {
        'b' => '\u{0008}',
        'f' => '\u{000c}',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        _ => c,
    }
}

fn parse_int(s: &str) -> TemplateResult<i64> {
    Ok(s.parse()?)
}

fn parse_float(s: &str) -> TemplateResult<f64> {
    Ok(s.parse()?)
}

fn parse_bool(s: &str) -> TemplateResult<bool> {
    Ok(s.parse()?)
}

mod tests {
    use std::f64;
    use super::{
        Arg,
        BoolOp,
        FilterFun,
        ParseError,
        Rule,
        SubstExpr,
        TemplateParser,
        TemplatePart,
        TestFun,
    };
    use pest::Parser;
    use pest::iterators::Pair;

    fn parse_single(rule: Rule, input: &str) -> Pair<Rule> {
        return TemplateParser::parse(rule, input).unwrap().next().unwrap();
    }

    #[test]
    fn test_string_arg() {
        use super::process_arg;

        assert_eq!(
            process_arg(parse_single(Rule::arg, "\"\"")),
            Ok(Arg::Str("".to_string())),
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, r#""\"\\\r\n\"""#)),
            Ok(Arg::Str("\"\\\r\n\"".to_string())),
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "\"\\u2014\"")),
            Ok(Arg::Str("—".to_string())),
        );
    }

    #[test]
    fn test_int_arg() {
        use super::process_arg;

        assert_eq!(
            process_arg(parse_single(Rule::arg, "0")),
            Ok(Arg::Int(0))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "+0")),
            Ok(Arg::Int(0))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "-0")),
            Ok(Arg::Int(0))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "1")),
            Ok(Arg::Int(1))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "-1")),
            Ok(Arg::Int(-1))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "9223372036854775807")),
            Ok(Arg::Int(9223372036854775807))
        );
        assert!(
            process_arg(parse_single(Rule::arg, "9223372036854775808")).is_err()
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "-9223372036854775808")),
            Ok(Arg::Int(-9223372036854775808))
        );
        assert!(
            process_arg(parse_single(Rule::arg, "-9223372036854775809")).is_err()
        );
    }

    #[test]
    fn test_float_arg() {
        use super::process_arg;

        assert_eq!(
            process_arg(parse_single(Rule::arg, "0.")),
            Ok(Arg::Float(0.0))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "0.0")),
            Ok(Arg::Float(0.0))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "-1e-1")),
            Ok(Arg::Float(-0.1))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "4.9e-324")),
            Ok(Arg::Float(4.9e-324))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "4.9e-325")),
            Ok(Arg::Float(0.0))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "1.7976931348623157E308")),
            Ok(Arg::Float(1.7976931348623157e308))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "1.7976931348623157E3099")),
            Ok(Arg::Float(f64::INFINITY))
        );
    }

    #[test]
    fn test_bool_arg() {
        use super::process_arg;

        assert_eq!(
            process_arg(parse_single(Rule::arg, "true")),
            Ok(Arg::Bool(true))
        );
        assert_eq!(
            process_arg(parse_single(Rule::arg, "false")),
            Ok(Arg::Bool(false))
        );
    }

    #[test]
    fn test_test_expr() {
        use super::process_test_expr;

        assert_eq!(
            process_test_expr(parse_single(Rule::test_expr, "1 == 2")),
            Ok(SubstExpr::Test {
                fun: TestFun::Eq,
                args: vec!(
                    SubstExpr::Atom(Arg::Int(1)),
                    SubstExpr::Atom(Arg::Int(2)),
                ),
            })
        );
        assert_eq!(
            process_test_expr(parse_single(Rule::test_expr, "true != false")),
            Ok(SubstExpr::Test {
                fun: TestFun::NotEq,
                args: vec!(
                    SubstExpr::Atom(Arg::Bool(true)),
                    SubstExpr::Atom(Arg::Bool(false)),
                ),
            })
        );
        assert_eq!(
            process_test_expr(parse_single(Rule::test_expr, "a is defined")),
            Ok(SubstExpr::Test {
                fun: TestFun::Defined,
                args: vec!(
                    SubstExpr::Atom(Arg::Var("a".to_string())),
                ),
            })
        );
        assert_eq!(
            process_test_expr(parse_single(Rule::test_expr, "1 is eq(2)")),
            Ok(SubstExpr::Test {
                fun: TestFun::Eq,
                args: vec!(
                    SubstExpr::Atom(Arg::Int(1)),
                    SubstExpr::Atom(Arg::Int(2)),
                ),
            })
        );
        assert_eq!(
            process_test_expr(parse_single(Rule::test_expr, "a is defined")),
            Ok(SubstExpr::Test {
                fun: TestFun::Defined,
                args: vec!(
                    SubstExpr::Atom(Arg::Var("a".to_string())),
                ),
            })
        );
        assert_eq!(
            process_test_expr(parse_single(Rule::test_expr, "1 is not eq(2)")),
            Ok(SubstExpr::Test {
                fun: TestFun::NotEq,
                args: vec!(
                    SubstExpr::Atom(Arg::Int(1)),
                    SubstExpr::Atom(Arg::Int(2)),
                ),
            })
        );
    }

    #[test]
    fn test_filter_expr() {
        use super::process_filter_expr;

        assert_eq!(
            process_filter_expr(parse_single(Rule::filter_expr, "true")),
            Ok(SubstExpr::Atom(Arg::Bool(true)))
        );
        assert_eq!(
            process_filter_expr(parse_single(Rule::filter_expr, "\"test\" | cap_first")),
            Ok(SubstExpr::Filter {
                fun: FilterFun::CapFirst,
                args: vec!(SubstExpr::Atom(Arg::Str("test".to_string())))
            })
        );
        assert_eq!(
            process_filter_expr(parse_single(Rule::filter_expr, "\"test\" | truncate(3)")),
            Ok(SubstExpr::Filter {
                fun: FilterFun::Truncate,
                args: vec!(
                    SubstExpr::Atom(Arg::Str("test".to_string())),
                    SubstExpr::Atom(Arg::Int(3)),
                )
            })
        );
        assert_eq!(
            process_filter_expr(parse_single(Rule::filter_expr, "x|lower|truncate(3)")),
            Ok(SubstExpr::Filter {
                fun: FilterFun::Truncate,
                args: vec!(
                    SubstExpr::Filter {
                        fun: FilterFun::Lower,
                        args: vec!(
                            SubstExpr::Atom(Arg::Var("x".to_string())),
                        ),
                    },
                    SubstExpr::Atom(Arg::Int(3)),
                )
            })
        );
    }

    #[test]
    fn test_expr() {
        use super::process_expr;

        assert_eq!(
            process_expr(parse_single(
                Rule::expr,
                "true and false or true"
            )),
            Ok(
                SubstExpr::Bool {
                    op: BoolOp::Or,
                    args: vec!(
                        SubstExpr::Bool {
                            op: BoolOp::And,
                            args: vec!(
                                SubstExpr::Atom(Arg::Bool(true)),
                                SubstExpr::Atom(Arg::Bool(false)),
                            )
                        },
                        SubstExpr::Atom(Arg::Bool(true)),
                    ),
                }
            )
        );
        assert_eq!(
            process_expr(parse_single(
                Rule::expr,
                "true or false and true"
            )),
            Ok(
                SubstExpr::Bool {
                    op: BoolOp::Or,
                    args: vec!(
                        SubstExpr::Atom(Arg::Bool(true)),
                        SubstExpr::Bool {
                            op: BoolOp::And,
                            args: vec!(
                                SubstExpr::Atom(Arg::Bool(false)),
                                SubstExpr::Atom(Arg::Bool(true)),
                            )
                        },
                    ),
                }
            )
        );
        assert_eq!(
            process_expr(parse_single(
                Rule::expr,
                "true and (false or true)"
            )),
            Ok(
                SubstExpr::Bool {
                    op: BoolOp::And,
                    args: vec!(
                        SubstExpr::Atom(Arg::Bool(true)),
                        SubstExpr::Bool {
                            op: BoolOp::Or,
                            args: vec!(
                                SubstExpr::Atom(Arg::Bool(false)),
                                SubstExpr::Atom(Arg::Bool(true)),
                            )
                        },
                    ),
                }
            )
        );
        assert_eq!(
            process_expr(parse_single(
                Rule::expr,
                r#"y or x == "OK""#
            )),
            Ok(SubstExpr::Bool {
                op: BoolOp::Or,
                args: vec!(
                    SubstExpr::Atom(Arg::Var("y".to_string())),
                    SubstExpr::Test {
                        fun: TestFun::Eq,
                        args: vec!(
                            SubstExpr::Atom(Arg::Var("x".to_string())),
                            SubstExpr::Atom(Arg::Str("OK".to_string())),
                        )
                    },
                )
            }),
        );
        assert_eq!(
            process_expr(parse_single(
                Rule::expr,
                r#"y or x|upper == "OK""#
            )),
            Ok(SubstExpr::Bool {
                op: BoolOp::Or,
                args: vec!(
                    SubstExpr::Atom(Arg::Var("y".to_string())),
                    SubstExpr::Test {
                        fun: TestFun::Eq,
                        args: vec!(
                            SubstExpr::Filter {
                                fun: FilterFun::Upper,
                                args: vec!(
                                    SubstExpr::Atom(Arg::Var("x".to_string())),

                                )
                            },
                            SubstExpr::Atom(Arg::Str("OK".to_string())),
                        )
                    },
                )
            }),
        );
        assert_eq!(
            process_expr(parse_single(
                Rule::expr,
                r#"x is defined and x|trim == "test" or y|lower|truncate(2) is not eq("ok")"#
            )),
            Ok(
                SubstExpr::Bool {
                    op: BoolOp::Or,
                    args: vec!(
                        SubstExpr::Bool {
                            op: BoolOp::And,
                            args: vec!(
                                SubstExpr::Test {
                                    fun: TestFun::Defined,
                                    args: vec!(
                                        SubstExpr::Atom(Arg::Var("x".to_string())),
                                    )
                                },
                                SubstExpr::Test {
                                    fun: TestFun::Eq,
                                    args: vec!(
                                        SubstExpr::Filter {
                                            fun: FilterFun::Trim,
                                            args: vec!(
                                                SubstExpr::Atom(Arg::Var("x".to_string())),
                                            )
                                        },
                                        SubstExpr::Atom(Arg::Str("test".to_string())),
                                    )
                                },
                            ),
                        },
                        SubstExpr::Test {
                            fun: TestFun::NotEq,
                            args: vec!(
                                SubstExpr::Filter {
                                    fun: FilterFun::Truncate,
                                    args: vec!(
                                        SubstExpr::Filter {
                                            fun: FilterFun::Lower,
                                            args: vec!(
                                                SubstExpr::Atom(Arg::Var("y".to_string()))
                                            )
                                        },
                                        SubstExpr::Atom(Arg::Int(2)),
                                    )
                                },
                                SubstExpr::Atom(Arg::Str("ok".to_string())),
                            )
                        }
                    )
                }
            )
        );
    }

    #[test]
    fn test_parse_template() {
        use super::parse_template;

        assert_eq!(
            parse_template(""),
            Ok(vec!())
        );
        assert!(
            parse_template("${{}}").is_err()
        );
        assert_eq!(
            parse_template("${{ 1 }}"),
            Ok(vec!(
                TemplatePart::Subst(SubstExpr::Atom(Arg::Int(1))),
            ))
        );
        assert_eq!(
            parse_template("${{ 1 }} ${{2}}"),
            Ok(vec!(
                TemplatePart::Subst(SubstExpr::Atom(Arg::Int(1))),
                TemplatePart::Gap(" ".to_string()),
                TemplatePart::Subst(SubstExpr::Atom(Arg::Int(2))),
            ))
        );
        assert_eq!(
            parse_template(r"\${{ 1 }} \\${{2}}"),
            Ok(vec!(
                TemplatePart::Gap(r"${{ 1 }} \".to_string()),
                TemplatePart::Subst(SubstExpr::Atom(Arg::Int(2))),
            ))
        );
    }
}
