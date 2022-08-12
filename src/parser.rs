use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{escaped, is_not, tag, take_until};
use nom::character::complete::{alphanumeric1, anychar, char, digit1, multispace0, one_of};
use nom::combinator::{cut, eof, map, map_parser, map_res, recognize, verify};
use nom::error::context;
use nom::multi::{many0, many0_count, many1_count, separated_list1};
use nom::sequence::{delimited, preceded, terminated, tuple};

#[derive(Debug, PartialEq)]
pub enum TemplatePart {
    Gap(String),
    Expr(String),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Str(String),
    Int(i64),
    Val(String),
    GetAttr(Box<Expr>, String),
    GetItem(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn into_boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

fn parse_str(input: &str) -> IResult<&str, &str> {
    escaped(
        alphanumeric1, '\\', one_of("\"n\\")
    )(input)
}

fn string(input: &str) -> IResult<&str, &str> {
    context(
        "string",
        preceded(
            char('\"'),
            cut(terminated(parse_str, char('\"')))
        ),
  )(input)
}

fn int(input: &str) -> IResult<&str, i64> {
    context(
        "int",
        map_res(
            digit1,
            str::parse
        )
    )(input)
}

fn val_name(input: &str) -> IResult<&str, &str> {
    recognize(
        many1_count(alt((
            alphanumeric1,
            tag("_"),
            tag("-")
        )))
    )(input)
}

fn val_attr(input: &str) -> IResult<&str, (Expr, &str)> {
    context(
        "value_attr",
        map(
            tuple((
                parse_expression,
                tag("."),
                cut(val_name)
            )),
            |(v, _, a)| (v, a)
        )
    )(input)
}

fn val_index(input: &str) -> IResult<&str, (Expr, Expr)> {
    context(
        "value_index",
        tuple((
            parse_expression,
            delimited(
                tag("["),
                preceded(multispace0, parse_expression),
                cut(preceded(multispace0, tag("]")))
            )
        ))
    )(input)
}

fn parse_expression(input: &str) -> IResult<&str, Expr> {
    todo!()
    // alt((
    //     map(string, |v| Expr::Str(v.to_string())),
    //     map(int, Expr::Int),
    //     map(val_name, |v| Expr::Val(v.to_string())),
    //     map(val_attr, |(v, a)| Expr::GetAttr(Box::new(v), a.to_string())),
    //     map(val_index, |(v, i)| Expr::GetItem(Box::new(v), Box::new(i))),
    // ))(input)
}


fn var_name(input: &str) -> IResult<&str, &str> {
    recognize(
        many1_count(alt((
            alphanumeric1,
            tag("_"),
            tag("-")
        )))
    )(input)
}

fn var_path(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list1(tag("."), var_name)(input)
}

fn expr(input: &str) -> IResult<&str, &str> {
    take_until("}}")(input)
        .map(|(next_input, expr)| {
            (next_input, expr.trim())
        })
}

fn subst(input: &str) -> IResult<&str, TemplatePart> {
    map(
        delimited(
            tag("${{"),
            delimited(multispace0, expr, multispace0),
            // delimited(multispace0, var_path, multispace0),
            tag("}}")
        ),
        |e| TemplatePart::Expr(e.to_string())
    )(input)
}

fn gap(input: &str) -> IResult<&str, TemplatePart> {
    map(
        verify(
            escaped(
                is_not("$\\"),
                '\\',
                one_of("$\\")
            ),
            |s: &&str| !s.is_empty()
        ),
        |s: &str| TemplatePart::Gap(s.to_string())
    )(input)
}

pub fn template(input: &str) -> IResult<&str, Vec<TemplatePart>> {
    terminated(
        many0(
            alt((
                subst,
                gap,
            ))
        ),
        eof
    )(input)
}

#[cfg(test)]
mod tests {
    use nom::{Err, error_position};
    use nom::error::ErrorKind;
    // use crate::parser::TemplatePart::Expr;
    use crate::parser::{Expr, parse_expression, val_attr, val_index};

    use super::{gap, subst, template, TemplatePart, var_name, var_path};

    // #[test]
    // fn test_expression() {
    //     use Expr::*;
    //
    //     assert_eq!(
    //         parse_expression("a.b"),
    //         Ok((
    //             "",
    //             GetAttr(
    //                 Val("a".to_string()).into_boxed(),
    //                 "b".to_string()
    //             )
    //         ))
    //     );
    //     // assert_eq!(
    //     //     expression("a.b.c"),
    //     //     Ok((
    //     //         "",
    //     //         GetAttr(
    //     //             GetAttr(
    //     //                 Val("a".to_string()).into_boxed(),
    //     //                 "b".to_string()
    //     //             ).into_boxed(),
    //     //             "c".to_string()
    //     //         )
    //     //     ))
    //     // );
    // }

    // #[test]
    // fn test_expression() {
    //     assert_eq!(
    //         val_index("a[b]"),
    //         Ok(("", (Expr::Val("a".to_string()), Expr::Val("b".to_string()))))
    //     );
    //
    //     assert_eq!(
    //         val_index("a[\"b\"]"),
    //         Ok(("", (Expr::Val("a".to_string()), Expr::Str("b".to_string()))))
    //     );
    //
    //     assert_eq!(
    //         val_index("a[0]"),
    //         Ok(("", (Expr::Val("a".to_string()), Expr::Int(0))))
    //     );
    //
    //     // assert_eq!(
    //     //     val_index("b.c[0]"),
    //     //     Ok((
    //     //         "",
    //     //         (
    //     //             Expr::GetAttr(Box::new(Expr::Val("b".to_string())), "c".to_string()),
    //     //             Expr::Int(0)
    //     //         )
    //     //     ))
    //     // );
    //     // assert_eq!(
    //     //     val_index("a[b.c[0]]"),
    //     //     Ok((
    //     //         "",
    //     //         (
    //     //             Expr::Val("a".to_string()),
    //     //             Expr::Index(
    //     //                 Box::new(Expr::Attr(Box::new(Expr::Val("b".to_string())), "c".to_string())),
    //     //                 Box::new(Expr::Int(0))
    //     //             )
    //     //         )
    //     //     ))
    //     // );
    // }


    #[test]
    fn test_var_name_parser() {

        assert_eq!(
            var_name(""),
            Err(Err::Error(error_position!("", ErrorKind::Many1Count)))
        );
        assert_eq!(
            var_name("a"),
            Ok(("", "a"))
        );
        assert_eq!(
            var_name("a."),
            Ok((".", "a"))
        );
        assert_eq!(
            var_name("1"),
            Ok(("", "1"))
        );
        assert_eq!(
            var_name("a_1-b_2"),
            Ok(("", "a_1-b_2"))
        );
    }

    #[test]
    fn test_var_path_parser() {
        assert_eq!(
            var_path(""),
            Err(Err::Error(error_position!("", ErrorKind::Many1Count)))
        );
        assert_eq!(
            var_path("a.b"),
            Ok(("", vec!("a", "b")))
        );
        assert_eq!(
            var_path("a.b_2 "),
            Ok((" ", vec!("a", "b_2")))
        );
    }

    #[test]
    fn test_subst() {
        assert_eq!(
            subst(""),
            Err(Err::Error(error_position!("", ErrorKind::Tag)))
        );
        assert_eq!(
            subst("${}"),
            Err(Err::Error(error_position!("${}", ErrorKind::Tag)))
        );
        assert_eq!(
            subst("${{}}"),
            Ok(("", TemplatePart::Expr("".to_string())))
            // Err(Err::Error(error_position!("", ErrorKind::Tag)))
        );
        assert_eq!(
            subst("${{a}}"),
            Ok(("", TemplatePart::Expr("a".to_string())))
        );
        assert_eq!(
            subst("${{a.b}} "),
            Ok((" ", TemplatePart::Expr("a.b".to_string())))
        );
        assert_eq!(
            subst("${{  a[0].b  }} "),
            Ok((" ", TemplatePart::Expr("a[0].b".to_string())))
        );
    }

    #[test]
    fn test_gap() {
        assert_eq!(
            gap(""),
            Err(Err::Error(error_position!("", ErrorKind::Verify)))
        );
        assert_eq!(
            gap("123 $ "),
            Ok(("$ ", TemplatePart::Gap("123 ".to_string())))
        );
        assert_eq!(
            gap("123 \\${{}} \\\\${{}}"),
            Ok(("${{}}", TemplatePart::Gap("123 \\${{}} \\\\".to_string())))
        );
    }

    #[test]
    fn test_template() {
        assert_eq!(
            template(""),
            Ok(("", vec!()))
        );
        assert_eq!(
            template("abc"),
            Ok(("", vec![TemplatePart::Gap("abc".to_string())]))
        );
        assert_eq!(
            template("${{abc}}"),
            Ok(("", vec![TemplatePart::Expr("abc".to_string())]))
        );
        assert_eq!(
            template("\\$${{abc}}: ${{x.y.0}}"),
            Ok(("", vec![
                TemplatePart::Gap("\\$".to_string()),
                TemplatePart::Expr("abc".to_string()),
                TemplatePart::Gap(": ".to_string()),
                TemplatePart::Expr("x.y.0".to_string()),
            ]))
        );
        assert_eq!(
            template("$"),
            Err(Err::Error(error_position!("$", ErrorKind::Eof)))
        );
        assert_eq!(
            template("${{"),
            Err(Err::Error(error_position!("${{", ErrorKind::Eof)))
        );
    }
}