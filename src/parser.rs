use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{escaped, is_not, tag};
use nom::character::complete::{alphanumeric1, multispace0, one_of};
use nom::combinator::{eof, map, recognize, verify};
use nom::multi::{many0, many1_count, separated_list1};
use nom::sequence::{delimited, terminated};

#[derive(Debug, PartialEq)]
pub enum TemplatePart {
    Gap(String),
    Subst(Vec<String>),
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

fn subst(input: &str) -> IResult<&str, TemplatePart> {
    map(
        delimited(
            tag("${{"),
            delimited(multispace0, var_path, multispace0),
            tag("}}")
        ),
        |p| TemplatePart::Subst(p.iter().map(|&n| n.to_string()).collect())
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

    use super::{gap, subst, template, TemplatePart, var_name, var_path};

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
            Err(Err::Error(error_position!("}}", ErrorKind::Many1Count)))
        );
        assert_eq!(
            subst("${{a}}"),
            Ok(("", TemplatePart::Subst(vec!("a".to_string()))))
        );
        assert_eq!(
            subst("${{a.b}} "),
            Ok((" ", TemplatePart::Subst(vec!("a".to_string(), "b".to_string()))))
        );
        assert_eq!(
            subst("${{  a.b  }} "),
            Ok((" ", TemplatePart::Subst(vec!("a".to_string(), "b".to_string()))))
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
            Ok(("", vec![TemplatePart::Subst(vec!["abc".to_string()])]))
        );
        assert_eq!(
            template("\\$${{abc}}: ${{x.y.0}}"),
            Ok(("", vec![
                TemplatePart::Gap("\\$".to_string()),
                TemplatePart::Subst(vec!["abc".to_string()]),
                TemplatePart::Gap(": ".to_string()),
                TemplatePart::Subst(vec!["x".to_string(), "y".to_string(), "0".to_string()]),
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