use combine::{between, eof, one_of, many, many1, not_followed_by, satisfy, sep_by1, token};
use combine::parser::char::{alpha_num, string};
use combine::parser::combinator::recognize;
use combine::parser::range::take_while1;
use combine::parser::repeat::escaped;
use combine::{ParseError, Parser, Stream};

#[derive(Debug, PartialEq)]
pub enum TemplatePart {
    Gap(String),
    Subst(Vec<String>),
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
    use combine::error::StringStreamError;

    use super::{TemplatePart, var_name, var_path, subst, gap, template};

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