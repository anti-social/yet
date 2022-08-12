use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Null,
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Ident(String),
    Neg(Box<Expr>),
    GetAttr(Box<Expr>, String),
    GetItem(Box<Expr>, Box<Expr>),
}

enum ValuePath {
    Attr(String),
    Item(Box<Expr>),
}

fn ident<C, E>() -> impl Parser<C, C::Collection, Error = E> + Copy + Clone
where C: text::Character,
      E: chumsky::Error<C>,
{
    filter(|c: &C| c.to_char().is_ascii_alphabetic() || c.to_char() == '_')
        .map(Some)
        .chain::<C, Vec<_>, _>(
            filter(|c: &C| c.to_char().is_ascii_alphanumeric() || c.to_char() == '_' || c.to_char() == '-')
                .repeated(),
        )
        .collect()
}

pub(crate) fn expr_parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = ident()
        .padded();

    let expr = recursive(|expr| {
        let int = just::<_, _, Simple<char>>('-').or_not()
            .chain::<char, _, _>(text::int(10))
            .collect::<String>()
            .from_str::<i64>()
            .unwrapped()
            .labelled("int");

        let frac = just('.').chain(text::digits(10));

        let exp = just('e')
            .or(just('E'))
            .chain(just('+').or(just('-')).or_not())
            .chain(text::digits(10));

        let float = just('-').or_not()
            .chain(text::int(10))
            .chain::<char, _, _>(frac.or(exp))
            .collect::<String>()
            .from_str()
            .unwrapped()
            .labelled("float");

        let boolean = just("true")
            .to(true)
            .or(just("false").to(false))
            .labelled("boolean");

        let nil = just("null").to(())
            .labelled("null");

        let escape_str = just('\\').ignore_then(
            just('\\')
                .or(just('/'))
                .or(just('"'))
                .or(just('b').to('\x08'))
                .or(just('f').to('\x0C'))
                .or(just('n').to('\n'))
                .or(just('r').to('\r'))
                .or(just('t').to('\t'))
                .or(just('u').ignore_then(
                    filter(|c: &char| c.is_digit(16))
                        .repeated()
                        .exactly(4)
                        .collect::<String>()
                        .validate(|digits, span, emit| {
                            char::from_u32(u32::from_str_radix(&digits, 16).unwrap())
                                .unwrap_or_else(|| {
                                    emit(Simple::custom(span, "invalid unicode character"));
                                    '\u{FFFD}' // unicode replacement character
                                })
                        }),
                )),
        );

        let string = just('"')
            .ignore_then(
                filter(|c| *c != '\\' && *c != '"')
                    .or(escape_str)
                    .repeated()
            )
            .then_ignore(just('"'))
            .collect::<String>()
            .labelled("string");

        let atom = float.map(Expr::Float)
            .or(int.map(Expr::Int))
            .or(boolean.map(Expr::Bool))
            .or(nil.map(|_| Expr::Null))
            .or(string.map(Expr::Str));

        let value = ident.map(Expr::Ident);

        let value_path = just('.')
            .ignore_then(ident)
            .map(ValuePath::Attr)
            .labelled("getattr")
            .or(
                expr
                    .padded()
                    .delimited_by(just('['), just(']'))
                    .map(|v| ValuePath::Item(Box::new(v)))
                    .labelled("getitem")
            );
        let chain = value
            .then(value_path.repeated())
            .foldl(|lhs, tail| {
                match tail {
                    ValuePath::Attr(name) => Expr::GetAttr(Box::new(lhs), name),
                    ValuePath::Item(index) => Expr::GetItem(Box::new(lhs), index),
                }
            });

        atom.or(chain)
    });

    expr.padded()
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Template {
    Text(String),
    Expr(Expr),
}

pub(crate) fn template_parser() -> impl Parser<char, Vec<Template>, Error = Simple<char>> {
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('$'))
    );

    let text = filter(|c: &char| *c != '\\' && *c != '$')
        .or(escape)
        .repeated()
        .at_least(1)
        .collect::<String>();

    let expr = just("${{")
        .ignore_then(expr_parser())
        .then_ignore(take_until(just("}}")))
        .labelled("expr");

    text
        .map(Template::Text)
        .or(expr.map(Template::Expr))
        .repeated()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;
    use chumsky::primitive::end;

    use crate::expr::{expr_parser, template_parser};

    #[test]
    fn test_expr_parser() {
        use super::Expr::*;

        let parser = expr_parser().then_ignore(end());

        assert_eq!(
            parser.parse("0"),
            Ok(Int(0))
        );
        assert_eq!(
            parser.parse("42"),
            Ok(Int(42))
        );
        assert_eq!(
            parser.parse("-42"),
            Ok(Int(-42))
        );

        assert_eq!(
            parser.parse("\"42\""),
            Ok(Str("42".to_string()))
        );
        assert_eq!(
            parser.parse(" \"42\" "),
            Ok(Str("42".to_string()))
        );

        assert_eq!(
            parser.parse("0.0"),
            Ok(Float(0f64))
        );

        assert_eq!(
            parser.parse("a"),
            Ok(Ident("a".to_string()))
        );
        assert_eq!(
            parser.parse(" a "),
            Ok(Ident("a".to_string()))
        );

        assert_eq!(
            parser.parse("a.b"),
            Ok(GetAttr(Ident("a".to_string()).into(), "b".to_string()))
        );
        assert_eq!(
            parser.parse("a.b.c"),
            Ok(
                GetAttr(
                    GetAttr(
                        Ident("a".to_string()).into(),
                        "b".to_string()
                    ).into(),
                    "c".to_string()
                )
            )
        );
        assert_eq!(
            parser.parse("a-b.c-d"),
            Ok(GetAttr(Ident("a-b".to_string()).into(), "c-d".to_string()))
        );

        assert_eq!(
            parser.parse("a[0]"),
            Ok(GetItem(Ident("a".to_string()).into(), Int(0).into()))
        );
        assert_eq!(
            parser.parse("a[0][1]"),
            Ok(
                GetItem(
                    GetItem(
                        Ident("a".to_string()).into(),
                        Int(0).into()
                ).into(),
                    Int(1).into()
                )
            )
        );
        assert_eq!(
            parser.parse("a[b[0]]"),
            Ok(
                GetItem(
                    Ident("a".to_string()).into(),
                    GetItem(Ident("b".to_string()).into(), Int(0).into()).into()
                )
            )
        );
        assert_eq!(
            parser.parse("a [ b[0] ]"),
            Ok(
                GetItem(
                    Ident("a".to_string()).into(),
                    GetItem(Ident("b".to_string()).into(), Int(0).into()).into()
                )
            )
        );
        assert_eq!(
            parser.parse("a[-1]"),
            Ok(GetItem(Ident("a".to_string()).into(), Int(-1).into()))
        );

        assert_eq!(
            parser.parse("a.b[ c[0] ]"),
            Ok(
                GetItem(
                    GetAttr(
                        Ident("a".to_string()).into(),
                        "b".to_string()
                    ).into(),
                    GetItem(Ident("c".to_string()).into(), Int(0).into()).into()
                )
            )
        );

        assert_eq!(
            parser.parse("a[0].b"),
            Ok(
                GetAttr(
                    GetItem(
                        Ident("a".to_string()).into(),
                        Int(0).into()
                    ).into(),
                    "b".to_string()
                )
            )
        );
        assert_eq!(
            parser.parse("a[0].b[1]"),
            Ok(
                GetItem(
                    GetAttr(
                        GetItem(
                            Ident("a".to_string()).into(),
                            Int(0).into()
                        ).into(),
                        "b".to_string()
                    ).into(),
                    Int(1).into()
                )
            )
        );
    }

    #[test]
    fn test_template_parser() {
        use super::Template::*;
        use super::Expr;

        let parser = template_parser();

        assert_eq!(
            parser.parse("Asdf"),
            Ok(vec![Text("Asdf".to_string())])
        );
        assert_eq!(
            parser.parse("${{ asdf }}"),
            Ok(vec![Expr(Expr::Ident("asdf".to_string()))])
        );
        assert_eq!(
            parser.parse("${{ 1 }}${{ 2 }}"),
            Ok(vec![Expr(Expr::Int(1)), Expr(Expr::Int(2))])
        );
        assert_eq!(
            parser.parse("Hello ${{ asdf }} world!"),
            Ok(vec![
                Text("Hello ".to_string()),
                Expr(Expr::Ident("asdf".to_string())),
                Text(" world!".to_string())
            ])
        );
        assert_eq!(
            parser.parse("Hello ${{ \"${{ asdf }}\" }} world! \\${{ asdf }}"),
            Ok(vec![
                Text("Hello ".to_string()),
                Expr(Expr::Str("${{ asdf }}".to_string())),
                Text(" world! ${{ asdf }}".to_string())
            ])
        );
    }
}
