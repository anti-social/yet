use serde_yaml::Value;
use serde_yaml::value::{Tag, TaggedValue};

use snafu::prelude::*;
use crate::template::TemplatingError;

use super::elements::Def;
use super::expr::Expr;

#[derive(Debug, PartialEq, Snafu)]
pub enum EvalError {
    #[snafu(display("{msg}: {value}"))]
    Overflow { msg: &'static str, value: i64 },
    #[snafu(display("{msg}"))]
    Type { msg: String },
    #[snafu(display("Name '{name}' is not defined"))]
    Name { name: String },
    #[snafu(display("Missing attribute: {name}"))]
    GetAttr { name: String },
    #[snafu(display("Index error: {ix:?}"))]
    GetItem { ix: Value },
}

pub(crate) trait EvalContext {
    fn resolve(&self, name: &str) -> Result<Value, EvalError>;

    fn render(&self, value: &Value) -> Result<Value, TemplatingError>;
}

impl Expr {
    pub(crate) fn eval(&self, ctx: &impl EvalContext) -> Result<Value, EvalError> {
        use Expr::*;

        Ok(match self {
            Null => Value::Null,
            Str(v) => Value::from(v.clone()),
            Int(v) => Value::from(*v),
            Float(v) => Value::from(*v),
            Bool(v) => Value::from(*v),
            Ident(name) => {
                match maybe_unpack_tagged_value(ctx.resolve(name)?.clone()) {
                    (value, Some(tag)) if tag == Def::NAME => {
                        ctx.render(&value).unwrap()
                    }
                    (value, tag) => maybe_pack_tagged_value(value, tag)
                }
            },
            Neg(expr) => {
                if let (Value::Number(v), tag) = maybe_unpack_tagged_value(expr.eval(ctx)?) {
                    let negated = if let Some(v) = v.as_i64() {
                        if let Some(negated) = v.checked_neg() {
                            negated.into()
                        } else {
                            return OverflowSnafu { msg: "Negation overflow", value: v }.fail();
                        }
                    } else if let Some(v) = v.as_f64() {
                        Value::from(-v)
                    } else {
                        unreachable!()
                    };
                    maybe_pack_tagged_value(negated, tag)
                } else {
                    return TypeSnafu { msg: "Negation is only allowed on numbers".to_string() }.fail();
                }
            }
            GetAttr(expr, name) => {
                if let (Value::Mapping(v), tag) = maybe_unpack_tagged_value(expr.eval(ctx)?) {
                    maybe_pack_tagged_value(
                        v.get(name)
                            .context(GetAttrSnafu { name: name.to_string() })?
                            .clone(),
                        tag
                    )
                } else {
                    return TypeSnafu { msg: "Getting an attribute is only allowed on mappings".to_string() }.fail();
                }
            }
            GetItem(expr, index) => {
                let (ix, _) = maybe_unpack_tagged_value(index.eval(ctx)?);
                let (obj, tag) = maybe_unpack_tagged_value(expr.eval(ctx)?);
                let item = match obj {
                    Value::Mapping(v) => {
                        v.get(&ix)
                            .context(GetItemSnafu { ix })?
                            .clone()
                    }
                    Value::Sequence(v) => {
                        v.get(Self::value_to_sequence_index(&ix, v.len())?)
                            .context(GetItemSnafu { ix })?
                            .clone()
                    }
                    Value::String(v) => {
                        let c = v.chars().nth(Self::value_to_sequence_index(&ix, v.len())?)
                            .context(GetItemSnafu { ix })?;
                        Value::from(String::from(c))
                    }
                    _ => panic!("Not supported value for getting item operation"),
                };
                maybe_pack_tagged_value(item, tag)
            }
        })
    }

    fn value_to_sequence_index(ix: &Value, seq_len: usize) -> Result<usize, EvalError> {
        if let Value::Number(ix) = ix {
            if let Some(ix) = ix.as_u64() {
                Ok(ix as usize)
            } else if let Some(ix) = ix.as_i64() {
                // Here `ix` is always a negative integer
                let rev_ix = (-ix) as usize;
                if rev_ix > seq_len {
                    return GetItemSnafu { ix: ix.clone() }.fail();
                } else {
                    Ok(seq_len - rev_ix)
                }
            } else {
                return TypeSnafu { msg: "Index for a sequence must be an integer number".to_string() }.fail();
            }
        } else {
            return TypeSnafu { msg: "Index for a sequence must be an integer number".to_string() }.fail();
        }
    }
}

fn maybe_unpack_tagged_value(value: Value) -> (Value, Option<Tag>) {
    if let Value::Tagged(tagged_value) = value {
        (tagged_value.value, Some(tagged_value.tag))
    } else {
        (value, None)
    }
}

fn maybe_pack_tagged_value(value: Value, tag: Option<Tag>) -> Value {
    if let Some(tag) = tag {
        Value::Tagged(TaggedValue { tag, value }.into())
    } else {
        value
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use serde_yaml::{Mapping, Value};

    use crate::eval::{EvalContext, EvalError};
    use crate::expr::Expr::*;
    use crate::template::TemplatingError;

    impl EvalContext for HashMap<&str, Value> {
        fn resolve(&self, name: &str) -> Result<Value, EvalError> {
            self.get(name)
                .map(|v| v.clone())
                .ok_or(EvalError::GetAttr { name: name.to_string() })
        }

        fn render(&self, _value: &Value) -> Result<Value, TemplatingError> {
            todo!()
        }
    }

    #[test]
    fn test_negate() {
        let ctx = HashMap::<&str, Value>::new();

        assert_eq!(
            Neg(Int(42).into()).eval(&ctx),
            Ok(Value::from(-42))
        );
        assert_eq!(
            Neg(Int(-42).into()).eval(&ctx),
            Ok(Value::from(42))
        );
        assert_eq!(
            Neg(Int(i64::MAX).into()).eval(&ctx),
            Ok(Value::from(i64::MIN + 1))
        );
        assert_eq!(
            Neg(Int(i64::MIN).into()).eval(&ctx),
            Err(EvalError::Overflow { msg: "Negation overflow", value: -9223372036854775808 })
        );
    }

    #[test]
    fn test_getattr() {
        let mut ctx = HashMap::new();

        let mut a = Mapping::new();
        a.insert("b".to_string().into(), 42.into());
        ctx.insert("a", a.into());

        let mut d = Mapping::new();
        d.insert("0".into(), "test".into());
        let mut c = Mapping::new();
        c.insert("d".into(), d.into());
        ctx.insert("c", c.into());

        assert_eq!(
            GetAttr(
                Ident("a".to_string()).into(),
                "b".to_string()
            ).eval(&ctx),
            Ok(42.into())
        );
        assert_eq!(
            GetAttr(
                GetAttr(
                    Ident("c".to_string()).into(),
                    "d".to_string()
                ).into(),
                "0".to_string()
            ).eval(&ctx),
            Ok("test".into())
        );
        assert_eq!(
            GetAttr(
                Ident("a".to_string()).into(),
                "x".to_string()
            ).eval(&ctx),
            Err(EvalError::GetAttr { name: "x".to_string() })
        );
    }

    #[test]
    fn test_getitem() {
        let mut ctx = HashMap::new();
        ctx.insert(
            "v",
            Value::from(
                vec![
                    Value::from("a"),
                    Value::from(vec![Value::from(1), Value::from(2)])
                ]
            )
        );

        assert_eq!(
            GetItem(
                Ident("v".to_string()).into(),
                Int(0).into()
            ).eval(&ctx),
            Ok("a".into())
        );
        assert_eq!(
            GetItem(
                GetItem(
                    Ident("v".to_string()).into(),
                    Int(1).into()
                ).into(),
                Int(0).into()
            ).eval(&ctx),
            Ok(1.into())
        );
        assert_eq!(
            GetItem(
                Ident("v".to_string()).into(),
                Int(2).into()
            ).eval(&ctx),
            Err(EvalError::GetItem { ix: 2.into() })
        );
    }

    #[test]
    fn test_eval() {

    }
}