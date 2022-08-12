use serde_yaml::{Mapping, Sequence, Value};

use crate::template::{RenderContext, TemplatingError};

pub(crate) trait Element {
    fn resolve(&self, ctx: &RenderContext) -> Result<Value, TemplatingError>;
}

pub(crate) struct If<'a> {
    pub condition_tmpl: &'a str,
    pub then: &'a Value,
    pub else_: &'a Value,
}

impl<'a> If<'a> {
    pub(crate) const NAME: &'static str = "*If";

    pub(crate) fn from_mapping(map: &'a Mapping) -> Result<Self, TemplatingError> {
        use super::template::TemplatingError::*;

        let condition_tmpl = match map.get("condition") {
            Some(Value::String(condition)) => condition,
            Some(_) => {
                return Err(InvalidFieldType {
                    elem: Self::NAME, field: "condition", allowed_types: &["string"]
                });
            },
            None => {
                return Err(RequiredField {
                    elem: Self::NAME, field: "condition"
                });
            },
        };
        let then = map.get("then").unwrap_or(&Value::Null);
        let else_ = map.get("else").unwrap_or(&Value::Null);
        Ok(Self { condition_tmpl, then, else_ })
    }
}

impl<'a> Element for If<'a> {
    fn resolve(&self, ctx: &RenderContext) -> Result<Value, TemplatingError> {
        use super::template::TemplatingError::*;

        let branch = match ctx.resolve_string(self.condition_tmpl)? {
            Value::Bool(true) => self.then,
            Value::Bool(false) => self.else_,
            _ => {
                return Err(InvalidResolvedFieldType {
                    elem: Self::NAME, field: "condition", allowed_types: &["bool"]
                });
            }
        };
        Ok(ctx.render(branch)?)
    }
}

pub(crate) enum EachItems<'a> {
    Template(&'a String),
    Sequence(&'a Sequence),
    Mapping(&'a Mapping),
}

pub(crate) struct Each<'a> {
    pub items: EachItems<'a>,
    pub bind: &'a str,
    pub body: &'a Value,
}

impl<'a> Each<'a> {
    pub(crate) const NAME: &'static str = "*Each";

    pub(crate) fn from_mapping(map: &'a Mapping) -> Result<Self, TemplatingError> {
        use super::template::TemplatingError::*;

        let items = match map.get("items") {
            Some(Value::Sequence(items)) => EachItems::Sequence(items),
            Some(Value::Mapping(items)) => EachItems::Mapping(items),
            Some(Value::String(items_tmpl)) => EachItems::Template(items_tmpl),
            Some(_) => {
                return Err(InvalidFieldType {
                    elem: Self::NAME, field: "items", allowed_types: &["sequence", "string"]
                })
            }
            None => {
                return Err(RequiredField { elem: Self::NAME, field: "items" })
            }
        };
        let bind = match map.get("bind") {
            Some(Value::String(bind)) => bind,
            Some(_) => {
                return Err(InvalidFieldType {
                    elem: Self::NAME, field: "bind", allowed_types: &["string"]
                })
            },
            None => "item",
        };
        let body = match map.get("loop") {
            Some(body) => body,
            None => {
                return Err(RequiredField { elem: Self::NAME, field: "loop" })
            },
        };

        Ok(Each { items, bind, body })
    }

    fn resolve_items(&self, ctx: &RenderContext) -> Result<Sequence, TemplatingError> {
        use super::template::TemplatingError::*;

        let resolved_items = match self.items {
            EachItems::Template(items_tmpl) => ctx.resolve_string(items_tmpl)?,
            EachItems::Sequence(items) => ctx.resolve_sequence(items)?,
            EachItems::Mapping(items) => ctx.resolve_mapping(items)?,
        };
        match resolved_items {
            Value::Sequence(items) => Ok(items),
            Value::Mapping(items) => {
                let map_items = items.iter()
                    .map(|(k, v)| {
                        let mut item = Mapping::new();
                        item.insert(Value::from("key"), k.clone());
                        item.insert(Value::from("value"), v.clone());
                        Value::from(item)
                    })
                    .collect::<Sequence>();
                Ok(map_items)
            }
            _ => {
                return Err(InvalidResolvedFieldType {
                    elem: Self::NAME, field: "items", allowed_types: &["sequence"]
                });
            }
        }
    }
}

impl<'a> Element for Each<'a> {
    fn resolve(&self, ctx: &RenderContext) -> Result<Value, TemplatingError> {
        use super::template::TemplatingError::*;

        let resolved_items = self.resolve_items(ctx)?;

        let mut result_ast = None;
        for item in resolved_items {
            let mut scope = Mapping::new();
            scope.insert(Value::from(self.bind), item);
            let _scopes_guard = ctx.push_scope(scope);

            match ctx.render(&self.body)? {
                Value::Sequence(seq) => {
                    match result_ast {
                        None => {
                            result_ast = Some(Value::from(seq));
                        }
                        Some(Value::Sequence(ref mut result)) => {
                            result.extend(seq);
                        }
                        Some(_) => {
                            return Err(Resolve { msg: format!("Cannot merge into a list") });
                        }
                    }
                }
                Value::Mapping(map) => {
                    match result_ast {
                        None => {
                            result_ast = Some(Value::from(map));
                        }
                        Some(Value::Mapping(ref mut result)) => {
                            result.extend(map);
                        }
                        Some(_) => {
                            return Err(Resolve { msg: format!("Cannot merge into a map") });
                        }
                    }
                }
                _ => return Err(Resolve { msg: format!("Result of a loop cannot be a scalar") }),
            }
        }

        Ok(result_ast.unwrap_or(Value::Null))
    }
}

pub(crate) struct EachDocument<'a>(Each<'a>);

impl<'a> EachDocument<'a> {
    pub(crate) const NAME: &'static str = "*EachDocument";

    pub(crate) fn from_mapping(map: &'a Mapping) -> Result<Self, TemplatingError> {
        Ok(EachDocument(Each::from_mapping(map)?))
    }
}

impl<'a> Element for EachDocument<'a> {
    fn resolve(&self, ctx: &RenderContext) -> Result<Value, TemplatingError> {
        let resolved_items = self.0.resolve_items(ctx)?;

        let mut docs = Sequence::new();
        for item in resolved_items {
            let mut scope = Mapping::new();
            scope.insert(Value::from(self.0.bind), item);
            let _scopes_guard = ctx.push_scope(scope);

            docs.push(ctx.render(&self.0.body)?);
        }

        Ok(Value::from(docs))
    }
}