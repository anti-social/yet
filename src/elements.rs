use serde_yaml::{Mapping, Sequence, Value};
use serde_yaml::value::{Tag, TaggedValue};

use crate::template::{RenderContext, TemplatingError};
use crate::eval::EvalContext;

pub(crate) trait Element {
    fn render(&self, ctx: &RenderContext) -> Result<Value, TemplatingError>;
}

// pub(crate) trait FromYamlValue<'a> {
//     type Elem: Element + 'a;
//
//     fn from_value2(value: &'a Value) -> Result<Self::Elem, TemplatingError>
//         // where <Self as FromYamlValue>::Elem: 'v
//     ;
//
//     fn from_value_boxed2(value: &'a Value) -> Result<Box<dyn Element + 'a>, TemplatingError>
//         // where <Self as FromYamlValue>::Elem: 'v
//     {
//         let elem = Self::from_value2(value)?;
//         Ok(Box::new(elem) as Box<dyn Element + 'a>)
//     }
// }
// impl<'a> FromYamlValue<'a> for If<'a> {
//     type Elem = If<'a>;
//
//     fn from_value2(value: &'a Value) -> Result<Self::Elem, TemplatingError> {
//         Ok(Self { condition_tmpl: "true", then: value, else_: value })
//     }
//     // fn from_value2(value: &'a Value) -> Result<Self::Elem, TemplatingError> {
//     //     Ok(Self::Elem { condition_tmpl: "true", then: value, else_: value })
//     // }
//
// }
//
// pub(crate) trait ElementCtor<'a> {
//     type Elem: Element + 'a;
//
//     fn name(&self) -> &'static str;
//
//     fn from_value(&self, value: &'a Value) -> Result<Self::Elem, TemplatingError>;
//
//     fn from_value_boxed(&self, value: &'a Value) -> Result<Box<dyn Element + 'a>, TemplatingError>
//     // where <Self as FromYamlValue>::Elem: 'v
//     {
//         let elem = self.from_value(value)?;
//         Ok(Box::new(elem) as Box<dyn Element + 'a>)
//     }
// }
//
// pub(crate) struct LetCtor;
//
// impl<'a> ElementCtor<'a> for LetCtor {
//     type Elem = Let<'a>;
//
//     fn name(&self) -> &'static str { "*Let" }
//
//     fn from_value(&self, value: &'a Value) -> Result<Self::Elem, TemplatingError> {
//         todo!()
//     }
// }

pub(crate) struct Let<'a> {
    pub scope: &'a Mapping,
    pub in_block: &'a Value,
}

impl<'a> Let<'a> {
    pub(crate) const NAME: &'static str = "*Let";

    pub(crate) fn from_value(value: &Value) -> Result<Let, TemplatingError> {
        use super::template::TemplatingError::*;

        let map = match value {
            Value::Mapping(map) => map,
            _ => return Err(InvalidElementType {
                elem: Self::NAME, allowed_types: &["mapping"]
            })
        };
        let scope = match map.get("scope") {
            Some(Value::Mapping(scope)) => scope,
            Some(_) => {
                return Err(InvalidFieldType {
                    elem: Self::NAME, field: "scope", allowed_types: &["mapping"]
                });
            }
            None => {
                return Err(RequiredField {
                    elem: Self::NAME, field: "scope"
                });
            }
        };

        let in_block = match map.get("in") {
            Some(value) => value,
            None => return Err(RequiredField {
                elem: Self::NAME, field: "scope"
            })
        };

        Ok(Let { scope, in_block })
    }

    pub(crate) fn from_value_boxed<'v>(value: &'v Value) -> Result<Box<dyn Element + 'v>, TemplatingError> {
        Ok(Box::new(Self::from_value(value)?))
    }
}

impl<'a> Element for Let<'a> {
    fn render(&self, ctx: &RenderContext) -> Result<Value, TemplatingError> {
        let _scopes_guard = ctx.push_scope(
            ctx.render_mapping(&self.scope)?
        );

        ctx.render(&self.in_block)
    }
}

pub(crate) struct Def<'a> {
    body: &'a Value
}

impl<'a> Def<'a> {
    pub(crate) const NAME: &'static str = "*Def";

    pub(crate) fn from_value(value: &Value) -> Result<Def, TemplatingError> {
        Ok(Def { body: value })
    }

    pub(crate) fn from_value_boxed<'v>(value: &'v Value) -> Result<Box<dyn Element + 'v>, TemplatingError> {
        Ok(Box::new(Self::from_value(value)?))
    }
}

impl<'a> Element for Def<'a> {
    fn render(&self, _ctx: &RenderContext) -> Result<Value, TemplatingError> {
        Ok(
            Value::Tagged(Box::new(
                TaggedValue {
                    tag: Tag::new(Self::NAME.to_string()),
                    value: self.body.clone()
                }
            ))
        )
    }
}

pub(crate) struct If<'a> {
    pub condition_tmpl: &'a str,
    pub then: &'a Value,
    pub else_: &'a Value,
}

impl<'a> If<'a> {
    pub(crate) const NAME: &'static str = "*If";

    pub(crate) fn from_value(value: &Value) -> Result<If, TemplatingError> {
        use super::template::TemplatingError::*;

        let map = match value {
            Value::Mapping(map) => map,
            _ => return Err(InvalidElementType {
                elem: Self::NAME, allowed_types: &["mapping"]
            })
        };
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
        Ok(If { condition_tmpl, then, else_ })
    }

    pub(crate) fn from_value_boxed<'v>(value: &'v Value) -> Result<Box<dyn Element + 'v>, TemplatingError> {
        Ok(Box::new(Self::from_value(value)?))
    }
}

impl<'a> Element for If<'a> {
    fn render(&self, ctx: &RenderContext) -> Result<Value, TemplatingError> {
        use super::template::TemplatingError::*;

        let branch = match ctx.resolve_string(&self.condition_tmpl)? {
            Value::Bool(true) => &self.then,
            Value::Bool(false) => &self.else_,
            _ => {
                return Err(InvalidResolvedFieldType {
                    elem: Self::NAME, field: "condition", allowed_types: &["bool"]
                });
            }
        };

        ctx.render(branch)
    }
}

// pub(crate) trait FromYamlValue<'a, T: ElementEx + 'a> {
//     fn from_value(value: &'a Value) -> Result<T, TemplatingError>;
//
//     fn from_value_boxed(value: &'a Value) -> Result<Box<dyn ElementEx + 'a>, TemplatingError> {
//         let elem = Self::from_value(value)?;
//         Ok(Box::new(elem) as Box<dyn ElementEx + 'a>)
//     }
// }
// // impl<'a> FromYamlValue<'a, EachEx<'a>> for EachEx<'a> {
// //     fn from_value(value: &'a Value) -> Result<EachEx<'a>, TemplatingError> {
// //         Ok(EachEx { bind: "item", body: value })
// //     }
// // }
// impl<'a> EachEx<'a> {
//     pub(crate) fn from_value(value: &Value) -> Result<EachEx, TemplatingError> {
//         Ok(EachEx { bind: "item", body: value })
//     }
//
//     pub(crate) fn from_value_boxed<'v>(value: &'v Value) -> Result<Box<dyn ElementEx + 'v>, TemplatingError> {
//         Ok(Box::new(Self::from_value(value)?))
//     }
// }
// impl<'a> ElementEx for EachEx<'a> {
//     fn render(&self, ctx: &RenderContext) -> Result<Value, TemplatingError> {
//         todo!()
//     }
// }

pub(crate) struct Each<'a> {
    pub items: EachItems<'a>,
    pub bind: &'a str,
    pub body: &'a Value,
}

pub(crate) enum EachItems<'a> {
    Template(&'a String),
    Sequence(&'a Sequence),
    Mapping(&'a Mapping),
}

impl<'a> Each<'a> {
    pub(crate) const NAME: &'static str = "*Each";

    pub(crate) fn from_value(value: &Value) -> Result<Each, TemplatingError> {
        use super::template::TemplatingError::*;

        let map = match value {
            Value::Mapping(map) => map,
            _ => return Err(InvalidElementType {
                elem: Self::NAME, allowed_types: &["mapping"]
            })
        };
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

    pub(crate) fn from_value_boxed<'v>(value: &'v Value) -> Result<Box<dyn Element + 'v>, TemplatingError> {
        Ok(Box::new(Self::from_value(value)?))
    }

    fn resolve_items(&self, ctx: &RenderContext) -> Result<Sequence, TemplatingError> {
        use super::template::TemplatingError::*;

        let resolved_items = match self.items {
            EachItems::Template(items_tmpl) => ctx.resolve_string(items_tmpl)?,
            EachItems::Sequence(items) => ctx.resolve_sequence(items)?,
            EachItems::Mapping(items) => ctx.render_mapping(items)?.into(),
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
    fn render(&self, ctx: &RenderContext) -> Result<Value, TemplatingError> {
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

    pub(crate) fn from_value(value: &Value) -> Result<EachDocument, TemplatingError> {
        Ok(EachDocument(Each::from_value(value)?))
    }
}

impl<'a> Element for EachDocument<'a> {
    fn render(&self, ctx: &RenderContext) -> Result<Value, TemplatingError> {
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
