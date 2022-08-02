use failure::format_err;

use serde_yaml::{Mapping, Value};

use crate::template::RenderContext;

pub(crate) struct Each<'a> {
    pub items: Vec<Value>,
    pub bind: String,
    pub body: &'a Value,
}

impl<'a> Each<'a> {
    pub(crate) fn from_map(map: &'a Mapping, ctx: &RenderContext)
        -> Result<Each<'a>, failure::Error>
    {
        let items = match map.get("items") {
            Some(items) => match ctx.render(items)? {
                Value::Sequence(seq) => seq,
                _ => return Err(format_err!(
                    "items of *Each must be a sequence"
                )),
            },
            None => return Err(format_err!(
                "`items` is required field for *Each"
            )),
        };
        let bind = match map.get("bind") {
            Some(bind) => match ctx.render(bind)? {
                Value::String(bind_name) => {
                    bind_name
                },
                _ => return Err(format_err!(
                    "Only scalar can be a bind name"
                )),
            },
            None => "item".to_string(),
        };
        let body = match map.get("loop") {
            Some(loop_ast) => loop_ast,
            None => return Err(format_err!(
                "`loop` is required field for *Each"
            )),
        };

        Ok(Each {
            items, bind, body
        })
    }
}
