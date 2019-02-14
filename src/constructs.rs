use std::collections::BTreeMap;

use failure::format_err;

use quire::ast::Ast;

use crate::template::{RenderContext, Rendered};

pub(crate) struct Each<'a> {
    pub items: Vec<Ast>,
    pub bind: String,
    pub body: &'a Ast,
}

impl<'a> Each<'a> {
    pub(crate) fn from_map(ctx: &RenderContext, map: &'a BTreeMap<String, Ast>)
        -> Result<Each<'a>, failure::Error>
    {
        let items = match map.get("items") {
            Some(items) => match ctx.render(items)? {
                Rendered::Plain(rendered_items) => {
                    match rendered_items {
                        Ast::Seq(pos, tag, seq) => seq,
                        _ => return Err(format_err!(
                            "items of *Each must be a sequence"
                        )),
                    }
                },
                Rendered::Unpack(_) => return Err(format_err!(
                    "Unpacking is not supported for `items` field of `*Each`"
                )),
            },
            None => return Err(format_err!(
                "`items` is required field for *Each"
            )),
        };
        let bind = match map.get("bind") {
            Some(bind) => match ctx.render(bind)? {
                Rendered::Plain(Ast::Scalar(_, _, _, bind_name)) => {
                    bind_name
                },
                Rendered::Plain(_) => return Err(format_err!(
                    "Only scalar can be a bind name"
                )),
                Rendered::Unpack(_) => return Err(format_err!(
                    "Unpacking is not supported for `bind` field of `*Each`"
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