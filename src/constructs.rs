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
                    "Unpacking is not supported for *Each items field"
                )),
            },
            None => return Err(format_err!(
                "`items` is required field for *Each"
            )),
        };
        let body = match map.get("loop") {
            Some(loop_ast) => loop_ast,
            None => return Err(format_err!(
                "`loop` is required field for *Each"
            )),
        };

        Ok(Each {
            items,
            bind: "item".to_string(),
            body,
        })
    }
}