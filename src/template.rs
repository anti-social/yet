use std::collections::BTreeMap;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use failure::{Error, format_err};
use failure_derive::Fail;

use quire::Pos;
use quire::ast::Ast;
use quire::ast::NullKind;
use quire::ast::ScalarKind;
use quire::ast::Tag;

use serde_json::{json, Value};

use super::parser::template;
use crate::parser::TemplatePart;

#[derive(Debug, Fail)]
enum TemplatingError {
    #[fail(display = "template parsing error: {}", err)]
    ParseError { err: String }
}

//impl<Item, Range, Position> From<combine::ParseError<Item, Range, Posigion>> for TemplatingError {
//    fn from(err: combine::ParseError<Item, Range, Posigion>) -> TemplatingError {
//        TemplatingError::ParseError
//    }
//}

fn clone_tag(tag: &Tag) -> Tag {
    match tag {
        Tag::NonSpecific => Tag::NonSpecific,
        Tag::LocalTag(name) => Tag::LocalTag(name.clone()),
        Tag::GlobalTag(name) => Tag::GlobalTag(name.clone()),
    }
}

fn clone_scalar_kind(kind: &ScalarKind) -> ScalarKind {
    match kind {
        ScalarKind::Plain => ScalarKind::Plain,
        ScalarKind::Quoted => ScalarKind::Quoted,
    }
}

fn clone_null_kind(kind: &NullKind) -> NullKind {
    match kind {
        NullKind::Explicit => NullKind::Explicit,
        NullKind::Implicit => NullKind::Implicit,
    }
}

fn clone_ast(ast: &Ast) -> Ast {
    match ast {
        Ast::Map(pos, tag, map) => {
            let cloned_map = map.iter()
                .map(|(k, v)| (k.clone(), clone_ast(v)))
                .collect::<BTreeMap<_, _>>();
            Ast::Map(pos.clone(), clone_tag(tag), cloned_map)
        }
        Ast::Seq(pos, tag, seq) => {
            let cloned_seq = seq.iter()
                .map(|v| clone_ast(v))
                .collect::<Vec<_>>();
            Ast::Seq(pos.clone(), clone_tag(tag), cloned_seq)
        }
        Ast::Scalar(pos, tag, kind, val) => {
            Ast::Scalar(pos.clone(), clone_tag(tag), clone_scalar_kind(kind), val.clone())
        }
        Ast::Null(pos, tag, kind) => {
            Ast::Null(pos.clone(), clone_tag(tag), clone_null_kind(kind))
        }
    }
}

pub fn parse(fname: &str) -> Result<Vec<Ast>, quire::Error> {
    let ref mut content = String::new();
    File::open(fname).unwrap().read_to_string(content);

    let errors = quire::ErrorCollector::new();
    quire::raw_parse_all(Rc::new(fname.to_string()), content, |doc| {
        quire::ast::process(&quire::Options::default(), doc, &errors)
    })
}

pub struct RenderContext<'a> {
    values: &'a Ast,
    // env: HashMap<String, String>,
    // anchors: HashMap<String, Ast>,
}

impl<'a> RenderContext<'a> {
    pub fn new(values: &'a Ast) -> RenderContext {
        RenderContext {
            values,
        }
    }

    fn resolve_value(&self, var_path: &Vec<String>) -> Result<Ast, Error> {
        let scope_and_path = var_path.split_first();
        match scope_and_path {
            Some((scope, path)) if scope == "values" => {
                let mut cur_node = self.values;
                for p in path {
                    let map = match cur_node {
                        Ast::Map(pos, tag, map) => map,
                        _ => return Err(format_err!("Expected a mapping")),
                    };
                    let v = match map.get(p) {
                        Some(v) => v,
                        None => return Err(format_err!("Missing key: {}", p)),
                    };
                    cur_node = v;
                }
                Ok(clone_ast(cur_node))
            },
            Some((s, _)) => return Err(format_err!("Unknown scope: {}", s)),
            None => return Err(format_err!("Empty variable path")),
        }
    }
}

pub fn render(ast: &Ast, ctx: &RenderContext) -> Result<Ast, Error> {
    let rendered_ast = match ast {
        Ast::Map(pos, tag, map) => {
            let mut rendered_map = BTreeMap::new();
            for (k, v) in map {
                rendered_map.insert(render_template(k, ctx)?, render(v, ctx)?);
            }
            Ast::Map(pos.clone(), clone_tag(tag), rendered_map)
        }
        Ast::Seq(pos, tag, seq) => {
            let mut rendered_seq = Vec::with_capacity(seq.len());
            for a in seq {
                let b =
                rendered_seq.push(render(a, ctx)?);
            }
            Ast::Seq(pos.clone(), clone_tag(tag), rendered_seq)
        }
        Ast::Scalar(pos, tag, kind, val) => {
            let rendered_value = render_template(val, ctx)?;
            Ast::Scalar(pos.clone(), clone_tag(tag), clone_scalar_kind(kind), rendered_value)
        }
        Ast::Null(pos, tag, kind) => {
            Ast::Null(pos.clone(), Tag::NonSpecific, NullKind::Explicit)
        }
    };
    Ok(rendered_ast)
}

fn render_template(tmpl: &str, ctx: &RenderContext) -> Result<String, failure::Error> {
//    dbg!(tmpl);
    use combine::Parser;

    let parse_res = template().parse(tmpl)
        .map_err(|e| TemplatingError::ParseError {err: format!("{}", e)})?;
    let template_parts = match parse_res {
        (_, rest) if rest.len() > 0 => {
            return Err(format_err!("Non empty parse"));
        }
        (template_parts, _) => {
            template_parts
        }
    };

    let mut result = String::new();
    for p in &template_parts {
        match p {
            TemplatePart::Gap(gap) => result.push_str(gap),
            TemplatePart::Subst(var_path) => {
//                dbg!(var_path);
                match ctx.resolve_value(var_path)? {
                    Ast::Scalar(_, _, _, v) => {
//                        dbg!(&v);
                        result.push_str(&v)
                    },
                    _ => return Err(format_err!("Can render only scalar value")),
                }
            }
        }
    }

    Ok(result)
}
