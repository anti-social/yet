use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;

use failure::format_err;
use failure_derive::Fail;

use quire::Pos;
use quire::ast::Ast;
use quire::ast::NullKind;
use quire::ast::ScalarKind;
use quire::ast::Tag;

use super::parser::template;
use crate::parser::TemplatePart;

#[derive(Debug, Fail)]
enum TemplatingError {
    #[fail(display = "template parsing error: {}", err)]
    ParseError { err: String }
}

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

pub fn parse_template(fpath: &Path) -> Result<Vec<Ast>, failure::Error> {
    let ref mut content = String::new();
    File::open(fpath)?.read_to_string(content)?;

    let errors = quire::ErrorCollector::new();
    let fname = fpath.as_os_str().to_string_lossy().into_owned();
    Ok(quire::raw_parse_all(Rc::new(fname), content, |doc| {
        quire::ast::process(&quire::Options::default(), doc, &errors)
    })
        .map_err(|e| TemplatingError::ParseError {err: format!("{}", e)})?)
}

pub fn parse_values(fpath: &Path) -> Result<Ast, failure::Error> {
    let ref mut content = String::new();
    File::open(fpath)?.read_to_string(content)?;

    let errors = quire::ErrorCollector::new();
    let fname = fpath.as_os_str().to_string_lossy().into_owned();
    Ok(quire::raw_parse(Rc::new(fname), content, |doc| {
        quire::ast::process(&quire::Options::default(), doc, &errors)
    })
        .map_err(|e| TemplatingError::ParseError {err: format!("{}", e)})?)
}

struct TemplateScalar<'a> {
    pub tmpl: &'a str,
    pub pos: &'a Pos,
    pub tag: &'a Tag,
    pub kind: &'a ScalarKind,
}

impl<'a> TemplateScalar<'a> {
    fn new(tmpl: &'a str, pos: &'a Pos, tag: &'a Tag, kind: &'a ScalarKind)
        -> TemplateScalar<'a>
    {
        TemplateScalar {tmpl, pos, tag, kind}
    }
}

pub struct RenderContext<'a> {
    values: Option<&'a Ast>,
    env: &'a HashMap<String, String>,
    // anchors: HashMap<String, Ast>,
}

impl<'a> RenderContext<'a> {
    pub fn new(values: Option<&'a Ast>, env: &'a HashMap<String, String>) -> RenderContext<'a> {
        RenderContext {
            values, env
        }
    }

    fn resolve_value(&self, var_path: &Vec<String>) -> Result<Ast, failure::Error> {
        let scope_and_path = var_path.split_first();
        let value = match scope_and_path {
            Some((scope, path)) if scope == "values" => {
                let mut cur_node = self.values.ok_or(format_err!("Values scope is missing"))?;
                for p in path {
                    let map = match cur_node {
                        Ast::Map(_, _, map) => map,
                        _ => return Err(format_err!("Expected a mapping")),
                    };
                    let v = match map.get(p) {
                        Some(v) => v,
                        None => return Err(format_err!("Missing key in values: {}", p)),
                    };
                    cur_node = v;
                }
                clone_ast(cur_node)
            },
            Some((scope, path)) if scope == "env" => {
                let key = path.join(".");
                match self.env.get(&key) {
                    Some(v) => {
                        let pos = Pos {
                            filename: Rc::new("<env>".to_string()),
                            indent: 0,
                            line: 1,
                            line_start: true,
                            line_offset: 0,
                            offset: 0,
                        };
                        Ast::Scalar(pos, Tag::NonSpecific, ScalarKind::Quoted, v.clone())
                    },
                    None => return Err(format_err!("Missing key in env: {}", key))
                }
            },
            Some((s, _)) => return Err(format_err!("Unknown scope: {}", s)),
            None => return Err(format_err!("Empty variable path")),
        };
        Ok(value)
    }
}

pub fn render(ast: &Ast, ctx: &RenderContext) -> Result<Ast, failure::Error> {
    let rendered_ast = match ast {
        Ast::Map(pos, tag, map) => {
            let mut rendered_map = BTreeMap::new();
            for (k, v) in map {
                let tmpl = TemplateScalar::new(
                    k, pos, &Tag::NonSpecific, &ScalarKind::Quoted
                );
                if let Ast::Scalar(_, _, _, rendered_key) = render_template(&tmpl, ctx)? {
                    rendered_map.insert(
                        rendered_key,
                        render(v, ctx)?
                    );
                } else {
                    return Err(format_err!("Only scalar type can be a map key"))
                }
            }
            Ast::Map(pos.clone(), clone_tag(tag), rendered_map)
        }
        Ast::Seq(pos, tag, seq) => {
            let mut rendered_seq = Vec::with_capacity(seq.len());
            for a in seq {
                rendered_seq.push(render(a, ctx)?);
            }
            Ast::Seq(pos.clone(), clone_tag(tag), rendered_seq)
        }
        Ast::Scalar(pos, tag, kind, val) => {
            let tmpl = TemplateScalar::new(val, pos, tag, kind);
            render_template(&tmpl, ctx)?
        }
        Ast::Null(pos, tag, kind) => {
            Ast::Null(pos.clone(), clone_tag(tag), clone_null_kind(kind))
        }
    };
    Ok(rendered_ast)
}

fn render_template(tmpl: &TemplateScalar, ctx: &RenderContext)
    -> Result<Ast, failure::Error>
{
    use combine::Parser;

    let parse_res = template().parse(tmpl.tmpl)
        .map_err(|e| TemplatingError::ParseError {err: format!("{}", e)})?;
    let template_parts = match parse_res {
        (_, rest) if rest.len() > 0 => {
            return Err(format_err!("Non empty parse"));
        }
        (template_parts, _) => {
            template_parts
        }
    };

    // Single plain (non-quoted) substitution can be an Ast
    // command: ${{values.cmd}}
    match (tmpl.kind, template_parts.split_first()) {
        (ScalarKind::Plain, Some((TemplatePart::Subst(var_path), rest)))
        if rest.len() == 0 => {
            return Ok(ctx.resolve_value(var_path)?);
        }
        _ => {}
    }

    let mut rendered_tmpl = String::new();
    for p in &template_parts {
        match p {
            TemplatePart::Gap(gap) => rendered_tmpl.push_str(gap),
            TemplatePart::Subst(var_path) => {
                match ctx.resolve_value(var_path)? {
                    Ast::Scalar(_, _, _, v) => {
                        rendered_tmpl.push_str(&v);
                    },
                    Ast::Null(_, _, _) => {
                        rendered_tmpl.push_str("null");
                    },
                    _ => {
                        return Err(format_err!(
                            "Can render into string only scalar value or null"
                        ));
                    },
                }
            }
        }
    }

    Ok(Ast::Scalar(
        tmpl.pos.clone(),
        clone_tag(tmpl.tag),
        clone_scalar_kind(tmpl.kind),
        rendered_tmpl
    ))
}
