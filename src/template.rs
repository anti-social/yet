use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;
use std::result;

use failure::{self, format_err};
use failure_derive::Fail;

use quire::Pos;
use quire::ast::Ast;
use quire::ast::NullKind;
use quire::ast::ScalarKind;
use quire::ast::Tag;

use super::parser::template;
use crate::parser::TemplatePart;

static BOOL_TRUE_VALUES: &[&str] = &[
    "true", "TRUE", "True",
    "y", "Y", "yes", "YES", "Yes",
    "on", "ON", "On",
];

static BOOL_FALSE_VALUES: &[&str] = &[
    "false", "FALSE", "False",
    "n", "N", "no", "NO", "No",
    "off", "OFF", "Off",
];

pub trait WithPathResultExt<T, E>: failure::ResultExt<T, E> where E: fmt::Display {
    fn with_path<P: AsRef<Path>>(self, path: P)
        -> result::Result<T, failure::Context<String>>
        where Self: Sized
    {
        self.with_context(|e| format!("{}: {}", e, path.as_ref().display()))
    }
}

impl<T, E: fmt::Display> WithPathResultExt<T, E> for result::Result<T, E>
    where result::Result<T, E>: failure::ResultExt<T, E> {}

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
    File::open(fpath).with_path(fpath)?
        .read_to_string(content).with_path(fpath)?;

    let errors = quire::ErrorCollector::new();
    Ok(quire::raw_parse_all(Rc::new(fpath.display().to_string()), content, |doc| {
        quire::ast::process(&quire::Options::default(), doc, &errors)
    })
        .map_err(|e| TemplatingError::ParseError {err: format!("{}", e)})?)
}

pub fn parse_values(fpath: &Path) -> Result<Ast, failure::Error> {
    let ref mut content = String::new();
    File::open(fpath).with_path(fpath)?
        .read_to_string(content).with_path(fpath)?;

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

fn resolve_branch(data: &BTreeMap<String, Ast>, key: &str) -> Ast {
    let pos = Pos {
        filename: Rc::new("<expr>".to_string()),
        indent: 0,
        line: 1,
        line_start: true,
        line_offset: 0,
        offset: 0,
    };
    match data.get(key) {
        Some(value) => clone_ast(value),
        None => Ast::Null(
            pos, Tag::NonSpecific, NullKind::Implicit
        ),
    }
}

fn process_if(ctx: &RenderContext, data: &BTreeMap<String, Ast>) -> Result<Ast, failure::Error> {
    let resolved_ast = match data.get("condition") {
        Some(Ast::Scalar(pos, tag, kind, cond)) => {
            dbg!(cond);
            let rendered_cond = render_template(
                &TemplateScalar::new(cond, pos, tag, kind), ctx
            )?;
            match rendered_cond {
                Ast::Scalar(_, _, _, cond_value) => {
                    dbg!(&cond_value);
                    if BOOL_TRUE_VALUES.contains(&cond_value.as_str()) {
                        resolve_branch(data, "then")
                    } else if BOOL_FALSE_VALUES.contains(&cond_value.as_str()) {
                        dbg!(resolve_branch(data, "else"))
                    } else {
                        return Err(format_err!(
                            "`!*If` condition resolved to non-boolean value: {}", &cond_value
                        ));
                    }
                },
                _ => return Err(format_err!("`!*If` condition must be resolved into scalar")),
            }
        },
        Some(_) => return Err(format_err!("`!*If` condition must be a scalar")),
        None => return Err(format_err!("`!*If` must contain `condition` key")),
    };
    dbg!(&resolved_ast);
    Ok(resolved_ast)
}

fn process_map(ctx: &RenderContext, map: &BTreeMap<String, Ast>, pos: &Pos, tag: &Tag)
    -> Result<Ast, failure::Error>
{
    let mut rendered_map = BTreeMap::new();
    for (k, v) in map {
        let tmpl = TemplateScalar::new(
            k, pos, &Tag::NonSpecific, &ScalarKind::Quoted
        );
        if let Ast::Scalar(_, _, _, rendered_key) = render_template(&tmpl, ctx)? {
            match render_with_merge(v, ctx)? {
                Rendered::Plain(rendered_value) => {
                    rendered_map.insert(rendered_key, rendered_value);
                },
                Rendered::Merge(Ast::Map(.., m)) => {
                    rendered_map.extend(m);
                },
                _ => return Err(format_err!(
                                    "Cannot merge rendered value into map"
                                )),
            };
        } else {
            return Err(format_err!("Only scalar type can be a map key"))
        }
    }
    Ok(Ast::Map(pos.clone(), clone_tag(tag), rendered_map))
}

fn process_seq(ctx: &RenderContext, seq: &Vec<Ast>, pos: &Pos, tag: &Tag)
    -> Result<Ast, failure::Error>
{
    let mut rendered_seq = Vec::with_capacity(seq.len());
    for a in seq {
        match render_with_merge(a, ctx)? {
            Rendered::Plain(v) => rendered_seq.push(v),
            Rendered::Merge(Ast::Seq(.., s)) => {
                rendered_seq.extend(s);
            },
            Rendered::Merge(Ast::Null(..)) => {}
            _ => return Err(format_err!("Cannot merge rendered value into sequence")),
        }
    }
    Ok(Ast::Seq(pos.clone(), clone_tag(tag), rendered_seq))
}

enum Rendered {
    Plain(Ast),
    Merge(Ast),
}

pub fn render(ast: &Ast, ctx: &RenderContext) -> Result<Ast, failure::Error> {
    match render_with_merge(ast, ctx)? {
        Rendered::Plain(a) => Ok(a),
        Rendered::Merge(_) => return Err(format_err!(
            "Cannot merge rendered node into root"
        )),
    }
}

fn render_with_merge(ast: &Ast, ctx: &RenderContext)
    -> Result<Rendered, failure::Error>
{
    let rendered_ast = match ast {
        Ast::Map(pos, tag, map) => {
            dbg!(tag);
            match tag {
                Tag::LocalTag(t) if t == "*If" => {
                    Rendered::Merge(process_if(ctx, map)?)
                }
                _ => {
                    Rendered::Plain(process_map(ctx, map, pos, tag)?)
                }
            }
        }
        Ast::Seq(pos, tag, seq) => {
            Rendered::Plain(process_seq(ctx, seq, pos, tag)?)
        }
        Ast::Scalar(pos, tag, kind, val) => {
            let tmpl = TemplateScalar::new(val, pos, tag, kind);
            Rendered::Plain(render_template(&tmpl, ctx)?)
        }
        Ast::Null(pos, tag, kind) => {
            Rendered::Plain(
                Ast::Null(pos.clone(), clone_tag(tag), clone_null_kind(kind))
            )
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
