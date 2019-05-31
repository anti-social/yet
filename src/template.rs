use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;
use std::result;

use failure::{self, Compat, format_err};
use failure_derive::Fail;

use quire::Pos;
use quire::ast::Ast;
use quire::ast::NullKind;
use quire::ast::ScalarKind;
use quire::ast::Tag;

use crate::constructs::Each;
use crate::eval::{BOOL_FALSE_VALUES, BOOL_TRUE_VALUES};
use crate::eval::{Eval, EvalOk, EvalErr};
use crate::parser::{self, TemplatePart};
//use crate::parser::{ParseSubstitutionError, TemplatePart};
use crate::util::{clone_ast, clone_null_kind, clone_scalar_kind, clone_tag};
use crate::parser::SubstExpr;
use crate::parser::Arg;

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
    ParseError { err: String },
    #[fail(display = "template evaluation error: {:?}", err)]
    EvalError { err: EvalErr },
}

pub fn parse_template(fpath: &Path) -> Result<Vec<Ast>, failure::Error> {
    let ref mut content = String::new();
    File::open(fpath).with_path(fpath)?
        .read_to_string(content).with_path(fpath)?;

    let errors = quire::ErrorCollector::new();
    Ok(
        quire::raw_parse_all(
            Rc::new(fpath.display().to_string()),
            content,
            |doc| {
                quire::ast::process(&quire::Options::default(), doc, &errors)
            }
        )
            .map_err(|e| TemplatingError::ParseError {err: format!("{}", e)})?
    )
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

pub(crate) struct RenderContext<'a> {
    pub values: Option<&'a Ast>,
    pub env: &'a HashMap<String, String>,
    // anchors: HashMap<String, Ast>,
    pub scopes_stack: RefCell<Vec<HashMap<String, Ast>>>,
}

pub(crate) struct ScopesGuard<'a>(&'a RefCell<Vec<HashMap<String, Ast>>>);

impl<'a> Drop for ScopesGuard<'a> {
    fn drop(&mut self) {
        self.0.borrow_mut().pop();
    }
}

impl<'a> RenderContext<'a> {
    pub(crate) fn new(values: Option<&'a Ast>, env: &'a HashMap<String, String>) -> RenderContext<'a> {
        RenderContext {
            values, env, scopes_stack: RefCell::new(vec!())
        }
    }

    pub(crate) fn values(&self) -> Result<&Ast, failure::Error> {
        self.values.ok_or(format_err!("Values scope is missing"))
    }

    pub(crate) fn push_scopes(&self, scopes: HashMap<String, Ast>) -> ScopesGuard {
        self.scopes_stack.borrow_mut().push(scopes);
        ScopesGuard(&self.scopes_stack)
    }

    pub(crate) fn render(&self, ast: &Ast) -> Result<Rendered, failure::Error> {
        let rendered_ast = match ast {
            Ast::Map(pos, tag, map) => {
                match tag {
                    Tag::LocalTag(t) if t == "*If" => {
                        process_if(self, map)?
                    }
                    Tag::LocalTag(t) if t == "*Each" => {
                        let each = Each::from_map(self, map)?;
                        process_each(self, each)?
                    }
                    _ => {
                        Rendered::Plain(process_map(self, map, pos, tag)?)
                    }
                }
            }
            Ast::Seq(pos, tag, seq) => {
                Rendered::Plain(process_seq(self, seq, pos, tag)?)
            }
            Ast::Scalar(pos, tag, kind, val) => {
                Rendered::Plain(process_scalar(self, val, pos, tag, kind)?)
            }
            Ast::Null(pos, tag, kind) => {
                Rendered::Plain(
                    Ast::Null(pos.clone(), clone_tag(tag), clone_null_kind(kind))
                )
            }
        };
        Ok(rendered_ast)
    }
}

fn follow_ast<'a>(ast: &'a Ast, var_path: &[String], scope: &'a str)
    -> Result<&'a Ast, failure::Error>
{
    let mut cur_node = ast;
    for (ix, p) in var_path.iter().enumerate() {
        let map = match cur_node {
            Ast::Map(_, _, map) => map,
            _ => return Err(format_err!(
                "Expected a mapping: {}.{}", scope, var_path.join(".")
            )),
        };
        cur_node = match map.get(p) {
            Some(v) => v,
            None => return Err(format_err!(
                "Missing key: {}.{}", scope, var_path[..ix+1].join(".")
            )),
        };
    }
    Ok(cur_node)
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

fn process_if(ctx: &RenderContext, data: &BTreeMap<String, Ast>)
    -> Result<Rendered, failure::Error>
{
    let resolved_ast = match data.get("condition") {
        Some(Ast::Scalar(pos, tag, kind, cond)) => {
            let rendered_cond = process_scalar(ctx, cond, pos, tag, kind)?;
            match rendered_cond {
                Ast::Scalar(_, _, ScalarKind::Plain, cond_value) => {
                    let branch_ast = if BOOL_TRUE_VALUES.contains(&cond_value.as_str()) {
                        resolve_branch(data, "then")
                    } else if BOOL_FALSE_VALUES.contains(&cond_value.as_str()) {
                        resolve_branch(data, "else")
                    } else {
                        return Err(format_err!(
                            "`!*If` condition resolved to non-boolean value: {}", &cond_value
                        ));
                    };
                    ctx.render(&branch_ast)?
                },
                _ => return Err(format_err!("`!*If` condition must be resolved into scalar")),
            }
        },
        Some(_) => return Err(format_err!("`!*If` condition must be a scalar")),
        None => return Err(format_err!("`!*If` must contain `condition` key")),
    };
    Ok(resolved_ast)
}

fn process_each(ctx: &RenderContext, each: Each)
    -> Result<Rendered, failure::Error>
{
    let mut result_ast = None;
    for item in each.items {
        let mut scopes = HashMap::new();
        scopes.insert(each.bind.clone(), item);
        let scopes_guard = ctx.push_scopes(scopes);

        match ctx.render(&each.body)? {
            Rendered::Plain(Ast::Seq(pos, _, seq)) => {
                match result_ast {
                    None => {
                        result_ast = Some(Ast::Seq(pos.clone(), Tag::NonSpecific, seq));
                    }
                    Some(Ast::Seq(_, _, ref mut result_seq)) => {
                        result_seq.extend(seq);
                    }
                    Some(_) => {
                        return Err(format_err!("Cannot merge into a list"));
                    }
                }
            },
            Rendered::Plain(Ast::Map(pos, _, map)) => {
                match result_ast {
                    None => {
                        result_ast = Some(Ast::Map(pos.clone(), Tag::NonSpecific, map));
                    }
                    Some(Ast::Map(_, _, ref mut result_map)) => {
                        result_map.extend(map);
                    }
                    Some(_) => {
                        return Err(format_err!("Cannot merge into a map"));
                    }
                }
            }
            Rendered::Unpack(_) => unimplemented!(),
            _ => return Err(format_err!("Result of a loop cannot be a scalar")),
        }
    }

    Ok(Rendered::Plain(
        result_ast.unwrap_or_else(|| {
            let pos = Pos {
                filename: Rc::new("<each>".to_string()),
                indent: 0,
                line: 1,
                line_start: true,
                line_offset: 0,
                offset: 0,
            };
            Ast::Null(pos, Tag::NonSpecific, NullKind::Implicit)
        })
    ))
}

fn process_each_document(ctx: &RenderContext, each: Each)
    -> Result<Vec<Ast>, failure::Error>
{
    let mut result_docs = vec!();
    for item in each.items {
        let mut scopes = HashMap::new();
        scopes.insert(each.bind.clone(), item);
        let scopes_guard = ctx.push_scopes(scopes);

        match ctx.render(&each.body)? {
            Rendered::Plain(Ast::Null(_, _, NullKind::Implicit)) => {
                continue;
            },
            Rendered::Plain(rendered_body) => {
                result_docs.push(rendered_body);
            },
            Rendered::Unpack(_) => return Err(format_err!(
                "Cannot unpack into a root document"
            )),
        }
    }

    Ok(result_docs)
}

fn process_map(ctx: &RenderContext, map: &BTreeMap<String, Ast>, pos: &Pos, tag: &Tag)
    -> Result<Ast, failure::Error>
{
    let mut rendered_map = BTreeMap::new();
    for (k, v) in map {
        if let Ast::Scalar(_, _, _, rendered_key) = process_scalar(
            ctx, k, pos, &Tag::NonSpecific, &ScalarKind::Quoted
        )? {
            match ctx.render(v)? {
                Rendered::Plain(rendered_value) => {
                    rendered_map.insert(rendered_key, rendered_value);
                },
                Rendered::Unpack(Ast::Map(.., m)) => {
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
        match ctx.render(a)? {
            Rendered::Plain(v) => rendered_seq.push(v),
            Rendered::Unpack(Ast::Seq(.., s)) => {
                rendered_seq.extend(s);
            },
            Rendered::Unpack(Ast::Null(..)) => {}
            _ => return Err(format_err!("Cannot merge rendered value into sequence")),
        }
    }
    Ok(Ast::Seq(pos.clone(), clone_tag(tag), rendered_seq))
}

pub(crate) enum Rendered {
    Plain(Ast),
    Unpack(Ast),
}

pub fn render(ast: &Ast, values: Option<&Ast>, env: &HashMap<String, String>)
    -> Result<Vec<Ast>, failure::Error>
{
    let ctx = RenderContext::new(values, env);
    match ast {
        Ast::Map(pos, Tag::LocalTag(tag), map) if tag == "*EachDocument" => {
            let each = Each::from_map(&ctx, map)?;
            process_each_document(&ctx, each)
        },
        _ => {
            match ctx.render(ast)? {
                Rendered::Plain(a) => Ok(vec!(a)),
                Rendered::Unpack(_) => return Err(format_err!(
                    "Cannot unpack rendered node into root"
                )),
            }
        }
    }
}

fn process_scalar(ctx: &RenderContext, tmpl: &str, pos: &Pos, tag: &Tag, kind: &ScalarKind)
    -> Result<Ast, failure::Error>
{
    let template_parts = parser::parse_template(tmpl)
        .map_err(|e| TemplatingError::ParseError {err: format!("{}", e)})?;

    // Single plain (non-quoted) substitution can be an Ast node:
    // ${{values.map-or-list}}
    match (kind, template_parts.split_first()) {
        (ScalarKind::Plain, Some((TemplatePart::Subst(expr), rest)))
        if rest.len() == 0 => {
            return Ok(expr.eval(ctx)?.into_ast());
        }
        _ => {}
    }

    let mut rendered_tmpl = String::new();
    for p in &template_parts {
        match p {
            TemplatePart::Gap(gap) => rendered_tmpl.push_str(gap),
            TemplatePart::Subst(expr) => {
                match expr.eval(ctx)?.into_ast() {
                    Ast::Scalar(_, _, _, v) => {
                        rendered_tmpl.push_str(&v);
                    },
                    Ast::Null(_, _, _) => {
                        rendered_tmpl.push_str("null");
                    },
                    ast => {
                        return Err(format_err!(
                            "Can render into string only scalar value or null"
                        ));
                    },
                }
            },
            _ => return Err(format_err!("Unsupported expression"))
        }
    }

    Ok(Ast::Scalar(
        pos.clone(),
        clone_tag(tag),
        clone_scalar_kind(kind),
        rendered_tmpl
    ))
}
