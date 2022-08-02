use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::result;

use failure::{self, format_err};
use failure_derive::Fail;

use serde::Deserialize;

use serde_yaml::{Mapping, Sequence, Value};
use serde_yaml::value::{Tag, TaggedValue};

use crate::constructs::Each;
use crate::parser::template;
use crate::parser::TemplatePart;

const VALUES_SCOPE: &str = "values";
const ENV_SCOPE: &str = "env";

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

pub fn parse_template(fpath: &Path) -> Result<Vec<Value>, failure::Error> {
    let ref mut content = String::new();
    File::open(fpath).with_path(fpath)?
        .read_to_string(content).with_path(fpath)?;

    let mut values = vec!();
    for doc_de in serde_yaml::Deserializer::from_str(&content) {
        values.push(Value::deserialize(doc_de)?);
    }

    Ok(values)
}

pub fn parse_values(fpath: &Path) -> Result<Value, failure::Error> {
    let ref mut content = String::new();
    File::open(fpath).with_path(fpath)?
        .read_to_string(content).with_path(fpath)?;

    let de = serde_yaml::Deserializer::from_str(&content);
    Ok(Value::deserialize(de)?)
}

pub(crate) struct RenderContext<'a> {
    values: Option<&'a Value>,
    env: &'a HashMap<String, String>,
    scopes_stack: RefCell<Vec<HashMap<String, Value>>>,
}

struct ScopesGuard<'a>(&'a RefCell<Vec<HashMap<String, Value>>>);

impl<'a> Drop for ScopesGuard<'a> {
    fn drop(&mut self) {
        self.0.borrow_mut().pop();
    }
}

impl<'a> RenderContext<'a> {
    fn new(values: Option<&'a Value>, env: &'a HashMap<String, String>) -> RenderContext<'a> {
        RenderContext {
            values, env, scopes_stack: RefCell::new(vec!())
        }
    }

    fn values(&self) -> Result<&Value, failure::Error> {
        self.values.ok_or(format_err!("Values scope is missing"))
    }

    fn push_scopes(&self, scopes: HashMap<String, Value>) -> ScopesGuard {
        self.scopes_stack.borrow_mut().push(scopes);
        ScopesGuard(&self.scopes_stack)
    }

    fn resolve_value(&self, var_path: &Vec<String>) -> Result<Value, failure::Error> {
        let scope_and_path = var_path.split_first();
        let value = match scope_and_path {
            Some((scope, path)) if scope == VALUES_SCOPE => {
                let var_ast = follow_ast(self.values()?, path, scope)?;
                var_ast.clone()
            },
            Some((scope, path)) if scope == ENV_SCOPE => {
                let key = path.join(".");
                match self.env.get(&key) {
                    Some(v) => {
                        Value::String(v.clone())
                    },
                    None => return Err(format_err!("Missing key: env.{}", key))
                }
            },
            Some((scope, path)) => {
                let mut found_ast = None;
                for scopes_frame in self.scopes_stack.borrow().iter().rev() {
                    match scopes_frame.get(scope) {
                        Some(scope_ast) => {
                            found_ast = Some(
                                follow_ast(scope_ast, path, scope)?.clone()
                            );
                            break;
                        }
                        None => continue
                    }
                }
                if let Some(var_ast) = found_ast {
                    var_ast
                } else {
                    return Err(format_err!("Unknown scope: {}", scope));
                }
            },
            None => return Err(format_err!("Empty variable path")),
        };
        Ok(value)
    }


    pub(crate) fn render(&self, ast: &Value) -> Result<Value, failure::Error> {
        let (value, tag) = unpack_tagged_value(ast);

        let rendered_ast = match (value, tag) {
            (Value::Mapping(map), Some(tag)) if tag == "*If" => {
                process_if(self, map)?
            }
            (Value::Mapping(map), Some(tag)) if tag == "*Each" => {
                process_each(self, Each::from_map(map, self)?)?
            }
            (Value::Mapping(map), tag) => {
                process_map(self, &map, tag)?
            }
            (Value::Sequence(seq), tag) => {
                process_seq(self, &seq, tag)?
            }
            (Value::String(v), tag) => {
                maybe_wrap_with_tag(render_template(&v, self)?, tag)
            }
            (Value::Bool(v), tag) => {
                maybe_wrap_with_tag(Value::Bool(*v), tag)
            }
            (Value::Number(v), tag) => {
                maybe_wrap_with_tag(Value::Number(v.clone()), tag)
            }
            (Value::Null, tag) => {
                maybe_wrap_with_tag(Value::Null, tag)
            }
            _ => unreachable!()
        };
        Ok(rendered_ast)
    }
}

fn unpack_tagged_value(value: &Value) -> (&Value, Option<&Tag>) {
    if let Value::Tagged(tagged_value) = value {
        (&tagged_value.value, Some(&tagged_value.tag))
    } else {
        (value, None)
    }
}

fn follow_ast<'a>(ast: &'a Value, var_path: &[String], scope: &'a str)
    -> Result<&'a Value, failure::Error>
{
    let mut cur_node = ast;
    for (ix, p) in var_path.iter().enumerate() {
        let map = match cur_node {
            Value::Mapping(map) => map,
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

fn resolve_branch(data: &Mapping, key: &str) -> Value {
    match data.get(key) {
        Some(value) => value.clone(),
        None => Value::Null,
    }
}

fn process_if(ctx: &RenderContext, data: &Mapping)
    -> Result<Value, failure::Error>
{
    let resolved_ast = match data.get("condition") {
        Some(Value::String(cond_tmpl)) => {
            match render_template(cond_tmpl, ctx)? {
                Value::Bool(true) => resolve_branch(data, "then"),
                Value::Bool(false) => resolve_branch(data, "else"),
                _ => {
                    return Err(format_err!("`!*If` condition must be resolved into scalar"))
                }
            }
        }
        Some(_) => return Err(format_err!("`!*If` condition must be a scalar")),
        None => return Err(format_err!("`!*If` must contain `condition` key")),
    };
    Ok(ctx.render(&resolved_ast)?)
}

fn process_each(ctx: &RenderContext, each: Each)
    -> Result<Value, failure::Error>
{
    let mut result_ast = None;
    for item in each.items {
        let mut scopes = HashMap::new();
        scopes.insert(each.bind.clone(), item);
        let _scopes_guard = ctx.push_scopes(scopes);

        match ctx.render(&each.body)? {
            Value::Sequence(seq) => {
                match result_ast {
                    None => {
                        result_ast = Some(Value::Sequence(seq));
                    }
                    Some(Value::Sequence(ref mut result)) => {
                        result.extend(seq);
                    }
                    Some(_) => {
                        return Err(format_err!("Cannot merge into a list"));
                    }
                }
            }
            Value::Mapping(map) => {
                match result_ast {
                    None => {
                        result_ast = Some(Value::Mapping(map));
                    }
                    Some(Value::Mapping(ref mut result)) => {
                        result.extend(map);
                    }
                    Some(_) => {
                        return Err(format_err!("Cannot merge into a map"));
                    }
                }
            }
            _ => return Err(format_err!("Result of a loop cannot be a scalar")),
        }
    }

    Ok(result_ast.unwrap_or(Value::Null))
}

fn process_each_document(ctx: &RenderContext, each: Each)
    -> Result<Vec<Value>, failure::Error>
{
    let mut result_docs = Sequence::new();
    for item in each.items {
        let mut scopes = HashMap::new();
        scopes.insert(each.bind.clone(), item);
        let _scopes_guard = ctx.push_scopes(scopes);

        result_docs.push(ctx.render(&each.body)?);
    }

    Ok(result_docs)
}

fn process_map(ctx: &RenderContext, map: &Mapping, tag: Option<&Tag>)
    -> Result<Value, failure::Error>
{
    let mut rendered_map = Mapping::new();
    for (key, value) in map {
        let rendered_key = match key {
            Value::String(key) => render_template(key, ctx)?,
            _ => key.clone(),
        };
        rendered_map.insert(rendered_key, ctx.render(value)?);
    }
    Ok(maybe_wrap_with_tag(Value::Mapping(rendered_map), tag))
}

fn process_seq(ctx: &RenderContext, seq: &Sequence, tag: Option<&Tag>)
    -> Result<Value, failure::Error>
{
    let mut rendered_seq = Sequence::with_capacity(seq.len());
    for v in seq {
        rendered_seq.push(ctx.render(v)?);
    }

    Ok(maybe_wrap_with_tag(Value::Sequence(rendered_seq), tag))
}

fn maybe_wrap_with_tag(value: Value, tag: Option<&Tag>) -> Value {
    if let Some(tag) = tag {
        Value::Tagged(Box::new(
            TaggedValue { tag: tag.clone(), value }
        ))
    } else {
        value
    }
}

pub fn render(ast: &Value, values: Option<&Value>, env: &HashMap<String, String>)
    -> Result<Vec<Value>, failure::Error>
{
    let ctx = RenderContext::new(values, env);
    match unpack_tagged_value(ast) {
        (Value::Mapping(map), Some(tag)) if tag == "*EachDocument" => {
            let each = Each::from_map(map, &ctx)?;
            process_each_document(&ctx, each)
        }
        _ => {
            Ok(vec!(ctx.render(ast)?))
        }
    }
}

fn render_template(tmpl: &str, ctx: &RenderContext)
    -> Result<Value, failure::Error>
{
    let parse_res = template(tmpl)
        .map_err(|e| TemplatingError::ParseError {err: format!("{}", e)})?;
    let template_parts = match parse_res {
        (rest, _) if rest.len() > 0 => {
            return Err(format_err!("Non empty parse"));
        }
        (_, template_parts) => {
            template_parts
        }
    };

    // Single substitution can be an Ast,
    // for example `command: ${{ values.cmd }}`
    match template_parts.split_first() {
        Some((TemplatePart::Subst(var_path), rest))
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
                    Value::String(v) => {
                        rendered_tmpl.push_str(&v);
                    }
                    Value::Bool(v) => {
                        rendered_tmpl.push_str(&v.to_string());
                    }
                    Value::Number(v) => {
                        rendered_tmpl.push_str(&v.to_string());
                    }
                    Value::Null => {
                        rendered_tmpl.push_str("null");
                    }
                    _ => {
                        return Err(format_err!(
                            "Can render into string only scalar value or null"
                        ));
                    },
                }
            }
        }
    }

    Ok(Value::String(rendered_tmpl))
}
