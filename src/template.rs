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

use crate::elements::{Each, EachDocument, Element, If};
use crate::parser::template;
use crate::parser::TemplatePart;
use crate::template::TemplatingError::Resolve;

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
pub enum TemplatingError {
    #[fail(display = "template parsing error: {}", err)]
    ParseError { err: String },
    #[fail(display = "missing field {:?} inside element {:?}", field, elem)]
    RequiredField { elem: &'static str, field: &'static str },
    #[fail(display = "invalid field type {:?} inside element {:?}, allowed types: {:?}", field, elem, allowed_types)]
    InvalidFieldType { elem: &'static str, field: &'static str, allowed_types: &'static [&'static str] },
    #[fail(display = "invalid resolved field type {:?} inside element {:?}, allowed types: {:?}", field, elem, allowed_types)]
    InvalidResolvedFieldType { elem: &'static str, field: &'static str, allowed_types: &'static [&'static str] },
    #[fail(display = "missing scope {}", 0)]
    MissingScope(String),
    #[fail(display = "undefined value {}", 0)]
    UndefinedValue(String),
    #[fail(display = "invalid value {}", 0)]
    InvalidValue(String),
    #[fail(display = "{}", 0)]
    Resolve(String),
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

pub(crate) struct ScopesGuard<'a>(&'a RefCell<Vec<HashMap<String, Value>>>);

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

    fn values(&self) -> Result<&Value, TemplatingError> {
        self.values.ok_or(TemplatingError::MissingScope("values".to_string()))
    }

    pub(crate) fn push_scopes(&self, scopes: HashMap<String, Value>) -> ScopesGuard {
        self.scopes_stack.borrow_mut().push(scopes);
        ScopesGuard(&self.scopes_stack)
    }

    fn resolve_value(&self, var_path: &Vec<String>) -> Result<Value, TemplatingError> {
        use self::TemplatingError::*;

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
                    None => return Err(UndefinedValue(format!("env.{}", key))),
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
                    return Err(MissingScope(scope.to_string()));
                }
            },
            None => return Err(InvalidValue("".to_string())),
        };
        Ok(value)
    }


    pub(crate) fn render(&self, ast: &Value) -> Result<Value, TemplatingError> {
        let (value, tag) = unpack_tagged_value(ast);

        let (resolved_value, add_tag) = match (value, tag) {
            (Value::Mapping(map), Some(tag)) if tag == If::NAME => {
                (If::from_mapping(map)?.resolve(self)?, None)
            }
            (Value::Mapping(map), Some(tag)) if tag == Each::NAME => {
                (Each::from_mapping(map)?.resolve(self)?, None)
            }
            (Value::Mapping(map), tag) => {
                (self.resolve_mapping(&map)?, tag)
            }
            (Value::Sequence(seq), tag) => {
                (self.resolve_sequence(&seq)?, tag)
            }
            (Value::String(v), tag) => {
                (self.resolve_string(&v)?, tag)
            }
            (Value::Bool(v), tag) => {
                (Value::Bool(*v), tag)
            }
            (Value::Number(v), tag) => {
                (Value::Number(v.clone()), tag)
            }
            (Value::Null, tag) => {
                (Value::Null, tag)
            }
            _ => unreachable!()
        };
        Ok(maybe_wrap_with_tag(resolved_value, add_tag))
    }

    pub(crate) fn resolve_mapping(&self, map: &Mapping) -> Result<Value, TemplatingError> {
        let mut rendered_map = Mapping::new();
        for (key, value) in map {
            let rendered_key = match key {
                Value::String(key) => self.resolve_string(key)?,
                _ => key.clone(),
            };
            rendered_map.insert(rendered_key, self.render(value)?);
        }
        Ok(Value::Mapping(rendered_map))
    }


    pub(crate) fn resolve_sequence(&self, seq: &Sequence) -> Result<Value, TemplatingError> {
        let mut rendered_seq = Sequence::with_capacity(seq.len());
        for v in seq {
            rendered_seq.push(self.render(v)?);
        }

        Ok(Value::Sequence(rendered_seq))
    }


    pub(crate) fn resolve_string(&self, tmpl: &str) -> Result<Value, TemplatingError> {
        use self::TemplatingError::*;

        let parse_res = template(tmpl)
            .map_err(|e| ParseError { err: format!("{}", e) })?;
        let template_parts = match parse_res {
            (rest, _) if rest.len() > 0 => {
                return Err(ParseError { err: format!("Non empty parse") });
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
                return Ok(self.resolve_value(var_path)?);
            }
            _ => {}
        }

        let mut rendered_tmpl = String::new();
        for p in &template_parts {
            match p {
                TemplatePart::Gap(gap) => rendered_tmpl.push_str(gap),
                TemplatePart::Subst(var_path) => {
                    match self.resolve_value(var_path)? {
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
                            return Err(Resolve(
                                format!("Can render into string only scalar value or null")
                            ));
                        },
                    }
                }
            }
        }

        Ok(Value::String(rendered_tmpl))
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
    -> Result<&'a Value, TemplatingError>
{
    use self::TemplatingError::*;

    let mut cur_node = ast;
    for (ix, p) in var_path.iter().enumerate() {
        let map = match cur_node {
            Value::Mapping(map) => map,
            _ => return Err(Resolve(
                format!("Expected a mapping: {}.{}", scope, var_path.join("."))
            )),
        };
        cur_node = match map.get(p) {
            Some(v) => v,
            None => return Err(UndefinedValue(
                format!("{}.{}", scope, var_path[..ix+1].join("."))
            )),
        };
    }
    Ok(cur_node)
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
    -> Result<Vec<Value>, TemplatingError>
{
    let ctx = RenderContext::new(values, env);
    match unpack_tagged_value(ast) {
        (Value::Mapping(map), Some(tag)) if tag == EachDocument::NAME => {
            match EachDocument::from_mapping(map)?.resolve(&ctx)? {
                Value::Sequence(seq) => Ok(seq),
                _ => unreachable!("*EachDocument must return a sequence"),
            }
        }
        _ => {
            Ok(vec!(ctx.render(ast)?))
        }
    }
}
