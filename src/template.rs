use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

use chumsky::Parser;
use chumsky::error::Simple;

use serde::Deserialize;

use serde_yaml::{Mapping, Sequence, Value};
use serde_yaml::value::{Tag, TaggedValue};

use snafu::prelude::*;

use crate::elements::{Each, EachDocument, Element, If};
use crate::eval::{EvalContext, EvalError};
use crate::expr::{Template, template_parser};

#[derive(Debug, Snafu)]
pub enum TemplatingError {
    #[snafu(display("Could not read file {path:?}"))]
    ReadFile { source: std::io::Error, path: PathBuf },
    #[snafu(display("Template parsing error: {err}"))]
    ParseError { err: String },
    #[snafu(display("Template parsing error"))]
    ParseTemplate { errors: Vec<Simple<char>> },
    #[snafu(display("{source}"))]
    Eval { #[snafu(source)] source: EvalError },
    #[snafu(display("Error when deserializing"))]
    Yaml { source: serde_yaml::Error },
    #[snafu(display("Error when serializing"))]
    Serialize { source: serde_yaml::Error },
    #[snafu(display("missing field {field:?} inside element {elem:?}"))]
    RequiredField { elem: &'static str, field: &'static str },
    #[snafu(display("invalid field type {field:?} inside element {elem:?}, allowed types: {allowed_types:?}"))]
    InvalidFieldType { elem: &'static str, field: &'static str, allowed_types: &'static [&'static str] },
    #[snafu(display("invalid resolved field type {field:?} inside element {elem:?}, allowed types: {allowed_types:?}"))]
    InvalidResolvedFieldType { elem: &'static str, field: &'static str, allowed_types: &'static [&'static str] },
    #[snafu(display("missing scope {name}"))]
    MissingScope { name: String },
    #[snafu(display("undefined value {name}"))]
    UndefinedValue { name: String },
    #[snafu(display("invalid value {name}"))]
    InvalidValue { name: String },
    #[snafu(display("{msg}"))]
    Resolve { msg: String },
}

pub fn parse_template(path: &Path) -> Result<Vec<Value>, TemplatingError> {
    let ref mut content = String::new();
    File::open(path).context(ReadFileSnafu { path })?
        .read_to_string(content).context(ReadFileSnafu { path })?;

    let mut values = vec!();
    for doc_de in serde_yaml::Deserializer::from_str(&content) {
        values.push(Value::deserialize(doc_de).context(YamlSnafu)?);
    }

    Ok(values)
}

pub fn parse_values(path: &Path) -> Result<Value, TemplatingError> {
    let ref mut content = String::new();
    File::open(path).context(ReadFileSnafu { path })?
        .read_to_string(content).context(ReadFileSnafu { path })?;

    let de = serde_yaml::Deserializer::from_str(&content);
    Ok(Value::deserialize(de).context(YamlSnafu)?)
}

pub(crate) struct RenderContext {
    stacked_scopes: RefCell<Vec<Mapping>>,
}

pub(crate) struct ScopesGuard<'a>(&'a RefCell<Vec<Mapping>>);

impl<'a> Drop for ScopesGuard<'a> {
    fn drop(&mut self) {
        self.0.borrow_mut().pop();
    }
}

impl EvalContext for RenderContext {
    fn resolve(&self, name: &str) -> Result<Value, EvalError> {
        for scope in self.stacked_scopes.borrow().iter().rev() {
            if let Some(value) = scope.get(name) {
                return Ok(value.clone());
            }
        }

        Err(EvalError::GetAttr { name: name.to_string() })
    }
}

impl RenderContext {
    fn new(values: Option<&Value>, env: &HashMap<String, String>) -> RenderContext {
        let mut root_scope = Mapping::new();
        if let Some(values) = values {
            root_scope.insert(Value::from("values"), values.clone());
        }
        let mut environ = Mapping::new();
        for (env_key, env_value) in env.iter() {
            environ.insert(Value::from(env_key.clone()), Value::from(env_value.clone()));
        }
        root_scope.insert(Value::from("env"), Value::from(environ));
        RenderContext {
            stacked_scopes: RefCell::new(vec!(root_scope)),
        }
    }

    // fn values(&self) -> Result<&Value, TemplatingError> {
    //     self.values.ok_or(TemplatingError::MissingScope { name: "values".to_string() })
    // }

    pub(crate) fn push_scope(&self, scope: Mapping) -> ScopesGuard {
        let mut new_scope = self.stacked_scopes.borrow().last().expect("missing root scope").clone();
        new_scope.extend(scope);
        self.stacked_scopes.borrow_mut().push(new_scope);
        ScopesGuard(&self.stacked_scopes)
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

        let parser = template_parser();
        let parsed_template = parser.parse(tmpl)
            .map_err(|errors| ParseTemplate { errors })?;

        // Single substitution can be an Ast,
        // for example `command: ${{ values.cmd }}`
        match parsed_template.split_first() {
            Some((Template::Expr(expr), rest))
            if rest.len() == 0 => {
                return Ok(expr.eval(self).context(EvalSnafu)?);
            }
            _ => {}
        }

        let mut rendered_tmpl = String::new();
        for p in &parsed_template {
            match p {
                Template::Text(text) => rendered_tmpl.push_str(&text),
                Template::Expr(expr) => {
                    match expr.eval(self).context(EvalSnafu)? {
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
                            return Err(Resolve {
                                msg: format!("Can render into string only scalar value or null")
                            });
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
