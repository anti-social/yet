use std::convert::TryFrom;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use failure::Fail;

use quire::Pos;
use quire::ast::Ast;
use quire::ast::Tag;
use quire::ast::ScalarKind;

use crate::parser::{Arg, BoolOp, FilterFun, SubstExpr, TestFun};
use crate::template::RenderContext;
use crate::util::clone_ast;
use crate::parser::SubstExpr::Filter;

pub static BOOL_TRUE_VALUES: &[&str] = &[
    "true", "TRUE", "True",
    "y", "Y", "yes", "YES", "Yes",
    "on", "ON", "On",
];

pub static BOOL_FALSE_VALUES: &[&str] = &[
    "false", "FALSE", "False",
    "n", "N", "no", "NO", "No",
    "off", "OFF", "Off",
];

const VALUES_SCOPE: &str = "values";
const ENV_SCOPE: &str = "env";

#[derive(Debug)]
pub enum EvalOk {
    Node(Ast),
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
}

impl EvalOk {
    fn to_string(&self) -> EvalResult {
        use self::EvalOk::*;
        use self::EvalErr::*;

        Ok(Str(match self {
            Node(v) => {
                return Err(TypeError(format!("Cannot convert yaml ast to string")));
            },
            Bool(v) => v.to_string(),
            Int(v) => v.to_string(),
            Float(v) => v.to_string(),
            Str(v) => v.to_string(),
        }))
    }

    pub fn into_ast(self) -> Ast {
        use self::EvalOk::*;

        let pos = Pos {
            filename: Rc::new("<eval>".to_string()),
            indent: 0,
            line: 1,
            line_start: true,
            line_offset: 0,
            offset: 0,
        };
        match self {
            Node(v) => v,
            Bool(v) => Ast::Scalar(pos, Tag::NonSpecific, ScalarKind::Plain, v.to_string()),
            Int(v) => Ast::Scalar(pos, Tag::NonSpecific, ScalarKind::Plain,v.to_string()),
            Float(v) => Ast::Scalar(pos, Tag::NonSpecific, ScalarKind::Plain,v.to_string()),
            Str(v) => Ast::Scalar(pos, Tag::NonSpecific, ScalarKind::Plain,v),
        }
    }

    pub fn type_name(&self) -> &str {
        use self::EvalOk::*;

        match self {
            Node(_) => "<yaml ast>",
            Bool(_) => "<bool>",
            Int(_) => "<integer>",
            Float(_) => "<float>",
            Str(_) => "<string>",
        }
    }
}

impl Clone for EvalOk {
    fn clone(&self) -> EvalOk {
        use self::EvalOk::*;

        match self {
            Node(v) => Node(clone_ast(v)),
            Bool(v) => Bool(*v),
            Int(v) => Int(*v),
            Float(v) => Float(*v),
            Str(v) => Str(v.clone()),
        }
    }
}

#[derive(Debug, Clone, Fail)]
pub(crate) enum EvalErr {
    #[fail(display = "missing variable: {}", 0)]
    MissingVar(String),
    #[fail(display = "expected mapping: {}", 0)]
    ExpectedMapping(String),
    #[fail(display = "invalid number of arguments: {}", 0)]
    InvalidNumArgs(String),
    #[fail(display = "type error: {}", 0)]
    TypeError(String),
    #[fail(display = "types cannot be compared: {}", 0)]
    IncomparableTypes(String, String),
    #[fail(display = "required boolean type: {}", 0)]
    NonBooleanTypes(String, String),
}

type EvalResult = Result<EvalOk, EvalErr>;

pub(crate) trait Eval {
    fn eval(&self, ctx: &RenderContext) -> EvalResult;
    fn eval_arg(&self, ctx: &RenderContext, arg: &Arg) -> EvalResult;
    fn eval_test_expr(&self, ctx: &RenderContext, fun: &TestFun, args: &Vec<EvalResult>) -> EvalResult;
    fn eval_bool_op(&self, ctx: &RenderContext, op: &BoolOp, args: &Vec<EvalResult>) -> EvalResult;
    fn eval_filter_expr(&self, ctx: &RenderContext, fun: &FilterFun, args: &Vec<EvalResult>) -> EvalResult;
}

impl Eval for SubstExpr {
    fn eval(&self, ctx: &RenderContext) -> EvalResult {
        match self {
            SubstExpr::Atom(var) => self.eval_arg(ctx, var),
            SubstExpr::Test { fun, args } => {
                let args = args.iter().map(|a| a.eval(ctx)).collect();
                self.eval_test_expr(ctx, fun, &args)
            },
            SubstExpr::Bool { op, args } => {
                let args = args.iter().map(|a| a.eval(ctx)).collect();
                self.eval_bool_op(ctx, op, &args)
            },
            SubstExpr::Filter { fun, args } => {
                let args = args.iter().map(|a| a.eval(ctx)).collect();
                self.eval_filter_expr(ctx, fun, &args)
            }
        }
    }

    fn eval_bool_op(&self, ctx: &RenderContext, op: &BoolOp, args: &Vec<EvalResult>) -> EvalResult {
        use self::EvalOk::*;
        use self::EvalErr::*;

        match op {
            BoolOp::Or => {
                required_num_args(op, args, 2, 2)?;
                match (&args[0], &args[1]) {
                    (Ok(Bool(v1)), Ok(Bool(v2))) => Ok(Bool(*v1 || *v2)),
                    (Ok(t1), Ok(t2)) => Err(NonBooleanTypes(
                        t1.type_name().to_string(), t2.type_name().to_string())
                    ),
                    (Err(e), _) | (_, Err(e)) => Err(e.clone()),
                }
            }
            _ => unimplemented!()
        }
    }

    fn eval_arg(&self, ctx: &RenderContext, arg: &Arg) -> EvalResult {
        use self::EvalOk::*;
        use self::EvalErr::*;

        let res = match arg {
            Arg::Int(i) => Ok(Int(*i)),
            Arg::Float(f) => Ok(Float(*f)),
            Arg::Bool(b) => Ok(Bool(*b)),
            Arg::Str(s) => Ok(Str(s.clone())),
            Arg::Var(var) => match var.split('.').collect::<Vec<_>>().split_first() {
                Some((scope, path)) if scope == &VALUES_SCOPE => {
                    match ctx.values {
                        Some(values) => follow_ast(values, scope, path),
                        None => Err(MissingVar(VALUES_SCOPE.to_string()))
                    }
                },
                Some((scope, path)) if scope == &ENV_SCOPE => {
                    let key = path.join(".");
                    match ctx.env.get(&key) {
                        Some(v) => Ok(Str(v.clone())),
                        None => Err(MissingVar(format!("{}.{}", scope, key))),
                    }
                },
                Some((scope, path)) => {
                    let mut found_ast = None;
                    for scopes_frame in ctx.scopes_stack.borrow().iter().rev() {
                        match scopes_frame.get(*scope) {
                            Some(scope_ast) => {
                                found_ast = Some(
                                    follow_ast(scope_ast, scope, path)
                                );
                                break;
                            }
                            None => continue
                        }
                    }
                    if let Some(var_ast) = found_ast {
                        var_ast
                    } else {
                        Err(MissingVar(format!("Unknown scope: {}", scope)))
                    }
                },
                None => Err(MissingVar(format!("Empty variable path"))),
            },
        };

        match res {
            Ok(Node(Ast::Scalar(_, _, ScalarKind::Plain, ref s))) => {
                eval_plain_scalar(s)
            },
            Ok(v) => Ok(v.clone()),
            Err(e) => Err(e.clone()),
        }
    }

    fn eval_test_expr(&self, ctx: &RenderContext, fun: &TestFun, args: &Vec<EvalResult>)
        -> EvalResult
    {
        use self::EvalOk::*;
        use self::EvalErr::*;

        match fun {
            TestFun::Defined => {
                required_num_args(fun, args, 1, 1)?;
                match &args[0] {
                    Ok(_) => Ok(Bool(true)),
                    Err(MissingVar(_)) => Ok(Bool(false)),
                    Err(e) => Err(e.clone()),
                }
            },
            TestFun::Undefined => {
                required_num_args(fun, args, 1, 1)?;
                match &args[0] {
                    Ok(_) => Ok(Bool(false)),
                    Err(MissingVar(_)) => Ok(Bool(true)),
                    Err(e) => Err(e.clone()),
                }
            },
            TestFun::Eq => {
                required_num_args(fun, args, 2, 2)?;
                eval_eq(&args[0], &args[1], false)
            },
            TestFun::NotEq => {
                required_num_args(fun, args, 2, 2)?;
                eval_eq(&args[0], &args[1], true)
            }
        }
    }

    fn eval_filter_expr(&self, ctx: &RenderContext, fun: &FilterFun, args: &Vec<EvalResult>)
        -> EvalResult
    {
        use self::EvalOk::*;
        use self::EvalErr::*;

        match fun {
            FilterFun::CapFirst => cap_first(&args),
            FilterFun::Lower => lower(&args),
            FilterFun::Trim => trim(&args),
            FilterFun::Truncate => truncate(&args),
            FilterFun::Upper => upper(&args),
        }
    }
}

fn cap_first(args: &Vec<EvalResult>) -> EvalResult {
    use self::EvalOk::*;
    use self::EvalErr::*;

    required_num_args(&FilterFun::CapFirst, args, 1, 1)?;
    match &args[0] {
        Ok(Str(s)) | Ok(Node(Ast::Scalar(_, _, _, s))) => {
            let mut chars = s.chars();
            Ok(Str(match chars.next() {
                None => String::new(),
                Some(c) => dbg!(c.to_uppercase().to_string()) + chars.as_str(),
            }))
        },
        Ok(_) => Err(TypeError("required a string".to_string())),
        Err(e) => Err(e.clone()),
    }
}

fn lower(args: &Vec<EvalResult>) -> EvalResult {
    use self::EvalOk::*;
    use self::EvalErr::*;

    required_num_args(&FilterFun::Lower, args, 1, 1)?;
    match &args[0] {
        Ok(Str(s)) | Ok(Node(Ast::Scalar(_, _, _, s))) => {
            Ok(Str(s.to_lowercase()))
        },
        Ok(_) => Err(TypeError("required a string".to_string())),
        Err(e) => Err(e.clone()),
    }
}

fn upper(args: &Vec<EvalResult>) -> EvalResult {
    use self::EvalOk::*;
    use self::EvalErr::*;

    required_num_args(&FilterFun::Lower, args, 1, 1)?;
    match &args[0] {
        Ok(Str(s)) | Ok(Node(Ast::Scalar(_, _, _, s))) => {
            Ok(Str(s.to_uppercase()))
        },
        Ok(_) => Err(TypeError("required a string".to_string())),
        Err(e) => Err(e.clone()),
    }
}

fn trim(args: &Vec<EvalResult>) -> EvalResult {
    use self::EvalOk::*;
    use self::EvalErr::*;

    required_num_args(&FilterFun::Lower, args, 1, 1)?;
    match dbg!(&args[0]) {
        Ok(Str(s))
        | Ok(Node(Ast::Scalar(_, _, _, s))) => {
            Ok(Str(s.trim().to_string()))
        },
        Ok(_) => Err(TypeError("required a string".to_string())),
        Err(e) => Err(e.clone()),
    }
}

fn truncate(args: &Vec<EvalResult>) -> EvalResult {
    use self::EvalOk::*;
    use self::EvalErr::*;

    required_num_args(&FilterFun::Lower, args, 2, 2)?;
    let mut args_iter = args.iter();
    let mut s = match dbg!(args_iter.next().unwrap()) {
        Ok(Str(s)) | Ok(Node(Ast::Scalar(_, _, _, s))) => s,
        Ok(_) => return Err(TypeError("required a string".to_string())),
        Err(e) => return Err(e.clone()),
    }.to_string();
    let length = match args_iter.next().unwrap() {
        Ok(Int(n)) => {
            match usize::try_from(*n) {
                Ok(l) => l,
                Err(e) => return Err(TypeError("Cannot convert to usize".to_string())),
            }
        },
        Ok(_) => return Err(TypeError("required an integer".to_string())),
        Err(e) => return Err(e.clone()),
    };

    s.truncate(length);
    Ok(Str(s))
}

fn eval_eq(a1: &EvalResult, a2: &EvalResult, negate: bool) -> EvalResult {
    use self::EvalOk::*;
    use self::EvalErr::*;

    match (&a1, &a2) {
        (Ok(a1), Ok(a2)) => {
            let res = match (a1, a2) {
                (Bool(v1), Bool(v2)) => v1 == v2,
                (Int(v1), Int(v2)) => v1 == v2,
                (Float(v1), Float(v2)) => v1 == v2,
                (Str(v1), Str(v2)) => v1 == v2,
                (t1, t2) => return Err(IncomparableTypes(
                    t1.type_name().to_string(),
                    t2.type_name().to_string()
                )),
            };
            Ok(Bool(if negate { !res } else { res }))
        },
        (Err(e), _) | (_, Err(e)) => Err(e.clone()),
    }
}

fn eval_plain_scalar(v: &str) -> EvalResult {
    use self::EvalOk::*;

    if BOOL_TRUE_VALUES.contains(&v) {
        Ok(Bool(true))
    } else if BOOL_FALSE_VALUES.contains(&v) {
        Ok(Bool(false))
    } else if let Ok(n) = v.parse::<i64>() {
        Ok(Int(n))
    } else if let Ok(n) = v.parse::<f64>() {
        Ok(Float(n))
    } else {
        Ok(Str(v.to_string()))
    }
}

fn required_num_args(fun: &Debug, args: &Vec<EvalResult>, min: usize, max: usize) -> Result<(), EvalErr> {
    if args.len() < min || args.len() > max {
        if min == max {
            return Err(EvalErr::InvalidNumArgs(format!(
                "{:?} takes {} argument(s), {} given", fun, min, args.len()
            )));
        } else {
            return Err(EvalErr::InvalidNumArgs(format!(
                "{:?} takes from {} to {} arguments, {} given",
                fun, min, max, args.len()
            )))
        }
    }
    Ok(())
}

fn follow_ast<'a>(ast: &'a Ast, scope: &'a str, var_path: &[&str])
    -> EvalResult
{
    let mut cur_node = ast;
    for (ix, p) in var_path.iter().enumerate() {
        let map = match cur_node {
            Ast::Map(_, _, map) => map,
            _ => return Result::Err(EvalErr::ExpectedMapping(format!(
                "{}.{}", scope, var_path.join(".")
            ))),
        };
        cur_node = match map.get(*p) {
            Some(v) => v,
            None => return Result::Err(EvalErr::MissingVar(format!(
                "{}.{}", scope, var_path[..ix+1].join(".")
            ))),
        };
    }
    Result::Ok(EvalOk::Node(clone_ast(cur_node)))
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use matches::assert_matches;
    
    use crate::parser::{Arg, SubstExpr, TestFun};
    use crate::template::RenderContext;
    use super::{Eval, EvalOk};

    fn get_test_env() -> HashMap<String, String> {
        let mut env = HashMap::new();
        env.insert("TEST".to_string(), "1".to_string());
        env.insert("TEST_EQ".to_string(), "1".to_string());
        env.insert("TEST_NOT_EQ".to_string(), "2".to_string());
        env
    }

    #[test]
    fn test_eval_defined_false() {
        let expr = SubstExpr::Test {
            fun: TestFun::Defined,
            args: vec!(
                SubstExpr::Atom(Arg::Var("env.UNDEFINED".to_string()))
            ),
        };
        let env = get_test_env();
        let ctx = RenderContext::new(None, &env);
        assert_matches!(
            expr.eval(&ctx),
            Ok(EvalOk::Bool(res)) if !res
        );
    }

    #[test]
    fn test_eval_defined_true() {
        let expr = SubstExpr::Test {
            fun: TestFun::Defined,
            args: vec!(
                SubstExpr::Atom(Arg::Var("env.TEST".to_string()))
            ),
        };
        let env = get_test_env();
        let ctx = RenderContext::new(None, &env);
        assert_matches!(
            expr.eval(&ctx),
            Ok(EvalOk::Bool(res)) if res
        );
    }

    #[test]
    fn test_eval_eq_true() {
        let expr = SubstExpr::Test {
            fun: TestFun::Eq,
            args: vec!(
                SubstExpr::Atom(Arg::Var("env.TEST".to_string())),
                SubstExpr::Atom(Arg::Var("env.TEST_EQ".to_string())),
            ),
        };
        let env = get_test_env();
        let ctx = RenderContext::new(None, &env);
        assert_matches!(
            expr.eval(&ctx),
            Ok(EvalOk::Bool(res)) if res
        );
    }

    #[test]
    fn test_eval_eq_false() {
        let expr = SubstExpr::Test {
            fun: TestFun::Eq,
            args: vec!(
                SubstExpr::Atom(Arg::Var("env.TEST".to_string())),
                SubstExpr::Atom(Arg::Var("env.TEST_NOT_EQ".to_string())),
            ),
        };
        let env = get_test_env();
        let ctx = RenderContext::new(None, &env);
        assert_matches!(
            expr.eval(&ctx),
            Ok(EvalOk::Bool(res)) if !res
        );
    }
}
