use quire::ast::Ast;
use quire::ast::Tag;
use quire::ast::ScalarKind;

use crate::parser::{Arg, SubstExpr, TestFun};
use crate::template::RenderContext;
use crate::util::clone_ast;

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

const VALUES_SCOPE: &str = "values";
const ENV_SCOPE: &str = "env";

#[derive(Debug)]
enum EvalOk {
    Node(Ast),
    Bool(bool),
    Str(String),
}

#[derive(Debug, Clone)]
enum EvalErr {
    MissingVar(String),
    ExpectedMapping(String),
    NumArgs(String),
}

type EvalResult = Result<EvalOk, EvalErr>;

trait Eval {
    fn eval(&self, ctx: &RenderContext) -> EvalResult;
    fn eval_arg(&self, ctx: &RenderContext, arg: &Arg) -> EvalResult;
    fn eval_expr(&self, ctx: &RenderContext, fun: &TestFun, args: &Vec<EvalResult>) -> EvalResult;
}

impl Eval for SubstExpr {
    fn eval(&self, ctx: &RenderContext) -> EvalResult {
        match self {
            SubstExpr::Var(var) => self.eval_arg(ctx, var),
            SubstExpr::Test{fun, args} => {
                let args = args.iter().map(|a| self.eval_arg(ctx, a)).collect();
                self.eval_expr(ctx, fun, &args)
            },
        }
    }

    fn eval_arg(&self, ctx: &RenderContext, arg: &Arg) -> EvalResult {
        match arg {
            Arg::Var(var_path) => match var_path.split_first() {
                Some((scope, path)) if scope == VALUES_SCOPE => {
                    match ctx.values {
                        Some(values) => follow_ast(values, scope, path),
                        None => Result::Err(EvalErr::MissingVar(format!("Missing scope: values")))
                    }
                },
                Some((scope, path)) if scope == ENV_SCOPE => {
                    let key = path.join(".");
                    match ctx.env.get(&key) {
                        Some(v) => Result::Ok(EvalOk::Str(v.clone())),
                        None => Result::Err(EvalErr::MissingVar(format!("Missing key: env.{}", key))),
                    }
                },
                Some((scope, path)) => {
                    let mut found_ast = None;
                    for scopes_frame in ctx.scopes_stack.borrow().iter().rev() {
                        match scopes_frame.get(scope) {
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
                        Result::Err(EvalErr::MissingVar(format!("Unknown scope: {}", scope)))
                    }
                },
                None => Result::Err(EvalErr::MissingVar(format!("Empty variable path"))),
            },
        }
    }

    fn eval_expr(&self, ctx: &RenderContext, fun: &TestFun, args: &Vec<EvalResult>)
        -> EvalResult
    {
        use self::EvalOk::*;
        use self::EvalErr::*;

        match fun {
            TestFun::Defined => {
                required_num_args(fun, args, 1)?;
                match &args[0] {
                    Ok(_) => Ok(Bool(true)),
                    Err(MissingVar(_)) => Ok(Bool(false)),
                    Err(e) => Err(e.clone()),
                }
            },
            TestFun::Undefined => {
                required_num_args(fun, args, 1)?;
                match &args[0] {
                    Ok(_) => Ok(Bool(false)),
                    Err(MissingVar(_)) => Ok(Bool(true)),
                    Err(e) => Err(e.clone()),
                }
            },
            TestFun::Eq => {
                required_num_args(fun, args, 2)?;
                match (&args[0], &args[1]) {
                    (Ok(Bool(a1)), Ok(Bool(a2))) => {
                        Ok(Bool(a1 == a2))
                    },
                    (Ok(Bool(a1)), Ok(Node(Ast::Scalar(_, _, ScalarKind::Plain, a2))))
                    | (Ok(Node(Ast::Scalar(_, _, ScalarKind::Plain, a2))), Ok(Bool(a1))) => {
                        Ok(Bool(a1 == Ba2))
                    },
                    (Ok(Str(a1)), Ok(Str(a2))) => {
                        Ok(Bool(a1 == a2))
                    },
                    (Ok(Node(Ast::Scalar(.., a1))), Ok(Str(a2)))
                    | (Ok(Str(a2)), Ok(Node(Ast::Scalar(.., a1)))) => {
                        Ok(Bool(a1 == a2))
                    },
                    (Ok(Node(a1)), Ok(Node(a2))) => match (a1, a2) {
                        (Ast::Scalar(.., s1), Ast::Scalar(.., s2)) => Ok(Bool(s1 == s2)),
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                }
            }
        }
    }
}

fn eval_ast(a: Ast) -> EvalResult {
    match a {
        Ast::Scalar(_, _, ScalarKind::Plain, ref s)
        if BOOL_TRUE_VALUES.contains(&s.as_str()) => {
            Ok(EvalOk::Bool(true))
        },
        Ast::Scalar(_, _, ScalarKind::Plain, s)
        if BOOL_FALSE_VALUES.contains(&s.as_str()) => {
            Ok(EvalOk::Bool(false))
        },
        Ast::Scalar(_, _, ScalarKind::Plain, s)
    }
}



fn required_num_args(fun: &TestFun, args: &Vec<EvalResult>, req_len: usize) -> Result<(), EvalErr> {
    if args.len() != req_len {
        return Err(EvalErr::NumArgs(format!(
            "{:?} takes 1 argument, {} given", fun, args.len()
        )));
    }
    Ok(())
}

fn follow_ast<'a>(ast: &'a Ast, scope: &'a str, var_path: &[String])
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
        cur_node = match map.get(p) {
            Some(v) => v,
            None => return Result::Err(EvalErr::MissingVar(format!(
                "Missing key: {}.{}", scope, var_path[..ix+1].join(".")
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
            args: vec!(Arg::Var(vec!("env".to_string(), "UNDEFINED".to_string()))),
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
            args: vec!(Arg::Var(vec!("env".to_string(), "TEST".to_string()))),
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
                Arg::Var(vec!("env".to_string(), "TEST".to_string())),
                Arg::Var(vec!("env".to_string(), "TEST_EQ".to_string())),
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
                Arg::Var(vec!("env".to_string(), "TEST".to_string())),
                Arg::Var(vec!("env".to_string(), "TEST_NOT_EQ".to_string())),
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
