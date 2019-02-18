use quire::ast::Ast;
use quire::ast::Tag;
use quire::ast::ScalarKind;

use crate::parser::{SubstExpr, TestFun, Variable};
use crate::template::RenderContext;
use crate::util::clone_ast;

const VALUES_SCOPE: &str = "values";
const ENV_SCOPE: &str = "env";

#[derive(Debug)]
enum EvalOk {
    Node(Ast),
    Bool(bool),
    Str(String),
}

#[derive(Debug)]
enum EvalErr {
    MissingVar(String),
    ExpectedMapping(String),
}

type EvalResult = Result<EvalOk, EvalErr>;

trait Eval {
    fn eval(&self, ctx: &RenderContext) -> EvalResult;
    fn eval_var(&self, ctx: &RenderContext, var_path: &Variable) -> EvalResult;
    fn eval_expr(&self, ctx: &RenderContext, fun: &TestFun, arg: EvalResult) -> EvalResult;
}

impl Eval for SubstExpr {
    fn eval(&self, ctx: &RenderContext) -> EvalResult {
        match self {
            SubstExpr::Var(var) => self.eval_var(ctx, var),
            SubstExpr::Test{fun, var} => self.eval_expr(ctx, fun, self.eval_var(ctx, var)),
        }
    }

    fn eval_var(&self, ctx: &RenderContext, var: &Variable) -> EvalResult {
        let scope_and_path = var.path.split_first();
        match scope_and_path {
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
        }
    }

    fn eval_expr(&self, ctx: &RenderContext, fun: &TestFun, arg: EvalResult)
        -> EvalResult
    {
        match fun {
            TestFun::Defined => match arg {
                Result::Ok(_) => Result::Ok(EvalOk::Bool(true)),
                Result::Err(EvalErr::MissingVar(_)) => Result::Ok(EvalOk::Bool(false)),
                Result::Err(e) => Result::Err(e),
            },
            TestFun::Undefined => match arg {
                Result::Ok(_) => Result::Ok(EvalOk::Bool(false)),
                Result::Err(EvalErr::MissingVar(_)) => Result::Ok(EvalOk::Bool(true)),
                Result::Err(e) => Result::Err(e),
            },
        }
    }
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
    
    use crate::parser::{SubstExpr, TestFun, Variable};
    use crate::template::RenderContext;
    use super::{Eval, EvalOk};

    fn get_test_env() -> HashMap<String, String> {
        let mut env = HashMap::new();
        env.insert("TEST".to_string(), "1".to_string());
        env
    }

    #[test]
    fn test_eval_defined_false() {
        let expr = SubstExpr::Test {
            var: Variable { path: vec!("env".to_string(), "UNDEFINED".to_string()) },
            fun: TestFun::Defined,
        };
        let env = get_test_env();
        let ctx = RenderContext::new(None, &env);
        assert_matches!(
            expr.eval(&ctx),
            Ok(EvalOk::Bool(res)) if res == false
        );
    }

    #[test]
    fn test_eval_defined_true() {
        let expr = SubstExpr::Test {
            var: Variable { path: vec!("env".to_string(), "TEST".to_string()) },
            fun: TestFun::Defined,
        };
        let env = get_test_env();
        let ctx = RenderContext::new(None, &env);
        assert_matches!(
            expr.eval(&ctx),
            Ok(EvalOk::Bool(res)) if res == true
        );
    }
}
