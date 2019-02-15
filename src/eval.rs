use crate::parser::{SubstExpr, TestFun};

struct EvalResult;

trait Eval {
    fn eval(&self) -> EvalResult;
}



impl Eval for SubstExpr {
    fn eval(&self) -> EvalResult {
        match self {
            SubstExpr::Var(var) => {},
            SubstExpr::Test{fun, var} => {
                match fun {
                    TestFun::Defined => {},
                    TestFun::Undefined => {},
                }
            },
        }

        EvalResult
    }
}
