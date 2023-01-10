use super::{Callable, Env, EvalResult, EvaluationError};
use crate::fun::syntax::Expr;
use crate::imp::syntax::{Imp, Procedure};

/// Evaluates a procedure with the given arguments,
/// and return the results on success.
pub fn eval_proc(env: &mut Env, proc: &Procedure, args: &[Expr]) -> EvalResult<Expr> {
  todo!()
}

/// Evaluate a procedure, returning the result and making changes to the environment
pub fn eval_stmt(env: &mut Env, stmt: &Imp) -> EvalResult<Expr> {
  todo!()
}