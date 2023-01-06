use crate::imp::syntax::Procedure;
use crate::fun::syntax::Expr;
use super::{Callable, Env, EvalResult, EvaluationError};

/// Evaluates a procedure with the given arguments,
/// and return the results on success.
pub fn eval_proc(env: &mut Env, proc: &Procedure, args: &[Expr]) -> EvalResult<Expr> {
    todo!()
}