mod imp;
mod fun;

use std::collections::HashMap;
use crate::fun::{Expr, Function};
use crate::imp::Procedure;
pub use fun::eval_func;
pub use imp::eval_proc;


#[derive(Debug)]
pub enum EvaluationError {
  UnboundVar { name: String },
  DivisionBy0,
  NonExaustivePatternMatch { pattern: Expr},
  FunctionDoesntExist { name: String },
  CannotApplyNonLambda { not_func: Expr, arg: Expr},
  UnsupportedBinaryOp {left: Expr, right: Expr}
}

pub type EvalResult<A> = Result<A, EvaluationError>;

#[derive(Debug)]
pub struct Env {
  pub vars: HashMap<String, Expr>,
  pub rules: HashMap<String, Callable>,
}

#[derive(Debug)]
pub enum Callable {
  Function(Function),
  Procedure(Procedure),
}

impl Env {
  pub fn new() -> Self {
    let vars = HashMap::new();
    let rules = HashMap::new();
    Env {vars, rules}
  }

  pub fn add_vars(&mut self, vars: Vec<(String, Expr)>) -> Vec<(String, Expr)> {
    let mut old_binds = Vec::new();
    for (name, value) in vars {
      if let Some(expr) = self.vars.insert(name.clone(), value) {
        old_binds.push((name, expr));
      }
    }
    old_binds
  }

  pub fn add_functions(&mut self, funcs: Vec<Function>) -> Vec<Callable> {
    let mut old_funcs = Vec::new();
    for func in funcs {
      let name = func.name.clone();
      let func = Callable::Function(func);
      if let Some(old_fun) = self.rules.insert(name, func) {
        old_funcs.push(old_fun);
      }
    }
    old_funcs
  }

  pub fn add_procedures(&mut self, procs: Vec<Procedure>) -> Vec<Callable> {
    let mut old_procs = Vec::new();
    for proc in procs {
      let name = proc.name.clone();
      let proc = Callable::Procedure(proc);
      if let Some(old_fun) = self.rules.insert(name, proc) {
        old_procs.push(old_fun);
      }
    }
    old_procs
  }
}