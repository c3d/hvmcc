use crate::{Expr, Id};
use std::collections::HashSet;

impl Expr {
  /// Returns a set of variables that are not defined inside of the expression
  pub fn get_unbound_vars(&self) -> HashSet<Id> {
    match self {
      Expr::Unit => HashSet::new(),
      Expr::Ctr { args, .. } => {
        let mut vars = HashSet::new();
        for arg in args {
          vars.extend(arg.get_unbound_vars());
        }
        vars
      }
      Expr::FunCall { args, .. } => {
        let mut vars = HashSet::new();
        for arg in args {
          vars.extend(arg.get_unbound_vars());
        }
        vars
      }
      Expr::Let { name, expr, body } => {
        let mut vars = body.get_unbound_vars();
        vars.remove(name); // The variable defined by the let is not unbound in its body
        vars.extend(expr.get_unbound_vars());
        vars
      }
      Expr::App { expr, argm } => {
        let mut vars = expr.get_unbound_vars();
        vars.extend(argm.get_unbound_vars().into_iter());
        vars
      }
      Expr::Var { name } => HashSet::from([name.clone()]),
      Expr::Unsigned { .. } => HashSet::new(),
      Expr::Float { .. } => HashSet::new(),
      Expr::BinOp { left, right, .. } => {
        let mut vars = left.get_unbound_vars();
        vars.extend(right.get_unbound_vars().into_iter());
        vars
      }
      Expr::Lambda { var, body } => {
        let mut vars = body.get_unbound_vars();
        vars.remove(var);
        vars
      }
      Expr::MatchExpr { scrutinee, cases } => {
        let mut vars = scrutinee.get_unbound_vars();
        for case in cases {
          let pat_vars = case.matched.get_unbound_vars();
          let body_vars = case.body.get_unbound_vars();
          let diff: HashSet<String> = body_vars.difference(&pat_vars).map(String::clone).collect();
          vars.extend(diff);
        }
        vars
      }
    }
  }
}
