use crate::fun::syntax::*;
use super::{Callable, Env, EvalResult, EvaluationError};
use super::imp::eval_proc;

/// Evaluates a function call
pub fn eval_func(env: &mut Env, func: &Function, args: &[Expr]) -> EvalResult<Expr>{
  let args = args.iter().map(|x| eval_expr(x, env)).collect::<EvalResult<Vec<Expr>>>()?;
  let name = func.name.clone();
  let ctr = Expr::Ctr { name: name.clone(), args: args.clone() };
  for Rule {lhs, rhs} in func.rules.iter() {
    if let Some(binds) = matches(&ctr, lhs) {
      return eval_expr_with(binds, rhs, env);
    }
  }
  let pattern = Expr::FunCall { name, args };
  Err(EvaluationError::NonExaustivePatternMatch { pattern })
}

/// Evaluates an expression given an environment.
pub fn eval_expr(term: &Expr, env: &mut Env) -> EvalResult<Expr> {
  match term {
    Expr::Ctr { name, args } => {
      let name = name.clone();
      let args = args
        .iter()
        .map(|x| eval_expr(x, env))
        .collect::<EvalResult<Vec<Expr>>>()?;
      Ok(Expr::Ctr { name, args })
    },
    Expr::FunCall { name, args } => {
      if let Some(call) = env.rules.get(name) {
        match call {
          Callable::Function(func) => eval_func(env, &func.clone(), args),
          Callable::Procedure(proc) => eval_proc(env, &proc.clone(), args),
        }
      } else {
        Err(EvaluationError::FunctionDoesntExist { name: name.to_string() })
      }
    }
    Expr::Let { name, expr, body } => {
      let expr = eval_expr(expr, env)?;
      let bind = vec![(name.clone(), expr)];
      eval_expr_with(bind, body, env)
    },
    Expr::App { expr, argm } => {
      let func = eval_expr(expr, env)?;
      match func {
        Expr::Lambda {var, body} => {
          let val = eval_expr(argm, env)?;
          let bind = vec![(var, val)];
          eval_expr_with(bind, &body, env)
        },
        term => Err(EvaluationError::CannotApplyNonLambda { not_func: term, arg: *argm.clone() })
      }
    },
    Expr::Var { name } => {
      if let Some(expr) = env.vars.get(name) {
        Ok(expr.clone())
      } else {
        Err(EvaluationError::UnboundVar { name: name.clone() })
      }
    },
    Expr::MatchExpr { scrutinee, cases } => {
      for CaseExpr {matched, body} in cases {
        let expr = eval_expr(scrutinee, env)?;
        if let Some(binds) = matches(&expr, matched) {
          return eval_expr_with(binds, body, env);
        }
      }
      let pattern = *scrutinee.clone();
      Err(EvaluationError::NonExaustivePatternMatch { pattern })
    },
    Expr::Lambda { var, body } => {
      let bind = vec![(var.clone(), Expr::Var {name: var.clone()})];
      // bind the variable to itself, in order to not change semantics of unbound variables.
      let body = Box::new(eval_expr_with(bind, body, env)?);
      let var = var.clone();
      Ok(Expr::Lambda {var, body})
      // return the new lambda with eval_expr'd body.
    },
    Expr::BinOp { op, left, right } => {
      let left  = eval_expr(left, env)?;
      let right = eval_expr(right, env)?;
      if let (&Expr::Unsigned { numb: a }, &Expr::Unsigned { numb: b }) = (&left, &right) {
        let new = |x: u64| x & 0xFFF_FFFF_FFFF_FFFF;
        let numb = match op {
          Oper::Add => Ok(new(a + b)),
          Oper::Sub => Ok(if a >= b { a - b } else { 0x1000000000000000 - (b - a) }) ,
          Oper::Mul => Ok(new((a as u128 * b as u128) as u64)),
          Oper::Div => {
            if b == 0 {
              Err(EvaluationError::DivisionBy0)
            }
            else {
              Ok(a / b)
            }
          },
          Oper::Mod => Ok(a % b),
          Oper::And => Ok(a & b),
          Oper::Or  => Ok(a | b),
          Oper::Xor => Ok(a ^ b),
          Oper::Shl => Ok(new(a << b)),
          Oper::Shr => Ok(a >> b),
          Oper::Lte => Ok(if a <  b { 1 } else { 0 }),
          Oper::Ltn => Ok(if a <= b { 1 } else { 0 }),
          Oper::Eql => Ok(if a == b { 1 } else { 0 }),
          Oper::Gte => Ok(if a >  b { 1 } else { 0 }),
          Oper::Gtn => Ok(if a >= b { 1 } else { 0 }),
          Oper::Neq => Ok(if a != b { 1 } else { 0 }),
        }?;
        Ok(Expr::Unsigned { numb })
      } else if let (&Expr::Float { numb: a }, &Expr::Float { numb: b }) = (&left, &right) {
        let numb = match op {
          Oper::Add => Ok(a + b),
          Oper::Sub => Ok(a - b),
          Oper::Mul => Ok(a * b),
          Oper::Div => {
            if b == 0.0 {
              Err(EvaluationError::DivisionBy0)
            }
            else {
              Ok(a / b)
            }
          },
          Oper::Mod => Ok(a % b),
          Oper::And => Ok(f64::cos(a) + f64::sin(b)),
          Oper::Or  => Ok(f64::atan2(a, b)),
          Oper::Xor => Ok(a.ceil() + a.floor()),
          Oper::Shl => Ok(b.powf(a)),
          Oper::Shr => Ok(a.log(b)),
          Oper::Lte => Ok(if a <  b { 1.0 } else { 0.0 }),
          Oper::Ltn => Ok(if a <= b { 1.0 } else { 0.0 }),
          Oper::Eql => Ok(if a == b { 1.0 } else { 0.0 }),
          Oper::Gte => Ok(if a >  b { 1.0 } else { 0.0 }),
          Oper::Gtn => Ok(if a >= b { 1.0 } else { 0.0 }),
          Oper::Neq => Ok(if a != b { 1.0 } else { 0.0 }),
        }?;
        Ok(Expr::Float { numb })
      }
      else {
        Err(EvaluationError::UnsupportedBinaryOp { left, right })
      }
    },
    Expr::Unit |
    Expr::Unsigned { .. } |
    Expr::Float { .. } => Ok(term.clone())
  }
}

/// If the term matches the lhs of the rule,
/// return the vars in the pattern and the expressions they bind to.
/// Otherwise return none.
fn matches(term: &Expr, lhs: &Expr) -> Option<Vec<(String, Expr)>> {
  match (lhs, term) {
    (Expr::Ctr {name: lhs_name, args: lhs_args}, Expr::Ctr {name: term_name, args: term_args}) => {
      if lhs_name == term_name && lhs_args.len() == term_args.len() {
        // needs to check sizes because zips behaves weirdly on different size vectors
        // instead of just throwing an error.
        let binds = term_args
          .iter()
          .zip(lhs_args.iter())
          .map(|(lhs_arg, term_arg)| matches(lhs_arg, term_arg))
          .fold(Some(vec![]), |a, b| match (a, b) {
            (Some(avec), Some(bvec)) => Some(avec.into_iter().chain(bvec.into_iter()).collect()),
            _ => None,
          });
        binds
      }
      else {
        None
      }
    },
    (Expr::Var {name}, some_term) => {
      Some(vec![(name.clone(), some_term.clone())])
    }
    (Expr::Unsigned {numb:lhs_numb}, Expr::Unsigned{numb:term_numb}) => {
      if lhs_numb == term_numb {
        Some(vec![])
      }
      else {
        None
      }
    }
    _ => None,
  }
}

/// Binds the names in `binds` to expressions in the environment
/// Then, evaluates `term` and remove those binds,
/// in order to do proper lexical scoping
fn eval_expr_with(binds: Vec<(String, Expr)>, term: &Expr, env: &mut Env) -> EvalResult<Expr> {
  let vars: Vec<Id> = binds.iter().map(|(x, _)| x.clone()).collect();
  // Add bindings
  let old_binds = env.add_vars(binds);
  // Evaluate
  let ret = eval_expr(term, env);
  // Remove bindings
  for var in vars {
    env.vars.remove(&var);
  }
  // Restore old values
  for (name, value) in old_binds {
    env.vars.insert(name, value);
  }
  ret
}