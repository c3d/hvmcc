use crate::fun::syntax::*;
use std::collections::HashMap;

type EvalResult<A> = Result<A, EvaluationError>;

#[derive(Debug)]
pub struct Env {
  vars: HashMap<String, Expr>,
  pub rules: HashMap<String, Function>,
}

#[derive(Debug)]
pub enum EvaluationError {
  UnboundVar { name: String },
  DivisionBy0,
  NonExaustivePatternMatch { pattern: Expr},
  FunctionDoesntExist { name: String },
  CannotApplyNonLambda { not_func: Expr, arg: Expr},
  UnsupportedBinaryOp {left: Expr, right: Expr}
}

impl Env {
  pub fn new() -> Self {
    let vars = HashMap::new();
    let rules = HashMap::new();
    Env {vars, rules}
  }
}

/// Returns Some if it the args match with the lhs and None if it does not
/// if it does, it returns a vector of names that binds with values
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
fn eval_with(binds: Vec<(String, Expr)>, term: &Expr, env: &mut Env) -> EvalResult<Expr> {
  let mut old_binds = vec![];
  for (name, value) in binds {
    if let Some(expr) = env.vars.insert(name.clone(), value) {
      old_binds.push((name, expr))
    }
  }
  let ret = eval(term, env);
  for (name, value) in old_binds {
    env.vars.insert(name, value);
  }
  ret
}

/// Evaluates an expression given an environment.
pub fn eval(term: &Expr, env: &mut Env) -> EvalResult<Expr> {
  match term {
    Expr::Ctr { name, args } => {
      let name = name.clone();
      let args = args
        .iter()
        .map(|x| eval(x, env))
        .collect::<EvalResult<Vec<Expr>>>()?;
      Ok(Expr::Ctr { name, args })
    },
    Expr::FunCall { name, args } => {
      let func = env.rules.get(name).cloned();
      // this `.cloned` is needed to free `env`.
      // it is kinda stupid.
      let args = args.iter().map(|x| eval(x, env)).collect::<EvalResult<Vec<Expr>>>()?;
      let ctr = Expr::Ctr { name: name.clone(), args };
      if let Some(function) = func {
        for Rule {lhs, rhs} in function.rules.iter() {
          if let Some(binds) = matches(&ctr, lhs) {
            return eval_with(binds, rhs, env);
          }
        }
        let pattern = term.clone();
        Err(EvaluationError::NonExaustivePatternMatch { pattern })
      } else {
        Err(EvaluationError::FunctionDoesntExist { name: name.clone() })
      }
    },
    Expr::Let { name, expr, body } => {
      let expr = eval(expr, env)?;
      let bind = vec![(name.clone(), expr)];
      eval_with(bind, body, env)
    },
    Expr::App { expr, argm } => {
      let func = eval(expr, env)?;
      match func {
        Expr::Lambda {var, body} => {
          let val = eval(argm, env)?;
          let bind = vec![(var, val)];
          eval_with(bind, &body, env)
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
        let expr = eval(scrutinee, env)?;
        if let Some(binds) = matches(&expr, matched) {
          return eval_with(binds, body, env);
        }
      }
      let pattern = *scrutinee.clone();
      Err(EvaluationError::NonExaustivePatternMatch { pattern })
    },
    Expr::Lambda { var, body } => {
      let bind = vec![(var.clone(), Expr::Var {name: var.clone()})];
      // bind the variable to itself, in order to not change semantics of unbound variables.
      let body = Box::new(eval_with(bind, body, env)?);
      let var = var.clone();
      Ok(Expr::Lambda {var, body})
      // return the new lambda with eval'd body.
    },
    Expr::BinOp { op, left, right } => {
      let left  = eval(left, env)?;
      let right = eval(right, env)?;
      let new = |x: u64| x & 0xFFF_FFFF_FFFF_FFFF;
      if let (&Expr::Unsigned {numb:a}, &Expr::Unsigned { numb: b }) = (&left, &right) {
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
