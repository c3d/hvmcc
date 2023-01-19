use crate::fun::syntax as fun;
use ::hvm::runtime::data::{f60, u60};
use hvm::language::syntax as hvm;
use std::collections::HashSet;

pub fn compile_function(function: fun::Function) -> Result<hvm::File, String> {
  let mut rules = Vec::new();
  for rule in function.rules {
    let file = compile_rule(&function.name, rule)?;
    rules.extend(file.rules);
  }
  Ok(hvm::File { rules, smaps: Vec::new() })
}

pub fn compile_rule(fn_name: &fun::Id, mut rule: fun::Rule) -> Result<hvm::File, String> {
  if !is_valid_lhs(&rule.lhs) {
    return Err(format!("Invalid lhs: {}", &rule.lhs));
  }
  let mut hoisted_fns = Vec::new();
  hoist_matches(rule.rhs.as_mut(), fn_name, &mut hoisted_fns);
  let lhs = compile_expr(&rule.lhs)?;
  let rhs = compile_expr(&rule.rhs)?;
  let main_rule = hvm::Rule { lhs, rhs };
  let mut rules = Vec::new();
  rules.push(main_rule);
  for func in hoisted_fns {
    let hoisted_file = compile_function(func)?;
    rules.extend(hoisted_file.rules);
  }
  Ok(hvm::File { rules, smaps: Vec::new() })
}

pub fn compile_expr(expr: &fun::Expr) -> Result<Box<hvm::Term>, String> {
  use ::hvm::Term;
  use fun::Expr;
  let term = match expr {
    Expr::Unit => Term::Ctr { name: "Unit".to_string(), args: Vec::new() },
    Expr::Ctr { name, args: fun_args } => {
      let mut args = Vec::with_capacity(fun_args.len());
      for arg in fun_args {
        let arg = compile_expr(arg)?;
        args.push(arg);
      }
      Term::Ctr { name: name.clone(), args }
    }
    Expr::FunCall { name, args: fun_args } => {
      let mut args = Vec::with_capacity(fun_args.len());
      for arg in fun_args {
        let arg = compile_expr(arg)?;
        args.push(arg);
      }
      Term::Ctr { name: name.clone(), args }
    }
    Expr::Let { name, expr, body } => {
      Term::Let { name: name.clone(), expr: compile_expr(expr)?, body: compile_expr(body)? }
    }
    Expr::App { expr, argm } => Term::App { func: compile_expr(expr)?, argm: compile_expr(argm)? },
    Expr::Var { name } => Term::Var { name: name.clone() },
    Expr::Unsigned { numb } => Term::U6O { numb: u60::new(*numb) },
    Expr::Float { numb } => Term::F6O { numb: f60::new(*numb) },
    Expr::BinOp { op, left, right } => {
      Term::Op2 { oper: *op, val0: compile_expr(left)?, val1: compile_expr(right)? }
    }
    Expr::Lambda { var, body } => Term::Lam { name: var.clone(), body: compile_expr(body)? },
    Expr::MatchExpr { .. } => {
      unreachable!("Match expressions should be hoisted before converting to HVM")
    }
  };
  Ok(Box::new(term))
}

pub fn is_valid_lhs(expr: &fun::Expr) -> bool {
  // Valid left-hand side expressions are a constructor with a list of valid patterns
  if let fun::Expr::Ctr { args, .. } = expr {
    args.iter().map(is_valid_pat).all(|x| x)
  } else {
    false
  }
}

pub fn is_valid_pat(expr: &fun::Expr) -> bool {
  // Valid patterns must be in normal form, and not introduce any variables
  use fun::Expr;
  match expr {
    Expr::Var { .. } => true,
    Expr::Unit => true,
    Expr::Unsigned { .. } => true,
    Expr::Float { .. } => true,
    Expr::Ctr { args, .. } => args.iter().map(is_valid_pat).all(|x| x),
    Expr::Let { .. } => false,
    Expr::Lambda { .. } => false,
    Expr::FunCall { .. } => false,
    Expr::App { .. } => false,
    Expr::BinOp { .. } => false,
    Expr::MatchExpr { .. } => false,
  }
}

pub fn hoist_matches(expr: &mut fun::Expr, fn_name: &fun::Id, hoisted: &mut Vec<fun::Function>) {
  // TODO: There are probably situations where we also want to hoist lambdas
  use fun::Expr;
  match expr {
    Expr::Unit | Expr::Var { .. } | Expr::Unsigned { .. } | Expr::Float { .. } => (),
    Expr::Ctr { args, .. } => {
      for arg in args {
        hoist_matches(arg, fn_name, hoisted);
      }
    }
    Expr::FunCall { args, .. } => {
      for arg in args {
        hoist_matches(arg, fn_name, hoisted);
      }
    }
    Expr::Let { expr, body, .. } => {
      hoist_matches(expr, fn_name, hoisted);
      hoist_matches(body, fn_name, hoisted);
    }
    Expr::App { expr, argm } => {
      hoist_matches(expr, fn_name, hoisted);
      hoist_matches(argm, fn_name, hoisted);
    }
    Expr::BinOp { left, right, .. } => {
      hoist_matches(left, fn_name, hoisted);
      hoist_matches(right, fn_name, hoisted);
    }
    Expr::Lambda { body, .. } => hoist_matches(body, fn_name, hoisted),
    Expr::MatchExpr { scrutinee, cases } => {
      // TODO: Can all match expressions be mapped to HVM's pattern matching?
      // maybe we should check whether it can be hoisted or not
      // Figure out which vars of the context need to be passed
      let mut ctx_names = HashSet::new();
      for case in cases.iter() {
        let pat_vars = case.matched.get_unbound_vars();
        let body_vars = case.body.get_unbound_vars();
        let diff: HashSet<String> = body_vars.difference(&pat_vars).map(String::clone).collect();
        ctx_names.extend(diff);
      }
      let ctx_vars = ctx_names.iter().map(|x| Expr::Var { name: x.clone() });
      // Create the hoisted function
      let aux_name = format!("{}.{}", fn_name, hoisted.len());
      let num_args = ctx_vars.len() + 1; // TODO: check if num_args > 16
      let mut rules = Vec::with_capacity(cases.len());
      for case in cases {
        let mut pats = Vec::with_capacity(num_args);
        pats.push(*case.matched.clone());
        pats.extend(ctx_vars.clone());
        let lhs = Box::new(Expr::Ctr { name: aux_name.clone(), args: pats });
        let rhs = case.body.clone();
        rules.push(fun::Rule { lhs, rhs });
      }
      let mut args = Vec::with_capacity(num_args);
      args.push("matched".to_string());
      args.extend(ctx_names.clone());
      let new_fn = fun::Function { name: fn_name.clone(), args, rules };
      hoisted.push(new_fn);
      // Replace the expression with a function call
      let mut args = Vec::with_capacity(num_args);
      args.push(*scrutinee.clone());
      args.extend(ctx_vars);
      *expr = Expr::FunCall { name: aux_name, args };
    }
  }
}