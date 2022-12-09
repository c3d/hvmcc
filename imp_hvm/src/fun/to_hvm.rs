use crate::fun::syntax as fun;
use hvm::language::syntax as hvm;
use std::collections::HashSet;

pub fn compile_function(function: fun::Function) -> Result<hvm::File, ()> {
  let mut rules = Vec::new();
  for rule in function.rules {
    let file = compile_rule(&function.name, rule)?;
    rules.extend(file.rules);
  }
  Ok(hvm::File { rules, smaps: Vec::new() })
}

pub fn compile_rule(
  fn_name: &fun::Id,
  mut rule: fun::Rule,
) -> Result<hvm::File, ()> {
  if !is_valid_lhs(&rule.lhs) {
    return Err(());
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

pub fn compile_expr(expr: &fun::Expr) -> Result<Box<hvm::Term>, ()> {
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
    Expr::Let { name, expr, body } => Term::Let {
      name: name.clone(),
      expr: compile_expr(expr)?,
      body: compile_expr(body)?,
    },
    Expr::App { expr, argm } => {
      Term::App { func: compile_expr(expr)?, argm: compile_expr(argm)? }
    }
    Expr::Var { name } => Term::Var { name: name.clone() },
    Expr::Unsigned { numb } => Term::U6O { numb: *numb },
    Expr::Float { numb } => Term::F6O { numb: *numb },
    Expr::BinOp { op, left, right } => Term::Op2 {
      oper: *op,
      val0: compile_expr(left)?,
      val1: compile_expr(right)?,
    },
    Expr::Lambda { var, body } => {
      Term::Lam { name: var.clone(), body: compile_expr(body)? }
    }
    Expr::MatchExpr { .. } => unreachable!(
      "Match expressions should be hoisted before converting to HVM"
    ),
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

pub fn hoist_matches(
  expr: &mut fun::Expr,
  fn_name: &fun::Id,
  hoisted: &mut Vec<fun::Function>,
) {
  // TODO: There are probably situations where we also want to hoist lambdas
  use fun::Expr;
  match expr {
    Expr::Unit
    | Expr::Var { .. }
    | Expr::Unsigned { .. }
    | Expr::Float { .. } => (),
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
      let mut ctx = HashSet::new();
      for case in cases.iter() {
        let pat_vars = get_unbound_vars(&case.matched);
        let body_vars = get_unbound_vars(&case.body);
        let diff: HashSet<String> =
          body_vars.difference(&pat_vars).map(String::clone).collect();
        ctx.extend(diff);
      }
      let ctx = ctx.iter().map(|x| Expr::Var { name: x.clone() });
      // Create the hoisted function
      let aux_name = format!("{}.{}", fn_name, hoisted.len());
      let num_args = ctx.len() + 1; // TODO: check if num_args > 16
      let mut rules = Vec::with_capacity(cases.len());
      for case in cases {
        let mut pats = Vec::with_capacity(num_args);
        pats.push(*case.matched.clone());
        pats.extend(ctx.clone());
        let lhs = Box::new(Expr::Ctr { name: aux_name.clone(), args: pats });
        let rhs = case.body.clone();
        rules.push(fun::Rule { lhs, rhs });
      }
      let new_fn = fun::Function { name: fn_name.clone(), rules };
      hoisted.push(new_fn);
      // Replace the expression with a function call
      let mut args = Vec::with_capacity(num_args);
      args.push(*scrutinee.clone());
      args.extend(ctx);
      *expr = Expr::FunCall { name: aux_name, args };
    }
  }
}

/// Returns a set of variables that are not defined inside of the expression
pub fn get_unbound_vars(expr: &fun::Expr) -> HashSet<fun::Id> {
  use fun::Expr;
  match expr {
    Expr::Unit => HashSet::new(),
    Expr::Ctr { args, .. } => {
      let mut vars = HashSet::new();
      for arg in args {
        vars.extend(get_unbound_vars(arg));
      }
      vars
    }
    Expr::FunCall { args, .. } => {
      let mut vars = HashSet::new();
      for arg in args {
        vars.extend(get_unbound_vars(arg));
      }
      vars
    }
    Expr::Let { name, expr, body } => {
      let mut vars = get_unbound_vars(body);
      vars.remove(name); // The variable defined by the let is not unbound in its body
      vars.extend(get_unbound_vars(expr));
      vars
    }
    Expr::App { expr, argm } => {
      let mut vars = get_unbound_vars(expr);
      vars.extend(get_unbound_vars(argm).into_iter());
      vars
    }
    Expr::Var { name } => HashSet::from([name.clone()]),
    Expr::Unsigned { .. } => HashSet::new(),
    Expr::Float { .. } => HashSet::new(),
    Expr::BinOp { left, right, .. } => {
      let mut vars = get_unbound_vars(left);
      vars.extend(get_unbound_vars(right).into_iter());
      vars
    }
    Expr::Lambda { var, body } => {
      let mut vars = get_unbound_vars(body);
      vars.remove(var);
      vars
    }
    Expr::MatchExpr { scrutinee, cases } => {
      let mut vars = get_unbound_vars(scrutinee);
      for case in cases {
        let pat_vars = get_unbound_vars(&case.matched);
        let body_vars = get_unbound_vars(&case.body);
        let diff: HashSet<String> =
          body_vars.difference(&pat_vars).map(String::clone).collect();
        vars.extend(diff);
      }
      vars
    }
  }
}
