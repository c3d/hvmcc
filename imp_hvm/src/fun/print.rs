use crate::imp::print::ind;
use crate::{CaseExpr, Expr, Function, Rule, FuncProgram};
use std::fmt;

pub fn pprint_fun(fun: &Expr, depth: usize) -> String {
  fn display_args(args: &[Expr]) -> String {
    args.iter().map(|x| format!(" {}", x)).collect::<Vec<String>>().join("")
  }
  match fun {
    Expr::Unit => ind("Unit", depth),
    Expr::Ctr { name, args } => {
      let lpar = ind("{", depth);
      let rpar = "}";
      let args = display_args(args);
      format!("{lpar}{name}{args}{rpar}")
    }
    Expr::FunCall { name, args } => {
      let lpar = ind("(", depth);
      let rpar = ")";
      let args = display_args(args);
      format!("{lpar}{name}{args}{rpar}")
    }
    Expr::Let { name, expr, body } => {
      let ilet = ind("let", depth);
      let body = pprint_fun(body, depth);
      format!("{ilet} {name} = {expr};\n{body}")
    }
    Expr::App { expr, argm } => {
      let lpar = ind("(", depth);
      let rpar = ")";
      format!("{lpar}!{expr} {argm}{rpar}")
    }
    Expr::Var { name } => ind(name, depth),
    Expr::Unsigned { numb } => ind(&hvm::u60::show(*numb), depth),
    Expr::Float { numb } => ind(&hvm::f60::show(hvm::f60::new(*numb)), depth),
    Expr::BinOp { op, left, right } => {
      let lpar = ind("(", depth);
      let rpar = ")";
      format!("{lpar}{op} {left} {right}{rpar}")
    }
    Expr::Lambda { var, body } => {
      let lam = ind("λ", depth);
      format!("{lam}{var} {body}")
    }
    Expr::MatchExpr { scrutinee, cases } => {
      let cases = cases
        .iter()
        .map(|x| display_case(x, depth + 2))
        .collect::<Vec<String>>()
        .join("\n");
      let imatch = ind("match", depth);
      format!("{imatch} {scrutinee} {{\n{}\n{}", cases, ind("}", depth))
    }
  }
}

fn display_case(case: &CaseExpr, depth: usize) -> String {
  let CaseExpr { matched, body } = case;
  let matched = pprint_fun(matched, depth);
  let body = pprint_fun(body, depth + 2);
  format!("{matched} =>\n{body}")
}

impl std::fmt::Display for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", pprint_fun(self, 0))
  }
}

impl fmt::Display for Rule {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} = {}", self.lhs, self.rhs)
  }
}

impl fmt::Display for Function {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let args = self.args.join("\n");
    let rules = self.rules.iter().map(Rule::to_string).collect::<Vec<String>>().join("\n");
    write!(f, "function {} ({}) {{\n{}\n}}", self.name, args, rules)
  }
}

impl fmt::Display for FuncProgram {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let program = self.0.iter().map(Function::to_string).collect::<Vec<_>>().join("\n\n");
    f.write_str(&program)
  }
}