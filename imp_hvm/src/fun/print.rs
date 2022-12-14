use crate::{Expr, CaseExpr};
use crate::imp::print::ind;

pub fn pprint_fun(fun: &Expr, depth: usize) -> String {
  fn display_args(args: &[Expr]) -> String {
    args.iter().map(|x| format!(" {}", x)).collect::<Vec<String>>().join("")
  }
  match fun {
    Expr::Unit => ind("Unit", depth),
    Expr::Ctr { name, args } => {
      let lpar = ind("(", depth);
      let rpar = ")";
      let args = display_args(args);
      format!("{lpar}{name}{args}{rpar}")
    },
    Expr::FunCall { name, args } => {
      let lpar = ind("(", depth);
      let rpar = ")";
      let args = display_args(args);
      format!("{lpar}{name}{args}{rpar}")
    },
    Expr::Let { name, expr, body } => {
      let ilet = ind("let", depth);
      let body = pprint_fun(body, depth);
      format!("{ilet} {name} = {expr};\n{body}")
    },
    Expr::App { expr, argm } => {
      let lpar = ind("(", depth);
      let rpar = ")";
      format!("{lpar}!{expr} {argm}{rpar}")
    },
    Expr::Var { name } => ind(name, depth),
    Expr::Unsigned { numb } => ind(&hvm::u60::show(*numb), depth),
    Expr::Float { numb } => ind(&hvm::f60::show(hvm::f60::new(*numb)), depth),
    Expr::BinOp { op, left, right } => {
      let lpar = ind("(", depth);
      let rpar = ")";
      format!("{lpar}{op} {left} {right}{rpar}")
    },
    Expr::Lambda { var, body } => {
      let lam = ind("λ", depth);
      format!("{lam}{var} {body}")
    },
    Expr::MatchExpr { scrutinee, cases } => {
      fn display_case(case: &CaseExpr, depth: usize) -> String {
        let CaseExpr { matched, body } = case;
        let matched = pprint_fun(matched, depth);
        let body = pprint_fun(body, depth+2);
        format!("{matched} =>\n{body}")
      }
      let cases = cases.iter().map(|x| display_case(x, depth+2)).collect::<Vec<String>>().join("\n");
      let imatch = ind("match", depth);
      format!("{imatch} {scrutinee} {{\n{}\n{}", cases, ind("}", depth))
    },
  }
}

impl std::fmt::Display for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", pprint_fun(self, 0))
  }
}

impl std::fmt::Display for CaseExpr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let CaseExpr { matched, body } = self;
    write!(f, "{} => {}", matched, body)
  }
}
