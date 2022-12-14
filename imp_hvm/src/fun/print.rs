use crate::{Expr, CaseExpr};


// Display trait.
// Debugging purposes

impl std::fmt::Display for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fn display_args(args: &[Expr]) -> String {
      args.iter().map(|x| format!(" {}", x)).collect::<Vec<String>>().join(" ")
    }
    match self {
      Expr::Unit => write!(f, "Unit"),
      Expr::Ctr { name, args } => write!(f, "{{{}{}}}", name, display_args(args)),
      Expr::FunCall { name, args } => write!(f, "({}{})", name, display_args(args)),
      Expr::Let { name, expr, body } => write!(f, "let {} = {};\n{}", name, expr, body),
      Expr::App { expr, argm } => write!(f, "(!{} {})", expr, argm),
      Expr::Var { name } => write!(f, "{}", name),
      Expr::Unsigned { numb } => write!(f, "{}", *numb),
      Expr::Float { numb } => write!(f, "{}", *numb),
      Expr::BinOp { op, left, right } => write!(f, "({} {} {})", op, left, right),
      Expr::Lambda { var, body } => write!(f, "λ{} {}", var, body),
      Expr::MatchExpr { scrutinee, cases } => {
        let cases = cases.iter().map(CaseExpr::to_string).collect::<Vec<String>>().join("\n");
        write!(f, "match {} {{ {} }}", scrutinee, cases)
      },
    }
  }
}

impl std::fmt::Display for CaseExpr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let CaseExpr { matched, body } = self;
    write!(f, "{} => {}", matched, body)
  }
}
