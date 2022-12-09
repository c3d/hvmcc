pub use hvm::syntax::Oper; // use hvm's native rule struct.

pub type Id = String;

#[derive(Clone, Debug)]
pub enum Expr {
  Unit,
  Ctr { name: Id, args: Vec<Expr> }, // Datatype Haskell
  FunCall { name: Id, args: Vec<Expr> }, // Function that pattern matches
  Let { name: Id, expr: Box<Expr>, body: Box<Expr> },
  App { expr: Box<Expr>, argm: Box<Expr> },
  Var { name: Id },
  Unsigned { numb: u64 },
  Float { numb: u64 },
  BinOp { op: Oper, left: Box<Expr>, right: Box<Expr> },
  Lambda { var: Id, body: Box<Expr> },
  MatchExpr { scrutinee: Box<Expr>, cases: Vec<CaseExpr> },
}

#[derive(Clone, Debug)]
pub struct Function {
  pub name: Id,
  pub rules: Vec<Rule>,
}

#[derive(Clone, Debug)]
pub struct Rule {
  pub lhs: Box<Expr>,
  pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct CaseExpr {
  pub matched: Box<Expr>,
  pub body: Box<Expr>,
}
