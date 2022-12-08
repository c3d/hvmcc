use crate::imp::Id;
use hvm::Term as Term;
pub use hvm::syntax::Rule; // use hvm's native rule struct.

pub enum Fun {
  Unit,
  Ctr { name: Id, args: Vec<Fun> }, // Datatype Haskell
  FunCall { name: Id, args: Vec<Fun> }, // Function that pattern matches
  Let { name: Id, expr: Box<Fun>, body: Box<Fun> },
  App { expr: Box<Fun>, args: Box<Fun> },
  Var { name: Id },
  Number { numb: u64 },
  Float { numb: u64 }, 
  BinOp { op: Oper, left: Box<Fun>, right: Box<Fun> },
  Lambda { var: Id, body: Box<Fun> },
  MatchExpr { scrutinee: Box<Fun>, cases: Vec<CaseExpr> },
}

pub struct CaseExpr {
  matched: Fun,
  body: Fun,
}

pub fn fun_to_hvm(functional: Fun) -> Term {
  todo!()
}

pub enum Oper {
  Add, Sub, Mul, Div,
  Mod, And, Or,  Xor,
  Shl, Shr, Lte, Ltn,
  Eql, Gte, Gtn, Neq,
}
