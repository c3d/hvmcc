use hvm::Term as Term;

pub type Id = String;

pub enum Imp {
  Sequence { stmt1: Box<Imp>, stmt2: Box<Imp> },
  Assignment { name: Id, expr: Fun },
  Expression { expr: Fun },
  MatchStmt { expr: Fun, cases: Vec<CaseStmt>, default: Option<Box<Imp>> },
  IfElse { condition: Fun, true_case: Box<Imp>, false_case: Box<Imp> },
  ForElse { initialize: Box<Imp>, condition: Fun, afterthought: Box<Imp>, body: Box<Imp>, else_case: Box<Imp> },
  ForInElse { target: Id, iterator: Fun, body: Box<Imp>, else_case: Box<Imp> },
  WhileElse { condition: Fun, body: Box<Imp>, else_case: Box<Imp> },
  Label { name: Id, stmt: Box<Imp> },
  Return { value: Fun },
  Goto { name: Id },
  Continue, 
  Break,
  Pass,
}

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

pub enum Oper {
  Add, Sub, Mul, Div,
  Mod, And, Or,  Xor,
  Shl, Shr, Lte, Ltn,
  Eql, Gte, Gtn, Neq,
}

pub struct Rule {
  lhs: Fun,
  rhs: Fun,
}

pub struct CaseStmt {
  matched: Fun,
  body: Imp,
}

pub struct CaseExpr {
  matched: Fun,
  body: Fun,
}

pub struct Procedure {
  name: Id,
  arguments: Vec<Id>,
  body: Imp,
}
