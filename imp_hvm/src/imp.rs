use crate::fun::Fun;

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

pub struct CaseStmt {
  matched: Fun,
  body: Imp,
}

pub struct Procedure {
  name: Id,
  arguments: Vec<Id>,
  body: Imp,
}

pub type Program = Vec<Procedure>;

pub fn imp_to_fun(imperative: Imp) -> Fun {
  todo!()
}
