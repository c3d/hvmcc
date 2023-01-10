use crate::fun::{Expr, Id};

#[derive(Clone, Debug)]
pub enum Imp {
  Block {
    stmts: Vec<Imp>
  },
  Assignment {
    name: Id,
    expr: Expr,
  },
  Expression {
    expr: Expr,
  },
  MatchStmt {
    expr: Expr,
    cases: Vec<CaseStmt>,
    default: Box<Imp>,
  },
  IfElse {
    condition: Expr,
    true_case: Box<Imp>,
    false_case: Box<Imp>,
  },
  ForElse {
    initialize: Box<Imp>,
    condition: Expr,
    afterthought: Box<Imp>,
    body: Box<Imp>,
    else_case: Box<Imp>,
  },
  ForInElse {
    target: Id,
    iterator: Expr,
    body: Box<Imp>,
    else_case: Box<Imp>,
  },
  WhileElse {
    condition: Expr,
    body: Box<Imp>,
    else_case: Box<Imp>,
  },
  Label {
    name: Id,
    stmt: Box<Imp>,
  },
  Return {
    value: Expr,
  },
  Goto {
    name: Id,
  },
  ProcedureDef {
    name: Id,
    args: Vec<Id>,
    body: Box<Imp>,
  },
  Continue,
  Break,
  Pass,
}

#[derive(Clone, Debug)]
pub struct CaseStmt {
  pub matched: Expr,
  pub body: Imp,
}

#[derive(Clone, Debug)]
pub struct Procedure {
  pub name: Id,
  pub args: Vec<Id>,
  pub body: Imp,
}

pub struct Program(pub Vec<Imp>);
