use crate::fun::{Expr, Id};

#[derive(Clone, Debug)]
pub enum Imp {
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
    default: StmtBlock,
  },
  IfElse {
    condition: Expr,
    true_case: StmtBlock,
    false_case: StmtBlock,
  },
  ForElse {
    initialize: Box<Imp>,
    condition: Expr,
    afterthought: Box<Imp>,
    body: StmtBlock,
    else_case: StmtBlock,
  },
  ForInElse {
    target: Id,
    iterator: Expr,
    body: StmtBlock,
    else_case: StmtBlock,
  },
  WhileElse {
    condition: Expr,
    body: StmtBlock,
    else_case: StmtBlock,
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
    body: StmtBlock,
  },
  Continue,
  Break,
  Pass,
}

pub type StmtBlock = Vec<Imp>;

#[derive(Clone, Debug)]
pub struct CaseStmt {
  pub matched: Expr,
  pub body: StmtBlock,
}

#[derive(Clone, Debug)]
pub struct Procedure {
  pub name: Id,
  pub args: Vec<Id>,
  pub body: StmtBlock,
}

pub struct Program(pub Vec<Imp>);
