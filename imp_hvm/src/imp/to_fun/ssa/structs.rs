use crate::fun::{Id, Oper};
use std::collections::{HashMap, HashSet};

pub type BlockId = usize;
pub type ConvertResult<T> = Result<T, String>;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaId(pub usize);

#[derive(Debug, Clone)]
pub struct Phi {
  pub name: Id,
  pub ops: Vec<SsaId>,
  pub users: HashSet<SsaId>,
}

#[derive(Debug, Clone)]
pub enum Operand {
  Phi(Phi),
  Value(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
  Unit,
  Ctr { name: Id, args: Vec<Expr> }, // Datatype Haskell
  FunCall { name: Id, args: Vec<Expr> }, // Function that pattern matches
  Let { name: Id, expr: Box<Expr>, body: Box<Expr> },
  App { expr: Box<Expr>, argm: Box<Expr> },
  Var { name: SsaId },
  Unsigned { numb: u64 },
  Float { numb: f64 },
  BinOp { op: Oper, left: Box<Expr>, right: Box<Expr> },
  Lambda { var: Id, body: Box<Expr> },
  MatchExpr { scrutinee: Box<Expr>, cases: Vec<CaseOp> },
  Parameter, // If a variable is the parameter of a match
}

#[derive(Debug, Clone)]
pub struct CaseOp {
  pub matched: Box<Expr>,
  pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Assignment {
  pub id: SsaId,
  pub blk: BlockId,
  pub var: Id,
  pub op: Operand
}

#[derive(Debug, Clone)]
pub struct Block {
  pub id: BlockId,
  pub preds: Vec<BlockId>,
  pub assignments: Vec<SsaId>,
  pub crnt_defs: HashMap<Id, SsaId>,
  pub incomplete_phis: HashMap<Id, SsaId>,
}

#[derive(Debug, Clone)]
pub enum BlockKind {
  Simple,
  Match { cond: Box<Expr>, cases: Vec<(Expr, BlockId)>, default: BlockId },
  Jump { dest: BlockId },
  Return { val: Box<Expr> },
}

impl Block {
  pub fn new(id: BlockId, preds: Vec<BlockId>) -> Self {
    Block { id, preds, assignments: Vec::new(), crnt_defs: HashMap::new(), incomplete_phis: HashMap::new() }
  }
}
