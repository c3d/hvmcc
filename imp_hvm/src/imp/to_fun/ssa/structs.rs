use crate::fun::{Expr, Id, Oper};
use crate::CaseExpr;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type PhiRef = Rc<RefCell<Phi>>;
pub type BlockId = usize;

#[derive(Debug, Clone)]
pub struct Phi {
  pub name: Id,
  pub blk_id: BlockId,
  pub ops: Vec<Rc<Operand>>,
  pub users: HashMap<Id, PhiRef>,
}

impl Phi {
  pub fn new(name: Id, blk_id: BlockId) -> Self {
    Phi { name, blk_id, ops: vec![], users: HashMap::new() }
  }

  pub fn add_operand(&mut self, op: Rc<Operand>) {
    if let Operand::Phi { phi } = &*op {
      self.users.insert(phi.borrow().name.clone(), phi.clone());
    }
    self.ops.push(op);
  }

  pub fn replace_by(&mut self, operand: Rc<Operand>) {
    #[cfg(feature = "log")]
    println!("Replace by! {self:?} {*operand:?}");
  }
}

#[derive(Debug, Clone)]
pub enum Operand {
  Unit,
  Ctr { name: Id, args: Vec<Rc<Operand>> }, // Datatype Haskell
  FunCall { name: Id, args: Vec<Rc<Operand>> }, // Function that pattern matches
  Let { name: Id, expr: Rc<Operand>, body: Rc<Operand> },
  App { expr: Rc<Operand>, argm: Rc<Operand> },
  Var { name: Id },
  Unsigned { numb: u64 },
  Float { numb: f64 },
  BinOp { op: Oper, left: Rc<Operand>, right: Rc<Operand> },
  Lambda { var: Id, body: Rc<Operand> },
  MatchExpr { scrutinee: Rc<Operand>, cases: Vec<CaseOp> },
  Phi { phi: PhiRef },
  Undef,
  Parameter, // If a variable is the parameter of a match
}

impl From<Expr> for Rc<Operand> {
  fn from(value: Expr) -> Self {
    match value {
      Expr::App { expr, argm } => Operand::App { expr: expr.into(), argm: argm.into() },
      Expr::BinOp { op, left, right } => {
        Operand::BinOp { op, left: left.into(), right: right.into() }
      }
      Expr::Ctr { name, args } => {
        Operand::Ctr { name, args: args.into_iter().map(|x| x.into()).collect() }
      }
      Expr::Float { numb } => Operand::Float { numb },
      Expr::FunCall { name, args } => {
        Operand::FunCall { name, args: args.into_iter().map(|x| x.into()).collect() }
      }
      Expr::Lambda { var, body } => Operand::Lambda { var, body: body.into() },
      Expr::Let { name, expr, body } => Operand::Let { name, expr: expr.into(), body: body.into() },
      Expr::MatchExpr { scrutinee, cases } => Operand::MatchExpr {
        scrutinee: scrutinee.into(),
        cases: cases.into_iter().map(|x| x.into()).collect(),
      },
      Expr::Unit => Operand::Unit,
      Expr::Unsigned { numb } => Operand::Unsigned { numb },
      Expr::Var { name } => Operand::Var { name },
    }.into()
  }
}

impl From<Box<Expr>> for Rc<Operand> {
  fn from(value: Box<Expr>) -> Self {
    (*value).into()
  }
}

#[derive(Debug, Clone)]
pub struct CaseOp {
  pub matched: Rc<Operand>,
  pub body: Rc<Operand>,
}

impl From<CaseExpr> for CaseOp {
  fn from(value: CaseExpr) -> Self {
    CaseOp { matched: value.matched.into(), body: value.body.into() }
  }
}

#[derive(Debug, Clone)]
pub struct Block {
  pub id: BlockId,
  pub preds: Vec<BlockId>,
  pub assignments: Vec<(Id, Rc<Operand>)>,
}

#[derive(Debug, Clone)]
pub enum BlockKind {
  Simple,
  Match { cond: Rc<Operand>, cases: Vec<(Rc<Operand>, BlockId)>, default: BlockId },
  Jump { dest: BlockId },
  Return { dest: BlockId, val: Rc<Operand> },
}

impl Block {
  pub fn new(id: BlockId, preds: Vec<BlockId>) -> Self {
    Block { id, preds: preds.to_vec(), assignments: vec![] }
  }

  pub fn add_assignment(&mut self, name: Id, expr: Rc<Operand>) {
    #[cfg(feature = "log")]
    println!("Adding assignment to block {name} {expr}");
    self.assignments.push((name, expr))
  }
}
