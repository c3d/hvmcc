use crate::fun::{Expr, Id, Oper};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Phi {
  pub name: Id,
  pub block: Rc<RefCell<Block>>,
  pub operands: Vec<Rc<Operand>>,
  pub users: HashMap<Id, Rc<RefCell<Phi>>>,
}

#[derive(Debug, Clone)]
pub enum Operand {
  Phi { phi: Rc<RefCell<Phi>> },
  Undef,
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
}

#[derive(Debug, Clone)]
pub struct CaseOp {
  pub matched: Rc<Operand>,
  pub body: Rc<Operand>,
}

impl Phi {
  pub fn new(name: Id, block: Rc<RefCell<Block>>) -> Self {
    Phi { name, block, operands: vec![], users: HashMap::new() }
  }

  pub fn add_operand(&mut self, op: Rc<Operand>) {
    if let Operand::Phi { phi } = &*op {
      self.users.insert(phi.borrow().name.clone(), phi.clone());
    }
    self.operands.push(op);
  }

  pub fn replace_by(&mut self, operand: Rc<Operand>) {
    #[cfg(feature = "log")]
    println!("Replace by! {self:?} {*operand:?}");
  }
}

#[derive(Debug, Clone)]
pub struct Block {
  pub id: u64,
  pub preds: Vec<Rc<RefCell<Block>>>,
  pub assignments: Vec<(Id, Rc<Operand>)>,
}

impl Block {
  pub fn new(id: u64, blocks: &[Rc<RefCell<Block>>]) -> Self {
    Block { id, preds: blocks.to_vec(), assignments: vec![] }
  }

  pub fn add_assignment(&mut self, name: Id, expr: Rc<Operand>) {
    #[cfg(feature = "log")]
    println!("Adding assignment to block {name} {expr}");
    self.assignments.push((name, expr))
  }
}
