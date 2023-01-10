pub mod converter;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::fun::{Expr, Id};


#[derive(Debug, Clone)]
struct Phi {
    name: Id,
    block: Rc<Block>,
    operands: Vec<Rc<Operand>>,
    users: HashMap<Id, Rc<RefCell<Phi>>>,
}

#[derive(Debug, Clone)]
enum Operand {
    Phi(Rc<RefCell<Phi>>),
    Undef,
    Value,
}

impl Phi {
    fn new(name: Id, block: Rc<Block>) -> Self {
        Phi { name, block, operands: vec![], users: HashMap::new() }
    }

    fn add_operand(&mut self, op: Rc<Operand>) {
        if let Operand::Phi(phi) = &*op {
            self.users.insert(phi.borrow().name.clone(), phi.clone());
        }
        self.operands.push(op);
    }

    fn replace_by(&mut self, operand: Rc<Operand>) {
        #[cfg(feature = "log")]
        println!("Replace by! {self:?} {*operand:?}");
    }
}

#[derive(Debug, Clone)]
struct Block {
    id: u64,
    preds: Vec<Rc<Block>>,
    assignments: Vec<(Id, Expr)>,
}

impl Block {
    pub fn new(id: u64, blocks: &[Rc<Block>]) -> Self {
        Block { id, preds: blocks.to_vec(), assignments: vec![] }
    }

    fn get_preds(&self) -> &Vec<Rc<Block>> {
        &self.preds
    }

    fn add_assignment(&mut self, name: Id, expr: Expr) {
        #[cfg(feature = "log")]
        println!("Adding assignment to block {name} {expr}");
        self.assignments.push((name, expr))
    }
}