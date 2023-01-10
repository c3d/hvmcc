use super::*;
use crate::fun::Id;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct BraunConverter {
  crnt_def: HashMap<Id, HashMap<u64, Rc<Operand>>>,
  incomplete_phis: HashMap<u64, HashMap<Id, Rc<RefCell<Phi>>>>,
  sealed_blocks: HashMap<u64, Rc<RefCell<Block>>>,
}

impl BraunConverter {
  pub fn new() -> Self {
    BraunConverter {
      crnt_def: HashMap::new(),
      incomplete_phis: HashMap::new(),
      sealed_blocks: HashMap::new(),
    }
  }

  pub fn write_var(&mut self, name: &Id, block: Rc<RefCell<Block>>, value: Rc<Operand>) {
    #[cfg(feature = "log")]
    println!("write_var {name} {block:?} {value:?}");
    if let Some(vals) = self.crnt_def.get_mut(name) {
      vals.insert(block.borrow().id, value);
    } else {
      let vals = HashMap::from([(block.borrow().id, value)]);
      self.crnt_def.insert(name.clone(), vals);
    }
  }

  pub fn read_var(&mut self, name: &Id, block: Rc<RefCell<Block>>) -> Rc<Operand> {
    #[cfg(feature = "log")]
    println!("read_var {name} {block:?}");
    if let Some(vals) = self.crnt_def.get(name) {
      if let Some(val) = vals.get(&block.borrow().id) {
        return val.clone();
      }
    }
    self.read_var_recursive(name, block)
  }

  fn read_var_recursive(&mut self, name: &Id, block: Rc<RefCell<Block>>) -> Rc<Operand> {
    let mut value: Rc<Operand>;
    let blk_id = { block.borrow().id };
    if !self.sealed_blocks.contains_key(&blk_id) {
      let phi = Rc::new(RefCell::new(Phi::new(name.clone(), block.clone())));
      value = Rc::new(Operand::Phi { phi: phi.clone() });
      if let Some(vals) = self.incomplete_phis.get_mut(&blk_id) {
        vals.insert(name.clone(), phi);
      } else {
        let vals = HashMap::from([(name.clone(), phi)]);
        self.incomplete_phis.insert(blk_id, vals);
      }
    } else {
      let preds = &block.borrow().preds;
      if preds.len() == 1 {
        value = self.read_var(name, preds[0].clone());
      } else {
        let phi = Rc::new(RefCell::new(Phi::new(name.clone(), block.clone())));
        value = Rc::new(Operand::Phi { phi: phi.clone() });
        self.write_var(name, block.clone(), value);
        value = self.add_phi_operands(name, phi);
      }
    }
    self.write_var(name, block, value.clone());
    value
  }

  fn add_phi_operands(&mut self, name: &Id, phi: Rc<RefCell<Phi>>) -> Rc<Operand> {
    {
      let preds = {
        let phi = phi.borrow();
        let block = phi.block.borrow();
        block.preds.clone()
      };
      for pred in preds {
        phi.borrow_mut().add_operand(self.read_var(name, pred.clone()));
      }
    }
    self.try_remove_trivial_phi(phi)
  }

  fn try_remove_trivial_phi(&self, phi: Rc<RefCell<Phi>>) -> Rc<Operand> {
    let mut same: Option<Rc<Operand>> = None;
    {
      let phi_ = phi.borrow();
      for op in phi_.operands.iter() {
        match (&**op, same.as_deref()) {
          (Operand::Phi { phi: op }, _) if op.borrow().name == phi_.name => continue,
          (Operand::Phi { phi: op }, Some(Operand::Phi { phi: same }))
            if op.borrow().name == same.borrow().name =>
          {
            continue
          }
          (Operand::Undef, Some(Operand::Undef)) => continue,
          _ => (),
        }
        if same.is_some() {
          return Rc::new(Operand::Phi { phi: phi.clone() });
        }
        same = Some(op.clone());
      }
    }
    // If the phi is unrecheable or in the start block
    let same = same.unwrap_or(Rc::new(Operand::Undef));

    // Remember all users except the phi itself
    let users = {
      let phi = phi.borrow();
      let mut users = phi.users.clone();
      users.remove(&phi.name);
      users
    };

    // Reroute all uses of phi to same and remove phi
    {
      phi.borrow_mut().replace_by(same.clone());
    }

    // Try to recursively remove all phi users, which might have become trivial
    for user in users.into_values() {
      self.try_remove_trivial_phi(user);
    }
    same
  }

  pub fn seal_block(&mut self, block: Rc<RefCell<Block>>) {
    let blk_id = { block.borrow().id };
    let phis = if let Some(phis) = self.incomplete_phis.get(&blk_id) {
      let phis: Vec<_> = phis.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
      Some(phis)
    } else {
      None
    };
    // This cloning, wrapping and unwrapping is only to make the borrow checker happy
    if let Some(phis) = phis {
      for (name, phi) in phis {
        self.add_phi_operands(&name, phi.clone());
      }
      self.incomplete_phis.remove(&blk_id);
    }
    self.sealed_blocks.insert(blk_id, block);
  }
}
