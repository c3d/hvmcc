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
        // Local value numbering
        return val.clone();
      }
    }
    // Global value numbering
    self.read_var_recursive(name, block)
  }

  /// If a block currently contains no definition for a variable,
  /// we recursively look for a definition in its predecessors.
  fn read_var_recursive(&mut self, name: &Id, block: Rc<RefCell<Block>>) -> Rc<Operand> {
    let mut value: Rc<Operand>;
    let blk_id = { block.borrow().id };
    if !self.sealed_blocks.contains_key(&blk_id) {
      // But how to handle a look-up of a variable in an unsealed block,
      // which has no current definition for this variable?
      // In this case, we place an operandless φ function into the block
      // and record it as proxy definition
      let phi = Rc::new(RefCell::new(Phi::new(name.clone(), block.clone())));
      value = Rc::new(Operand::Phi { phi: phi.clone() });
      if let Some(vals) = self.incomplete_phis.get_mut(&blk_id) {
        vals.insert(name.clone(), phi);
      } else {
        let vals = HashMap::from([(name.clone(), phi)]);
        self.incomplete_phis.insert(blk_id, vals);
      }
    } else {
      let preds = { &block.borrow().preds };
      if preds.len() == 1 {
        // If the block has a single predecessor, just query it recursively for a definition.
        // (Optimized common case of one predecessor: No phi needed)
        value = self.read_var(name, preds[0].clone());
      } else {
        // Otherwise, we collect the definitions from all predecessors
        // and construct a φ function, which joins them into a single new value.
        // This φ function is recorded as current definition in this basic block.
        let phi = Rc::new(RefCell::new(Phi::new(name.clone(), block.clone())));
        value = Rc::new(Operand::Phi { phi: phi.clone() });
        // Looking for a value in a predecessor might in turn lead to further recursive look-ups.
        // Due to loops in the program, those might lead to endless recursion.
        // Therefore, before recursing, we first create the φ function without operands
        // and record it as the current definition for the variable in the block.
        // Then, we determine the φ function’s operands.
        // If a recursive look-up arrives back at the block,
        // this φ function will provide a definition and the recursion will end.
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

  /// Recursive look-up might leave redundant φ functions.
  /// We call a φ function v_φ trivial iff it just references itself
  /// and one other value v any number of times.
  /// Such a φ function can be removed and the value v is used instead;
  fn try_remove_trivial_phi(&self, phi: Rc<RefCell<Phi>>) -> Rc<Operand> {
    let mut same: Option<Rc<Operand>> = None;
    {
      let phi_ = phi.borrow();
      for op in phi_.operands.iter() {
        match (&**op, same.as_deref()) {
          // Unique value or self−reference
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
          // The phi merges at least two values: not trivial
          return Rc::new(Operand::Phi { phi: phi.clone() });
        }
        same = Some(op.clone());
      }
    }
    // As a special case, the φ function might use no other value besides itself.
    // This means that it is either unreachable or in the start block.
    // We replace it by an undefined value.
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

  /// We call a basic block sealed if no further predecessors will be added to the block.
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
