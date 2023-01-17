use super::*;
use crate::fun::Id;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct BraunConverter {
  crnt_def: HashMap<Id, HashMap<BlockId, Rc<Operand>>>,
  incomplete_phis: HashMap<BlockId, HashMap<Id, PhiRef>>,
  sealed_blocks: HashSet<BlockId>,
  pub blocks: Vec<Block>,
  blk_count: BlockId,
}

impl BraunConverter {
  pub fn new() -> Self {
    BraunConverter {
      crnt_def: HashMap::new(),
      incomplete_phis: HashMap::new(),
      sealed_blocks: HashSet::new(),
      blocks: Vec::new(),
      blk_count: 0,
    }
  }

  pub fn write_var(&mut self, name: &Id, blk_id: BlockId, value: Rc<Operand>) {
    #[cfg(feature = "log")]
    println!("write_var {name} {block:?} {value:?}");
    if let Some(vals) = self.crnt_def.get_mut(name) {
      vals.insert(blk_id, value);
    } else {
      let vals = HashMap::from([(blk_id, value)]);
      self.crnt_def.insert(name.clone(), vals);
    }
  }

  pub fn read_var(&mut self, name: &Id, blk_id: BlockId) -> Rc<Operand> {
    #[cfg(feature = "log")]
    println!("read_var {name} {block:?}");
    if let Some(vals) = self.crnt_def.get(name) {
      if let Some(val) = vals.get(&blk_id) {
        // Local value numbering
        return val.clone();
      }
    }
    // Global value numbering
    self.read_var_recursive(name, blk_id)
  }

  /// If a block currently contains no definition for a variable,
  /// we recursively look for a definition in its predecessors.
  fn read_var_recursive(&mut self, name: &Id, blk_id: BlockId) -> Rc<Operand> {
    let mut value: Rc<Operand>;
    if !self.sealed_blocks.contains(&blk_id) {
      // But how to handle a look-up of a variable in an unsealed block,
      // which has no current definition for this variable?
      // In this case, we place an operandless φ function into the block
      // and record it as proxy definition
      let phi = Rc::new(RefCell::new(Phi::new(name.clone(), blk_id)));
      value = Rc::new(Operand::Phi { phi: phi.clone() });
      if let Some(vals) = self.incomplete_phis.get_mut(&blk_id) {
        vals.insert(name.clone(), phi);
      } else {
        let vals = HashMap::from([(name.clone(), phi)]);
        self.incomplete_phis.insert(blk_id, vals);
      }
    } else {
      let preds = &self.blocks[blk_id].preds;
      if preds.len() == 1 {
        // If the block has a single predecessor, just query it recursively for a definition.
        // (Optimized common case of one predecessor: No phi needed)
        value = self.read_var(name, preds[0]);
      } else {
        // Otherwise, we collect the definitions from all predecessors
        // and construct a φ function, which joins them into a single new value.
        // This φ function is recorded as current definition in this basic block.
        let phi = Rc::new(RefCell::new(Phi::new(name.clone(), blk_id)));
        value = Rc::new(Operand::Phi { phi: phi.clone() });
        // Looking for a value in a predecessor might in turn lead to further recursive look-ups.
        // Due to loops in the program, those might lead to endless recursion.
        // Therefore, before recursing, we first create the φ function without operands
        // and record it as the current definition for the variable in the block.
        // Then, we determine the φ function’s operands.
        // If a recursive look-up arrives back at the block,
        // this φ function will provide a definition and the recursion will end.
        self.write_var(name, blk_id, value);
        value = self.add_phi_operands(name, phi);
      }
    }
    self.write_var(name, blk_id, value.clone());
    value
  }

  fn add_phi_operands(&mut self, name: &Id, phi: Rc<RefCell<Phi>>) -> Rc<Operand> {
    {
      let blk_id = { phi.borrow().blk_id };
      let preds = self.blocks[blk_id].preds.clone();
      for pred in preds {
        let val = self.read_var(name, pred);
        phi.borrow_mut().add_operand(val);
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
      for op in phi_.ops.iter() {
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
    let users: Vec<PhiRef> = {
      let mut phi = phi.borrow_mut();
      let name = phi.name.clone();
      phi.users.remove(&name);
      phi.users.values().map(|x| x.clone()).collect()
    };

    // Reroute all uses of phi to same and remove phi
    {
      phi.borrow_mut().replace_by(same.clone());
    }

    // Try to recursively remove all phi users, which might have become trivial
    for user in users {
      self.try_remove_trivial_phi(user);
    }
    same
  }

  /// We call a basic block sealed if no further predecessors will be added to the block.
  pub fn seal_block(&mut self, blk_id: BlockId) {
    // Add operands to every incomplete phi of this block, making them complete
    let phis = if let Some(phis) = self.incomplete_phis.get(&blk_id) {
      // This cloning is only to make the borrow checker happy
      phis.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    } else {
      vec![]
    };
    for (name, phi) in phis {
      self.add_phi_operands(&name, phi.clone());
    }
    self.incomplete_phis.remove(&blk_id);

    // Seal the block
    self.sealed_blocks.insert(blk_id);
  }

  pub fn new_block(&mut self, preds: Vec<BlockId>, kind: BlockKind) -> BlockId {
    let id = self.new_blk_id();
    let block = Block::new(id, preds, kind);
    self.blocks.push(block);
    id
  }

  pub fn new_blk_id(&mut self) -> BlockId {
    let n = self.blk_count;
    self.blk_count += 1;
    n
  }

  pub fn new_sealed_block(&mut self, preds: Vec<BlockId>, kind: BlockKind) -> BlockId {
    let blk_id = self.new_block(preds, kind);
    self.seal_block(blk_id);
    blk_id
  }
}
