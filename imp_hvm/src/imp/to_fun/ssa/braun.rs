use super::*;
use crate::fun::{self, Id};
use std::collections::HashSet;

#[derive(Debug)]
pub struct BraunConverter {
  pub blocks: Vec<Block>,
  pub assignments: Vec<Assignment>,
  sealed_blocks: HashSet<BlockId>,
}

impl BraunConverter {
  pub fn new() -> Self {
    BraunConverter {
      blocks: Vec::new(),
      assignments: Vec::new(),
      sealed_blocks: HashSet::new(),
    }
  }

  fn write_var(&mut self, name: Id, blk_id: BlockId, ssa_name: SsaId) {
    self.blocks[blk_id].crnt_defs.insert(name, ssa_name);
  }

  fn read_var(&mut self, name: &Id, blk_id: BlockId) -> ConvertResult<SsaId> {
    if let Some(val) = self.blocks[blk_id].crnt_defs.get(name) {
      // Local value numbering, definition already in block
      Ok(*val)
    } else {
      // Global value numbering, look recursively for the definition in predecessors
      // In case of branching, this will insert phis
      let blk_is_sealed = self.sealed_blocks.contains(&blk_id);
      let value = if blk_is_sealed {
        if self.blocks[blk_id].preds.len() == 1 {
          // If the block has a single predecessor, just query it directly
          self.read_var(name, self.blocks[blk_id].preds[0])?
        } else {
          // Otherwise, we collect the definitions from all predecessors
          // and construct a φ function, which joins them into a single new value.
          // This φ function is recorded as current definition in this block.
          let value = self.new_phi(name.clone(), blk_id);
          // Looking for a value in a predecessor might lead to more recursive look-ups.
          // Due to loops in the program, those might lead to endless recursion.
          // So, before recursing, we first create the φ function without operands
          // and record it as the current definition for the variable in the block.
          // Then, we determine the φ function’s operands.
          // If a recursive look-up arrives back at the block,
          // this φ function will provide a definition and the recursion will end.
          self.write_var(name.clone(), blk_id, value);
          self.add_phi_operands(value)?;
          value
        }
      } else {
        // In case of an unsealed block with no local definition of this var,
        // we place an operandless φ function into the block
        // and record it as proxy definition
        let value = self.new_phi(name.clone(), blk_id);
        self.blocks[blk_id].incomplete_phis.insert(name.clone(), value);
        value
      };
      // Write the definition found in predecessor in the current block
      self.write_var(name.clone(), blk_id, value);
      Ok(value)
    } 
  }

  fn add_phi_operands(&mut self, phi_id: SsaId) -> ConvertResult<()> {
    let blk_id = self.assignments[phi_id.0].blk;
    // Check all predecessors for possible operands of the phi
    for pred in self.blocks[blk_id].preds.clone() {
      let name = self.get_phi_ref(phi_id).unwrap().name.clone();
      let val = self.read_var(&name, pred)?;
      let pred_is_phi = matches!(self.assignments[val.0].op, Operand::Phi(_));
      let phi = self.get_phi_ref(phi_id).unwrap();
      // Add operand
      phi.ops.push(val);
      // Add user if op is a phi
      if pred_is_phi {
        phi.users.insert(val);
      }
    }
    self.try_remove_trivial_phi(phi_id)
  }

  /// Recursive look-up might leave redundant φ functions.
  /// We call a φ function v_φ trivial iff it just references itself
  /// and one other value v any number of times.
  /// Such a φ function can be removed and the value v is used instead;
  fn try_remove_trivial_phi(&mut self, phi_id: SsaId) -> ConvertResult<()> {
    // Search all unique and not self-referential values
    let uniques: HashSet<SsaId> = self.get_phi_ref(phi_id).unwrap().ops.iter().map(|x| x.clone()).filter(|op| op != &phi_id).collect();

    // Phis with 0 or 1 operand are trivial
    if uniques.len() < 2 {
      if let Some(unique) = uniques.iter().next() {
        // If the phi only points to a single value,
        // we replace it by an indirection to that value
        self.assignments[phi_id.0].op = Operand::Value(Expr::Var { name: *unique }.into());
        // Remove self from users
        let users = &mut self.get_phi_ref(phi_id).unwrap().users;
        users.remove(&phi_id);
        // Check all other phi users, which might have become trivial
        for user in users.clone() {
          self.try_remove_trivial_phi(user)?;
        }
        Ok(())
      } else {
        // If the φ function has no operand other than itself,
        // it means that it is either unreachable or in the start block.
        // In this case, it is an undefined value, so we error
        Err("Undefined reference found".into())
      }
    } else {
      Ok(())
    }
  }

  /// We call a basic block sealed if no further predecessors will be added to the block.
  pub fn seal_block(&mut self, blk_id: BlockId) -> ConvertResult<()> {
    // Add operands to every incomplete phi of this block, making them complete
    // TODO: This cloning is only to make the borrow checker happy,
    // but I'm not sure if this is totally correct.
    // What if adding an operand creates a new phi in this block?
    let phis: Vec<_> = self.blocks[blk_id].incomplete_phis.values().map(|x| x.clone()).collect();
    for phi in phis {
      self.add_phi_operands(phi)?;
    }
    // Seal the block
    self.sealed_blocks.insert(blk_id);
    Ok(())
  }

  pub fn new_block(&mut self, preds: Vec<BlockId>) -> BlockId {
    let id = self.blocks.len();
    let block = Block::new(id, preds);
    self.blocks.push(block);
    id
  }

  pub fn new_sealed_block(&mut self, preds: Vec<BlockId>) -> ConvertResult<BlockId> {
    let blk_id = self.new_block(preds);
    self.seal_block(blk_id)?;
    Ok(blk_id)
  }

  pub fn convert_expr(&mut self, expr: &fun::Expr, blk_id: BlockId) -> ConvertResult<Box<Expr>> {
    match expr {
      fun::Expr::Var { name } => {
        let name = self.read_var(&name, blk_id)?;
        Ok(Expr::Var { name }.into())
      }
      fun::Expr::Unsigned { numb } => Ok(Expr::Unsigned { numb: *numb }.into()),
      fun::Expr::BinOp { op, left, right } => {
        let left = self.convert_expr(left, blk_id)?;
        let right = self.convert_expr(right, blk_id)?;
        Ok(Expr::BinOp { op: *op, left, right }.into())
      }
      _ => Err("Other expressions not covered yet".into()),
    }
  }

  pub fn convert_pat(&mut self, expr: &fun::Expr, blk_id: BlockId) -> ConvertResult<Box<Expr>> {
    match expr {
      fun::Expr::Var { name } => {
        let val = Operand::Value(Expr::Parameter.into());
        let name = self.add_assignment(blk_id, name.clone(), val);
        Ok(Expr::Var { name }.into())
      }
      fun::Expr::Ctr { name, args } => {
        let mut args_conv = vec![];
        for arg in args {
          let arg = *self.convert_pat(arg, blk_id)?;
          args_conv.push(arg);
        }
        Ok(Expr::Ctr { name: name.clone(), args: args_conv }.into())
      }
      fun::Expr::Unit => Ok(Expr::Unit.into()),
      fun::Expr::Unsigned { numb } => Ok(Expr::Unsigned { numb: *numb }.into()),
      _ => Err("Invalid expression in pattern".into())
    }
  }

  pub fn add_assignment(&mut self, blk_id: BlockId, name: Id, val: Operand) -> SsaId {
    let new_id = SsaId(self.assignments.len());
    let assignment = Assignment {
      id: new_id,
      blk: blk_id,
      var: name.clone(),
      op: val,
    };
    self.assignments.push(assignment);
    self.blocks[blk_id].assignments.push(new_id);
    self.write_var(name, blk_id, new_id);
    new_id
  }

  pub fn new_phi(&mut self, name: Id, blk_id: BlockId) -> SsaId {
    let phi = Phi{ name: name.clone(), ops: vec![], users: HashSet::new() };
    let phi_op = Operand::Phi(phi);
    let ssa_id = self.add_assignment(blk_id, name.clone(), phi_op);
    ssa_id
  }

  fn get_phi_ref(&mut self, phi_id: SsaId) -> Option<&mut Phi> {
    if let Operand::Phi(phi) = &mut self.assignments[phi_id.0].op {
      Some(phi)
    } else {
      None
    }
  }
}
