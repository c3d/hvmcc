use std::collections::HashSet;

use super::ssa;
use crate::fun::{Expr, FuncProgram, Id};

pub fn ssa_proc_to_fun(converter: ssa::Converter) -> FuncProgram {
  // For each block in reverse order,
  // w3e want to check the used variables,
  // and convert the block into a lambda of these variables.
  // The previous block does it's own assignments and then calls its successor.
  // Later we can hoist these lambdas into functions if needed,
  // and use constant folding to collapse unnecessary indirections.
  let mut blk_conts: HashSet<Expr> = HashSet::new();
  let mut visit_stack = converter.proc_exits;  // Start with the returns
  while let Some(blk_id) = visit_stack.pop() {

  }
  todo!()
}

fn make_continuation_from_block(conv: &ssa::Converter, blk_id: ssa::BlockId) -> Expr {
  // let blk_assignments = conv.ssa.blocks[blk_id].assignments.clone();
  // let blk_vars = conv.ssa.crnt_def[blk_id].clone();
  // for (name, val) in blk_vars {
  //   match val.as_ref() {
  //     // This var comes from previous values, just add it as a lambda
  //     ssa::Operand::Phi { phi } => {

  //     }
  //     // This may come from a single previous value or from a local assignment, so we check
  //     ssa::Operand::Var { name } => {
  //       if let Some(op) = blk_assignments.get(name) {
  //         // Its coming from an assignment in this block
  //       } else {
  //         // Its coming from a predecessor
  //       }
  //     }
  //     _ => panic!("Unexpected var assignment when making continuation")
  //   }
  // }
  todo!()
}