use super::braun::BraunConverter;
use super::*;
use crate::imp::Imp;

#[derive(Debug)]
pub struct Converter {
  pub proc_name: String,
  pub ssa: BraunConverter,
  pub proc_entry: Option<BlockId>,
  pub proc_exits: Vec<BlockId>,
  pub blk_kinds: Vec<BlockKind>, // separate from ssa blocks to avoid double borrow
  scope_entry: Option<BlockId>,
  scope_exit: Option<BlockId>,
}

impl Converter {
  pub fn new() -> Self {
    Converter {
      proc_name: String::new(),
      ssa: BraunConverter::new(),
      proc_entry: None,
      proc_exits: Vec::new(),
      blk_kinds: Vec::new(),
      scope_entry: None,
      scope_exit: None,
    }
  }

  pub fn convert_proc(&mut self, proc: Procedure) -> ConvertResult<()> {
    self.proc_name = proc.name;

    // Create process entry
    let proc_entry = self.ssa.new_sealed_block(vec![])?;
    self.blk_kinds.push(BlockKind::Simple);
    self.proc_entry = Some(proc_entry);

    // Assign the procedure args
    for arg in proc.args {
      let val = Operand::Value(Expr::Parameter.into());
      self.ssa.add_assignment(proc_entry, arg, val);
    }

    // Convert process body
    let exit_blk = self.convert_stmt(proc.body, proc_entry)?;

    // Check that last block is a return
    if matches!(self.blk_kinds[exit_blk], BlockKind::Return{ .. }) {
      Ok(())
    } else {
      Err("Procedure doesn't end with a return".into())
    }
  }

  fn convert_stmt(&mut self, stmt: Imp, crnt_block: BlockId) -> ConvertResult<BlockId> {
    match stmt {
      Imp::Block { stmts } => {
        // This doesn't take care of scope,
        // that should be done in the original language
        // by renaming scope-local variable or a similar technique
        let mut crnt_block = crnt_block;
        for stmt in stmts {
          crnt_block = self.convert_stmt(stmt, crnt_block)?;
        }
        Ok(crnt_block)
      }
      Imp::Assignment { name, expr } => {
        #[cfg(feature = "log")]
        println!("assign {expr}");
        let expr = self.ssa.convert_expr(&expr, crnt_block)?;
        self.ssa.add_assignment(crnt_block, name, Operand::Value(expr));
        Ok(crnt_block)
      }
      Imp::Expression { .. } => {
        // TODO: For now, expression statements don't do anything
        Ok(crnt_block)
      }
      Imp::MatchStmt { expr, cases, default } => {
        // Default case is obligatory and separate in the AST
        let dflt_blk = self.ssa.new_sealed_block(vec![])?;
        let dflt_blk = self.convert_stmt(*default, dflt_blk)?;
        self.blk_kinds.push(BlockKind::Simple);

        // The branching block with the expr to be matched
        let match_blk = self.ssa.new_sealed_block(vec![crnt_block])?;
        let cond = self.ssa.convert_expr(&expr, match_blk)?;
        // Replace kind after creation cause we need the block before to insert phis
        let blk_kind = BlockKind::Match { cond, cases: vec![], default: dflt_blk };
        self.blk_kinds.push(blk_kind);

        // Seal the default case
        self.add_block_pred(dflt_blk, match_blk);
        self.ssa.seal_block(dflt_blk)?;

        // The exit block that ties the cases
        let exit_blk = self.ssa.new_block(vec![dflt_blk]);
        self.blk_kinds.push(BlockKind::Simple);

        // Convert each of the cases and point to the exit
        for case in cases {
          // Create entry block for this case
          let case_entry = self.ssa.new_sealed_block(vec![match_blk])?;
          self.blk_kinds.push(BlockKind::Simple);
          // Convert case pattern
          let pattern = self.ssa.convert_pat(&case.matched, case_entry)?;

          // Convert case body
          let case_exit = self.convert_stmt(case.body, case_entry)?;
          if let BlockKind::Match { cases, .. } = &mut self.blk_kinds[match_blk] {
            cases.push((*pattern, case_exit));
          } else {
            panic!("Block in match stmt is not Match");
          }
          self.add_block_pred(exit_blk, case_exit);
        }

        self.ssa.seal_block(exit_blk)?;
        Ok(exit_blk)
      }
      Imp::IfElse { condition, true_case, false_case } => {
        // Else block is the default case, we create early to point to it in the match block
        let else_blk = self.ssa.new_block(vec![]);
        self.blk_kinds.push(BlockKind::Simple);
        let else_blk = self.convert_stmt(*false_case, else_blk)?;

        // If condition
        let cond_blk = self.ssa.new_sealed_block(vec![crnt_block])?;
        let cond = self.ssa.convert_expr(&condition, cond_blk)?;
        self.blk_kinds.push(BlockKind::Match { cond, cases: vec![], default: else_blk });

        // Seal the else
        self.add_block_pred(else_blk, cond_blk);
        self.ssa.seal_block(else_blk)?;

        // Then block (condition true)
        let then_blk = self.ssa.new_sealed_block(vec![cond_blk])?;
        self.blk_kinds.push(BlockKind::Simple);
        let then_blk = self.convert_stmt(*true_case, then_blk)?;

        if let BlockKind::Match { cases, .. } = &mut self.blk_kinds[cond_blk] {
          cases.push((
            Expr::Ctr { name: "True".to_string(), args: vec![] }.into(),
            then_blk,
          ));
          cases.push((
            Expr::Ctr { name: "False".to_string(), args: vec![] }.into(),
            else_blk,
          ));
        } else {
          panic!("If Else block is not a Match");
        }

        let exit_blk = self.ssa.new_sealed_block(vec![then_blk, else_blk])?;
        self.blk_kinds.push(BlockKind::Simple);
        Ok(exit_blk)
      }
      Imp::ForElse { initialize, condition, afterthought, body, else_case } => {
        // Init block, called once
        let init_blk = self.ssa.new_sealed_block(vec![crnt_block])?;
        self.blk_kinds.push(BlockKind::Simple);
        let init_blk = self.convert_stmt(*initialize, init_blk)?;

        // Go to else on normal loop ending (no breaks)
        let else_blk = self.ssa.new_block(vec![]);
        self.blk_kinds.push(BlockKind::Simple);
        let else_blk = self.convert_stmt(*else_case, else_blk)?;

        // Loop block, with condition going to else or body
        let loop_blk = self.ssa.new_block(vec![init_blk]);
        let cond = self.ssa.convert_expr(&condition, loop_blk)?;
        self.blk_kinds.push(BlockKind::Match { cond, cases: vec![], default: else_blk });

        // Seal else block
        self.add_block_pred(else_blk, loop_blk);
        self.ssa.seal_block(else_blk)?;

        // Exit block that ties everything together
        let exit_blk = self.ssa.new_block(vec![else_blk]);
        self.blk_kinds.push(BlockKind::Simple);

        // Statement executed after one body iteration
        let after_blk = self.ssa.new_sealed_block(vec![])?;
        self.blk_kinds.push(BlockKind::Simple);
        let after_blk = self.convert_stmt(*afterthought, after_blk)?;

        // Swap the entry and exit for break and continue inside the body
        let old_entry = self.scope_entry;
        let old_exit = self.scope_exit;
        self.scope_entry = Some(after_blk);
        self.scope_exit = Some(exit_blk);

        // The loop body itself
        let body_blk = self.ssa.new_sealed_block(vec![loop_blk])?;
        self.blk_kinds.push(BlockKind::Simple);
        let body_blk = self.convert_stmt(*body, body_blk)?;

        // Afterthought predecessors are the last body stmt and any continues
        // TODO: does it bug if the last stmt in the body is a break?
        self.add_block_pred(after_blk, body_blk);
        self.ssa.seal_block(after_blk)?;

        // Condition block predecessors are the initializer expression and the afterthought
        self.add_block_pred(loop_blk, after_blk);
        self.ssa.seal_block(loop_blk)?;

        // Exit predecessors are the else block and any breaks in the body
        self.ssa.seal_block(exit_blk)?;

        // Restore the previous entry and exit after exiting the loop
        self.scope_entry = old_entry;
        self.scope_exit = old_exit;

        Ok(exit_blk)
      }
      Imp::ForInElse { .. } => todo!("Ranged For is not yet supported"),
      Imp::WhileElse { condition, body, else_case } => {
        // Go to else on normal loop ending (no breaks)
        let else_blk = self.ssa.new_block(vec![]);
        self.blk_kinds.push(BlockKind::Simple);
        let else_blk = self.convert_stmt(*else_case, else_blk)?;

        // Loop block, with condition going to else or body
        let loop_blk = self.ssa.new_block(vec![crnt_block]);
        let cond = self.ssa.convert_expr(&condition, loop_blk)?;
        self.blk_kinds.push(BlockKind::Match { cond, cases: vec![], default: else_blk });

        // Seal else block
        self.add_block_pred(else_blk, loop_blk);
        self.ssa.seal_block(else_blk)?;

        // Exit block that ties everything together
        let exit_blk = self.ssa.new_block(vec![else_blk]);
        self.blk_kinds.push(BlockKind::Simple);

        // Swap the entry and exit for break and continue inside the body
        let old_entry = self.scope_entry;
        let old_exit = self.scope_exit;
        self.scope_entry = Some(loop_blk);
        self.scope_exit = Some(exit_blk);

        // The loop body itself
        let body_blk = self.ssa.new_sealed_block(vec![loop_blk])?;
        self.blk_kinds.push(BlockKind::Simple);
        let body_blk = self.convert_stmt(*body, body_blk)?;

        // Seal the loop block
        self.add_block_pred(loop_blk, body_blk);
        self.ssa.seal_block(loop_blk)?;

        // Exit predecessors are the else block and any breaks in the body
        self.ssa.seal_block(exit_blk)?;

        // Restore the previous entry and exit after exiting the loop
        self.scope_entry = old_entry;
        self.scope_exit = old_exit;

        Ok(exit_blk)
      }
      Imp::Label { .. } => Err("Labels are not yet supported".into()),
      Imp::Return { value } => {
        let ret_blk = self.ssa.new_sealed_block(vec![crnt_block])?;
        let val = self.ssa.convert_expr(&value, ret_blk)?;
        self.blk_kinds.push(BlockKind::Return { val });
        self.proc_exits.push(ret_blk);
        Ok(ret_blk)
      }
      Imp::Goto { .. } => Err("Gotos are not yet supported".into()),
      Imp::ProcedureDef { .. } => panic!("Found procedure definition during SSA. Should've been hoisted earlier"),
      Imp::Continue => {
        if let Some(scope_entry) = self.scope_entry {
          let cont_block = self.ssa.new_sealed_block(vec![crnt_block])?;
          self.blk_kinds.push(BlockKind::Jump { dest: scope_entry });
          self.add_block_pred(scope_entry, cont_block);
          Ok(cont_block)
        } else {
          Err("Continue found outside of scoped block".into())
        }
      }
      Imp::Break => {
        if let Some(scope_exit) = self.scope_exit {
          let break_block = self.ssa.new_sealed_block(vec![crnt_block])?;
          self.blk_kinds.push(BlockKind::Jump { dest: scope_exit });
          self.add_block_pred(scope_exit, break_block);
          Ok(break_block)
        } else {
          Err("Break found outside of scoped block".into())
        }
      }
      Imp::Pass => Ok(crnt_block),
    }
  }

  fn add_block_pred(&mut self, blk_id: BlockId, pred_id: BlockId) {
    self.ssa.blocks[blk_id].preds.push(pred_id);
  }
}