use super::braun::BraunConverter;
use super::*;
use crate::fun::{Expr, Id};
use crate::imp::Imp;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Converter {
  names: HashMap<Id, u64>,
  pub ssa: BraunConverter,
  scope_entry: Option<BlockId>,
  scope_exit: Option<BlockId>,
  proc_exit: Option<BlockId>,
}

impl Converter {
  pub fn new() -> Self {
    Converter {
      names: HashMap::new(),
      ssa: BraunConverter::new(),
      scope_entry: None,
      scope_exit: None,
      proc_exit: None,
    }
  }

  fn next_name(&mut self, name: Id) -> Id {
    let new_n = if let Some(old_n) = self.names.get(&name) { old_n + 1 } else { 0 };
    self.names.insert(name.clone(), new_n);
    format!("{name}.{new_n}")
  }

  pub fn convert_proc(&mut self, proc: Procedure) -> BlockId {
    let ret_block = self.ssa.new_block(vec![], BlockKind::Simple);
    self.proc_exit = Some(ret_block);

    let proc_block = self.ssa.new_sealed_block(vec![], BlockKind::Simple);
    let proc_block = self.convert_stmt(proc.body, proc_block);

    self.ssa.blocks[ret_block].preds.push(proc_block);
    self.ssa.seal_block(ret_block);
    ret_block
  }

  pub fn convert_stmt(&mut self, stmt: Imp, crnt_block: BlockId) -> BlockId {
    match stmt {
      Imp::Block { stmts } => {
        // TODO: how do I make things declared in the block have local scope
        let mut crnt_block = crnt_block;
        for stmt in stmts {
          crnt_block = self.convert_stmt(stmt, crnt_block);
        }
        crnt_block
      }
      Imp::Assignment { name, expr } => {
        #[cfg(feature = "log")]
        println!("assign {expr}");
        let name_b = self.next_name(name.clone());
        let expr_b = self.convert_expr(&expr, crnt_block);
        self.ssa.blocks[crnt_block].add_assignment(name_b.clone(), expr_b);
        let name_b = Rc::new(Operand::Var { name: name_b });
        self.ssa.write_var(&name, crnt_block, name_b);
        crnt_block
      }
      Imp::Expression { .. } => {
        // TODO: For now, expression statements don't do anything
        crnt_block
      }
      Imp::MatchStmt { expr, cases, default } => {
        // Default case is obligatory and separate in the AST
        let dflt_blk = self.ssa.new_sealed_block(vec![], BlockKind::Simple);
        let dflt_blk = self.convert_stmt(*default, dflt_blk);

        // The branching block with the expr to be matched
        let blk_kind =
          BlockKind::Match { cond: expr.into(), cases: vec![], default: dflt_blk };
        let match_blk = self.ssa.new_sealed_block(vec![crnt_block], blk_kind);

        // Seal the default case
        self.ssa.blocks[dflt_blk].preds.push(match_blk);
        self.ssa.seal_block(dflt_blk);

        // The exit block that ties the cases
        let exit_blk = self.ssa.new_block(vec![dflt_blk], BlockKind::Simple);

        // Convert each of the cases and point to the exit
        for case in cases {
          let case_blk = self.ssa.new_sealed_block(vec![match_blk], BlockKind::Simple);
          let case_blk = self.convert_stmt(case.body, case_blk);
          {
            if let BlockKind::Match { cases, .. } = &mut self.ssa.blocks[match_blk].kind {
              cases.push((case.matched.into(), case_blk));
            } else {
              panic!("Block in match stmt is not Match");
            }
          }
          self.ssa.blocks[exit_blk].preds.push(case_blk);
        }

        self.ssa.seal_block(exit_blk);
        exit_blk
      }
      Imp::IfElse { condition, true_case, false_case } => {
        // Else block is the default case, we create early to point to it in the match block
        let else_blk = self.ssa.new_block(vec![], BlockKind::Simple);
        let else_blk = self.convert_stmt(*false_case, else_blk);

        // If condition
        let cond_kind =
          BlockKind::Match { cond: condition.into(), cases: vec![], default: else_blk };
        let cond_blk = self.ssa.new_sealed_block(vec![crnt_block], cond_kind);

        // Seal the else
        self.ssa.blocks[else_blk].preds.push(cond_blk);
        self.ssa.seal_block(else_blk);

        // Then block (condition true)
        let then_blk = self.ssa.new_sealed_block(vec![cond_blk], BlockKind::Simple);
        let then_blk = self.convert_stmt(*true_case, then_blk);

        {
          if let BlockKind::Match { cases, .. } = &mut self.ssa.blocks[cond_blk].kind {
            cases.push((
              Operand::Ctr { name: "True".to_string(), args: vec![] }.into(),
              then_blk,
            ));
            cases.push((
              Operand::Ctr { name: "False".to_string(), args: vec![] }.into(),
              else_blk,
            ));
          } else {
            panic!("IfElse block is not a Match");
          }
        }

        self.ssa.new_sealed_block(vec![then_blk, else_blk], BlockKind::Simple)
      }
      Imp::ForElse { initialize, condition, afterthought, body, else_case } => {
        // Init block, called once
        let init_blk = self.ssa.new_sealed_block(vec![crnt_block], BlockKind::Simple);
        let init_blk = self.convert_stmt(*initialize, init_blk);

        // Go to else on normal loop ending (no breaks)
        let else_blk = self.ssa.new_block(vec![], BlockKind::Simple);
        let else_blk = self.convert_stmt(*else_case, else_blk);

        // Loop block, with condition going to else or body
        let loop_kind =
          BlockKind::Match { cond: condition.into(), cases: vec![], default: else_blk };
        let loop_blk = self.ssa.new_block(vec![init_blk], loop_kind);

        // Seal else block
        self.ssa.blocks[else_blk].preds.push(loop_blk);
        self.ssa.seal_block(else_blk);

        // Exit block that ties everything together
        let exit_blk = self.ssa.new_block(vec![else_blk], BlockKind::Simple);

        // Swap the entry and exit for break and continue inside the body
        let old_entry = std::mem::replace(&mut self.scope_entry, Some(loop_blk));
        let old_exit = std::mem::replace(&mut self.scope_exit, Some(exit_blk));

        // Statement executed after one body iteration
        let after_blk = self.ssa.new_sealed_block(vec![], BlockKind::Simple);
        let after_blk = self.convert_stmt(*afterthought, after_blk);

        // The loop body itself
        let body_blk = self.ssa.new_sealed_block(vec![loop_blk], BlockKind::Simple);
        let body_blk = self.convert_stmt(*body, body_blk);

        // Afterthought predecessors are the last body stmt and any continues
        // TODO: does it bug if the last stmt in the body is a break?
        self.ssa.blocks[after_blk].preds.push(body_blk);
        self.ssa.seal_block(after_blk);

        // Condition block predecessors are the initializer expression and the afterthought
        self.ssa.blocks[loop_blk].preds.push(after_blk);
        self.ssa.seal_block(loop_blk);

        // Exit predecessors are the else block and any breaks in the body
        self.ssa.seal_block(exit_blk);

        // Restore the previous entry and exit after exiting the loop
        self.scope_entry = old_entry;
        self.scope_exit = old_exit;

        exit_blk
      }
      _ => todo!("Other statements still need to be done"),
    }
  }

  pub fn convert_expr(&mut self, expr: &Expr, blk_id: BlockId) -> Rc<Operand> {
    match expr {
      Expr::Var { name } => self.ssa.read_var(&name, blk_id),
      Expr::Unsigned { numb } => Rc::new(Operand::Unsigned { numb: *numb }),
      Expr::BinOp { op, left, right } => {
        let left = self.convert_expr(left, blk_id);
        let right = self.convert_expr(right, blk_id);
        Rc::new(Operand::BinOp { op: *op, left, right })
      }
      _ => todo!("Other expressions not covered yet"),
    }
  }
}