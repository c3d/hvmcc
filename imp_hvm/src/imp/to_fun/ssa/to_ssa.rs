use super::braun::BraunConverter;
use super::*;
use crate::fun::{Expr, Id};
use crate::imp::Imp;
use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;
use std::process::exit;
use std::rc::Rc;

struct Converter {
  names: HashMap<Id, u64>,
  // TODO: This should be the only owner of the Blocks, use `Weak` in other places.
  // Right now we have memory leaks
  blocks: Vec<BlockRef>,
  blk_count: u64,
  ssa: BraunConverter,
  scope_entry: Option<BlockRef>,
  scope_exit: Option<BlockRef>,
  proc_exit: Option<BlockRef>,
}

impl Converter {
  pub fn new() -> Self {
    Converter {
      names: HashMap::new(),
      blocks: Vec::new(),
      blk_count: 0,
      ssa: BraunConverter::new(),
      scope_entry: None,
      scope_exit: None,
      proc_exit: None,
    }
  }

  fn new_block(&mut self, preds: &[BlockRef], kind: BlockKind) -> BlockRef {
    let id = self.new_blk_id();
    let block = Rc::new(RefCell::new(Block::new(id, preds, kind)));
    self.blocks.push(block.clone());
    block
  }

  fn new_blk_id(&mut self) -> u64 {
    let n = self.blk_count;
    self.blk_count += 1;
    n
  }

  pub fn new_sealed_block(&mut self, preds: &[BlockRef], kind: BlockKind) -> BlockRef {
    let block = self.new_block(preds, kind);
    self.ssa.seal_block(block.clone());
    block
  }

  fn next_name(&mut self, name: Id) -> Id {
    let new_n = if let Some(old_n) = self.names.get(&name) { old_n + 1 } else { 0 };
    self.names.insert(name.clone(), new_n);
    format!("{name}.{new_n}")
  }

  pub fn convert_proc(&mut self, proc: Procedure) -> BlockRef {
    let ret_block = self.new_block(&vec![], BlockKind::Simple);
    self.proc_exit = Some(ret_block.clone());

    let proc_block = self.new_sealed_block(&vec![], BlockKind::Simple);
    let proc_block = self.convert_stmt(proc.body, proc_block.clone());

    add_block_pred(ret_block.clone(), proc_block);
    self.ssa.seal_block(ret_block.clone());
    ret_block
  }

  pub fn convert_stmt(&mut self, stmt: Imp, crnt_block: BlockRef) -> BlockRef {
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
        let expr_b = self.convert_expr(&expr, crnt_block.clone());
        {
          (*crnt_block).borrow_mut().add_assignment(name_b.clone(), expr_b);
        }
        let name_b = Rc::new(Operand::Var { name: name_b });
        self.ssa.write_var(&name, crnt_block.clone(), name_b);
        crnt_block
      }
      Imp::Expression { .. } => {
        // TODO: For now, expression statements don't do anything
        crnt_block
      }
      Imp::MatchStmt { expr, cases, default } => {
        // Default case is obligatory and separate in the AST
        let dflt_blk = self.new_sealed_block(&[], BlockKind::Simple);
        let dflt_blk = self.convert_stmt(*default, dflt_blk.clone());

        // The branching block with the expr to be matched
        let blk_kind = BlockKind::Match { cond: expr.into(), cases: vec![], default: dflt_blk.clone() };
        let match_blk = self.new_sealed_block(&[crnt_block], blk_kind);

        // Seal the default case
        add_block_pred(dflt_blk.clone(), match_blk.clone());
        self.ssa.seal_block(dflt_blk.clone());

        // The exit block that ties the cases
        let exit_blk = self.new_block(&[dflt_blk], BlockKind::Simple);

        // Convert each of the cases and point to the exit
        for case in cases {
          let case_blk = self.new_sealed_block(&[match_blk.clone()], BlockKind::Simple);
          let case_blk = self.convert_stmt(case.body, case_blk);
          {
            if let BlockKind::Match { cases, .. } = &mut (*match_blk).borrow_mut().kind {
              cases.push((case.matched.into(), case_blk.clone()));
            } else {
              panic!("Block in match stmt is not Match");
            }
          }
          add_block_pred(exit_blk.clone(), case_blk);
        }

        self.ssa.seal_block(exit_blk.clone());
        exit_blk
      },
      Imp::IfElse { condition, true_case, false_case } => {
        // Else block is the default case, we create early to point to it in the match block
        let else_blk = self.new_block(&[], BlockKind::Simple);
        let else_blk = self.convert_stmt(*false_case, else_blk.clone());

        // If condition
        let cond_kind = BlockKind::Match { cond: condition.into(), cases: vec![], default: else_blk.clone() };
        let cond_blk = self.new_sealed_block(&[crnt_block], cond_kind);

        // Seal the else
        add_block_pred(else_blk.clone(), cond_blk.clone());
        self.ssa.seal_block(else_blk.clone());
    
        // Then block (condition true)
        let then_blk = self.new_sealed_block(&[cond_blk.clone()], BlockKind::Simple);
        let then_blk = self.convert_stmt(*true_case, then_blk);

        {
          if let BlockKind::Match { cases, .. } = &mut (*cond_blk).borrow_mut().kind {
            cases.push((Operand::Ctr { name: "True".to_string(), args: vec![] }.into(), then_blk.clone()));
            cases.push((Operand::Ctr { name: "False".to_string(), args: vec![] }.into(), else_blk.clone()));
          } else {
            panic!("IfElse block is not a Match");
          }
        }
        
        self.new_sealed_block(&[then_blk, else_blk], BlockKind::Simple)
      }
      Imp::ForElse { initialize, condition, afterthought, body, else_case } => {
        // Init block, called once
        let init_blk = self.new_sealed_block(&[crnt_block], BlockKind::Simple);
        let init_blk = self.convert_stmt(*initialize, init_blk);
        
        // Go to else on normal loop ending (no breaks)
        let else_blk = self.new_block(&[], BlockKind::Simple);
        let else_blk = self.convert_stmt(*else_case, else_blk.clone());

        // Loop block, with condition going to else or body
        let loop_kind = BlockKind::Match { cond: condition.into(), cases: vec![], default: else_blk.clone() };
        let loop_blk = self.new_block(&[init_blk], loop_kind);
        
        // Seal else block
        add_block_pred(else_blk.clone(), loop_blk.clone());
        self.ssa.seal_block(else_blk.clone());
        
        // Exit block that ties everything together
        let exit_blk = self.new_block(&[else_blk], BlockKind::Simple);
    
        // Swap the entry and exit for break and continue inside the body
        let old_entry = std::mem::replace(&mut self.scope_entry, Some(loop_blk.clone()));
        let old_exit = std::mem::replace(&mut self.scope_exit, Some(exit_blk.clone()));
        
        // Statement executed after one body iteration
        let after_blk = self.new_sealed_block(&[], BlockKind::Simple);
        let after_blk = self.convert_stmt(*afterthought, after_blk);

        // The loop body itself
        let body_blk = self.new_sealed_block(&[loop_blk.clone()], BlockKind::Simple);
        let body_blk = self.convert_stmt(*body, body_blk);

        // Afterthought predecessors are the last body stmt and any continues
        // TODO: does it bug if the last stmt in the body is a break?
        { (*after_blk).borrow_mut().preds.push(body_blk); }
        self.ssa.seal_block(after_blk.clone());

        // Condition block predecessors are the initializer expression and the afterthought
        { (*loop_blk).borrow_mut().preds.push(after_blk); }
        self.ssa.seal_block(loop_blk);

        // Exit predecessors are the else block and any breaks in the body
        self.ssa.seal_block(exit_blk.clone());

        // Restore the previous entry and exit after exiting the loop
        self.scope_entry = old_entry;
        self.scope_exit = old_exit;

        exit_blk
      }
      _ => todo!("Other statements still need to be done"),
    }
  }

  pub fn convert_expr(&mut self, expr: &Expr, block: BlockRef) -> Rc<Operand> {
    match expr {
      Expr::Var { name } => self.ssa.read_var(&name, block),
      Expr::Unsigned { numb } => Rc::new(Operand::Unsigned { numb: *numb }),
      Expr::BinOp { op, left, right } => {
        let left = self.convert_expr(left, block.clone());
        let right = self.convert_expr(right, block);
        Rc::new(Operand::BinOp { op: *op, left, right })
      }
      _ => todo!("Other expressions not covered yet"),
    }
  }
}

fn add_block_pred(block: BlockRef, new_pred: BlockRef) {
  { (*block).borrow_mut().preds.push(new_pred); }
}