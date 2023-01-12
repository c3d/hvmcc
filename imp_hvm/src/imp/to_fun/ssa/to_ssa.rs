use super::braun::BraunConverter;
use super::{Block, Operand};
use crate::fun::{Expr, Id};
use crate::imp::Imp;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

struct Converter {
  names: HashMap<Id, u64>,
  blocks: Vec<Rc<RefCell<Block>>>,
  blk_count: u64,
  ssa: BraunConverter,
}

impl Converter {
  pub fn new() -> Self {
    Converter {
      names: HashMap::new(),
      blocks: Vec::new(),
      blk_count: 0,
      ssa: BraunConverter::new(),
    }
  }

  fn new_block(&mut self, preds: &[Rc<RefCell<Block>>]) -> Rc<RefCell<Block>> {
    let block = Rc::new(RefCell::new(Block::new(self.new_blk_id(), preds)));
    self.blocks.push(block.clone());
    block
  }

  fn new_blk_id(&mut self) -> u64 {
    let n = self.blk_count;
    self.blk_count += 1;
    n
  }

  pub fn new_sealed_block(&mut self, preds: &[Rc<RefCell<Block>>]) -> Rc<RefCell<Block>> {
    let block = self.new_block(preds);
    self.ssa.seal_block(block.clone());
    block
  }

  fn next_name(&mut self, name: Id) -> Id {
    let new_n = if let Some(old_n) = self.names.get(&name) { old_n + 1 } else { 0 };
    self.names.insert(name.clone(), new_n);
    format!("{name}.{new_n}")
  }


  pub fn convert_stmt(&mut self, stmt: Imp, crnt_block: Rc<RefCell<Block>>) -> Rc<RefCell<Block>> {
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
          crnt_block.borrow_mut().add_assignment(name_b.clone(), expr_b);
        }
        let name_b = Rc::new(Operand::Var { name: name_b });
        self.ssa.write_var(&name, crnt_block.clone(), name_b);
        crnt_block
      }
      Imp::Expression { expr } => {
        // For now, expression statements don't do anything
        crnt_block
      }
      Imp::MatchStmt { expr, cases, default } => todo!(),
      Imp::IfElse { condition, true_case, false_case } => {
        let condition = Imp::Expression { expr: condition };
        let cond_blk = self.new_sealed_block(&[crnt_block.clone()]);
        let cond_blk = self.convert_stmt(condition, cond_blk);
    
        let t_blk = self.new_sealed_block(&[cond_blk.clone()]);
        let t_blk = self.convert_stmt(*true_case, t_blk);
    
        let else_blk = self.new_sealed_block(&[cond_blk.clone()]);
        let else_blk = self.convert_stmt(*false_case, else_blk);
        
        self.new_sealed_block(&[t_blk, else_blk])
      }
      Imp::ForElse { initialize, condition, afterthought, body, else_case } => {
        let init_blk = self.new_sealed_block(&[crnt_block]);
        let init_blk = self.convert_stmt(*initialize, init_blk);

        let cond_blk = self.new_block(&[init_blk]);
        let condition = Imp::Expression { expr: condition };
        let cond_blk = self.convert_stmt(condition, cond_blk);

        let else_blk = self.new_sealed_block(&[cond_blk.clone()]);
        let else_blk = self.convert_stmt(*else_case, else_blk);

        let exit_blk = self.new_block(&[else_blk]);
        
        let after_blk = self.new_sealed_block(&[]);
        let after_blk = self.convert_stmt(*afterthought, after_blk);

        // TODO: we need to pass the blocks to break or continue
        let body_blk = self.new_sealed_block(&[cond_blk.clone()]);
        let body_blk = self.convert_stmt(*body, body_blk);

        // Afterthought predecessors are the last body stmt and any continues
        // TODO: does it bug if the last stmt in the body is a break?
        { after_blk.borrow_mut().preds.push(body_blk); }
        self.ssa.seal_block(after_blk.clone());

        // Condition block predecessors are the initializer expression and the afterthought
        { cond_blk.borrow_mut().preds.push(after_blk); }
        self.ssa.seal_block(cond_blk);

        // Exit predecessors are the else block and any breaks in the body
        self.ssa.seal_block(exit_blk.clone());

        exit_blk
      }
      _ => todo!("Other statements still need to be done"),
    }
  }

  pub fn convert_expr(&mut self, expr: &Expr, block: Rc<RefCell<Block>>) -> Rc<Operand> {
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
