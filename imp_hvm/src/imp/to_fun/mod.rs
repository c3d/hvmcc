pub mod ssa;

use crate::fun::{Expr, Id};
use crate::imp::{CaseStmt, Imp, Procedure};
use std::collections::HashSet;

pub fn imp_to_fun(_imperative: Imp) -> Expr {
  todo!()
}

fn hoist_proc_defs(proc: &mut Imp, proc_name: &Id, hoisted: &mut Vec<Procedure>) {
  match proc {
    Imp::Assignment { .. }
    | Imp::Expression { .. }
    | Imp::Return { .. }
    | Imp::Goto { .. }
    | Imp::Continue
    | Imp::Break
    | Imp::Pass => {}
    Imp::MatchStmt { cases, default, .. } => {
      for CaseStmt { body, .. } in cases {
        hoist_proc_defs(body, proc_name, hoisted);
      }
      hoist_proc_defs(default, proc_name, hoisted);
    }
    Imp::IfElse { true_case, false_case, .. } => {
      hoist_proc_defs(true_case, proc_name, hoisted);
      hoist_proc_defs(false_case, proc_name, hoisted);
    }
    Imp::ForElse { body, else_case, .. }
    | Imp::ForInElse { body, else_case, .. }
    | Imp::WhileElse { body, else_case, .. } => {
      hoist_proc_defs(body, proc_name, hoisted);
      hoist_proc_defs(else_case, proc_name, hoisted);
    }
    Imp::Label { stmt, .. } => {
      hoist_proc_defs(stmt, proc_name, hoisted);
    }
    Imp::Block { stmts } => {
      for stmt in stmts {
        hoist_proc_defs(stmt, proc_name, hoisted);
      }
    }
    Imp::ProcedureDef { name, args, body } => {
      let name = format!("{proc_name}.{name}");
      let vars = unbound_in_stmt(body);
      let args: HashSet<Id> = args.iter().cloned().collect();
      let diff: Vec<Id> = vars.difference(&args).map(String::clone).collect();
      let new_proc = Procedure { name: name.clone(), args: diff, body: *body.clone() };
      // TODO:i think this is not enough,
      // and for every call of this new proc, it should add the `diff` arguments by default.
      // this should be done by the way of mapping when translating.
      hoisted.push(new_proc);
      hoist_proc_defs(body, &name, hoisted);
      *proc = Imp::Pass;
    }
  }
}

fn unbound_in_stmt(stmt: &Imp) -> HashSet<Id> {
  use crate::fun::to_hvm::get_unbound_vars as unbound_in_expr;

  match stmt {
    Imp::Assignment { name, expr } => {
      let mut unbound_vars = unbound_in_expr(expr);
      unbound_vars.remove(name);
      unbound_vars
    }
    Imp::Expression { expr } => unbound_in_expr(expr),
    Imp::MatchStmt { expr, cases, default } => {
      let mut vars = unbound_in_expr(expr);
      for CaseStmt { matched, body } in cases {
        let pat_vars = unbound_in_expr(matched);
        let body_vars = unbound_in_stmt(body);
        let diff: HashSet<String> = body_vars.difference(&pat_vars).map(String::clone).collect();
        vars.extend(diff);
      }
      vars.extend(unbound_in_stmt(default));
      vars
    }
    Imp::IfElse { condition, true_case, false_case } => {
      let mut vars = unbound_in_expr(condition);
      vars.extend(unbound_in_stmt(true_case));
      vars.extend(unbound_in_stmt(false_case));
      vars
    }
    Imp::ForElse { initialize, condition, afterthought, body, else_case } => {
      let mut vars = unbound_in_stmt(initialize);
      vars.extend(unbound_in_expr(condition));
      vars.extend(unbound_in_stmt(afterthought));
      vars.extend(unbound_in_stmt(body));
      vars.extend(unbound_in_stmt(else_case));
      vars
    }
    Imp::ForInElse { target, iterator, body, else_case } => {
      let mut vars = unbound_in_expr(iterator);
      vars.extend(unbound_in_stmt(body));
      vars.extend(unbound_in_stmt(else_case));
      vars.remove(target);
      vars
    }
    Imp::WhileElse { condition, body, else_case } => {
      let mut vars = unbound_in_expr(condition);
      vars.extend(unbound_in_stmt(body));
      vars.extend(unbound_in_stmt(else_case));
      vars
    }
    // label name is not a variable
    Imp::Label { stmt, .. } => unbound_in_stmt(stmt),
    // neither is goto
    Imp::Goto { .. } => HashSet::new(),
    Imp::Return { value } => unbound_in_expr(value),
    // procedure name is not a variable
    Imp::ProcedureDef { args, body, .. } => {
      let vars = unbound_in_stmt(body);
      let args: HashSet<Id> = args.iter().cloned().collect();
      let diff: HashSet<Id> = vars.difference(&args).map(String::clone).collect();
      diff
    }
    Imp::Block { stmts } => stmts
      .iter()
      .map(unbound_in_stmt)
      .fold(HashSet::new(), |acc, val| acc.union(&val).map(String::clone).collect()),
    Imp::Continue | Imp::Break | Imp::Pass => HashSet::new(),
  }
}
