use crate::{Imp, CaseStmt, Procedure, Program};
use crate::fun::print::pprint_fun;

/// Pads the string n characters to the right
pub fn ind(string: &str, n: usize) -> String {
  let pad = " ".repeat(n);
  format!("{pad}{string}")
}

pub fn indent_block(imp: &Imp, n:usize) -> String {
  // indents the last } closing the block, and increases the recursion depth.
  let block = pprint_imp(imp, n+2);
  format!("{{\n{block}\n{}",ind("}", n))
}

fn pprint_imp(imp: &Imp, depth: usize) -> String {
  match imp {
    Imp::Sequence { stmt1, stmt2 } => {
      let fst = pprint_imp(stmt1, depth);
      let snd = pprint_imp(stmt2, depth);
      format!("{fst}\n{snd}")
    },
    Imp::Assignment { name, expr } => {
      let name = ind(name, depth);
      let expr = pprint_fun(expr, depth);
      format!("{name} = {expr};")
    },
    Imp::Expression { expr } => {
      let expr = pprint_fun(expr, depth);
      format!("{expr};")
    },
    Imp::MatchStmt { expr, cases, default } => {
      fn display_case(case: &CaseStmt, depth: usize) -> String {
        let CaseStmt { matched, body } = case;
        let matched = pprint_fun(matched, depth);
        let body = pprint_imp(body, depth+2);
        format!("{matched} =>\n{body}",)
      }
      let cases = cases.iter().map(|x| display_case(x, depth+2)).collect::<Vec<String>>().join("\n");
      let imatch = ind("match", depth);
      if let Some(default) = default {
        let default_string = format!("{} => {}", ind("default", depth+2), default);
        let cases = [&cases[..], &default_string].concat();
        return format!("{imatch} {expr} {{\n{}\n{}", cases, ind("}", depth))
      }
      format!("{imatch} {expr} {{\n{}\n{}", cases, ind("}", depth))
    },
    Imp::IfElse { condition, true_case, false_case } => {
      let iif = ind("if", depth);
      let ielse = ind("else", depth);
      let t_block = indent_block(true_case, depth);
      let f_block = indent_block(false_case, depth);
      format!("{iif} {condition} {t_block}\n{ielse} {f_block}")
    },
    Imp::ForElse { initialize, condition, afterthought, body, else_case } => {
      let ifor = ind("for", depth);
      let ielse = ind("else", depth);
      let body_block = indent_block(body, depth);
      let else_block = indent_block(else_case, depth);
      format!("{ifor} ({initialize};{condition};{afterthought}) {body_block}\n{ielse} {else_block}")
    },
    Imp::ForInElse { target, iterator, body, else_case } => {
      let ifor = ind("for", depth);
      let ielse = ind("else", depth);
      let body_block = indent_block(body, depth);
      let else_block = indent_block(else_case, depth);
      format!("{ifor} {target} in {iterator} {body_block}\n{ielse} {else_block}")
    },
    Imp::WhileElse { condition, body, else_case } => {
      let iwhile = ind("while", depth);
      let ielse = ind("else", depth);
      let body_block = indent_block(body, depth);
      let else_block = indent_block(else_case, depth);
      format!("{iwhile} {condition} {body_block}\n{ielse} {else_block}")
    },
    Imp::Label { name, stmt } => format!("{}: {}", ind(name, depth), pprint_imp(stmt, depth)),
    Imp::Return { value } => format!("{} {};", ind("return", depth), value),
    Imp::Goto { name } => format!("{} {};", ind("goto", depth), name),
    Imp::Continue => ind("continue;", depth),
    Imp::Break => ind("break;", depth),
    Imp::Pass => ind("pass;", depth),
  }
}

impl std::fmt::Display for Imp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", pprint_imp(self, 0))
  }
}

impl std::fmt::Display for Procedure {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let Procedure {name, args, body} = self;
    let body_block = indent_block(body, 0);
    let args = args.join(", ");
    write!(f, "{name}({args}) {body_block}")
  }
}

impl std::fmt::Display for Program {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let procs: Vec<String> = self.0.iter().map(|x| format!("{}", x)).collect();
    write!(f, "{}", procs.join("\n"))
  }
}
