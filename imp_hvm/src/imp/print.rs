use crate::fun::print::pprint_fun;
use crate::{CaseStmt, Imp, Program};

/// Pads the string n characters to the right
pub fn ind(string: &str, n: usize) -> String {
  let pad = " ".repeat(n);
  format!("{pad}{string}")
}

// Converts a Vec<T> to a string by applying some mapping to each element, placing the results in separate lines
fn vec_to_string<T>(elems: &[T], func: &dyn Fn(&T) -> String, sep: &str) -> String {
  elems.iter().map(func).collect::<Vec<String>>().join(sep)
}

// Converts a body and else blocks of a statement into a `begin stmts; end else begin stmts; end` string
fn pprint_block_else(body: &Imp, else_case: &Imp, depth: usize) -> String {
  let body = pprint_imp(body, depth + 2);
  let i_else = ind("else", depth);
  let else_case = pprint_imp(else_case, depth + 2);
  format!("\n{body}\n{i_else}\n{else_case}")
}

fn pprint_imp(imp: &Imp, depth: usize) -> String {
  match imp {
    Imp::Assignment { name, expr } => {
      let name = ind(name, depth);
      format!("{name} = {expr};")
    }
    Imp::Expression { expr } => {
      let expr = pprint_fun(expr, depth);
      format!("{expr};")
    }
    Imp::MatchStmt { expr, cases, default } => {
      fn display_case(case: &CaseStmt, depth: usize) -> String {
        let CaseStmt { matched, body } = case;
        let matched = pprint_fun(matched, depth);
        let b_block = pprint_imp(body, depth + 2);
        format!("{matched} =>\n{b_block}",)
      }
      let cases = vec_to_string(cases, &|x| display_case(x, depth + 2), "\n");
      let imatch = ind("match", depth);
      let ielse = ind("else", depth);
      let dflt_block = pprint_imp(default, depth + 2);
      format!("{imatch} {expr} \n{cases}\n{ielse}\n{dflt_block}\nend")
    }
    Imp::IfElse { condition, true_case, false_case } => {
      let iif = ind("if", depth);
      let blocks = pprint_block_else(true_case, false_case, depth);
      format!("{iif} {condition} {blocks}")
    }
    Imp::ForElse { initialize, condition, afterthought, body, else_case } => {
      let ifor = ind("for", depth);
      // TODO: breaks indentation if there's a block expression in the for
      let blocks = pprint_block_else(body, else_case, depth);
      format!("{ifor} ({initialize}, {condition}, {afterthought}) {blocks}")
    }
    Imp::ForInElse { target, iterator, body, else_case } => {
      let ifor = ind("for", depth);
      let blocks = pprint_block_else(body, else_case, depth);
      format!("{ifor} {target} in {iterator} {blocks}")
    }
    Imp::WhileElse { condition, body, else_case } => {
      let iwhile = ind("while", depth);
      let blocks = pprint_block_else(body, else_case, depth);
      format!("{iwhile} {condition} {blocks}")
    }
    Imp::Label { name, stmt } => {
      format!("{}: {}", ind(name, depth), pprint_imp(stmt, depth))
    }
    Imp::Return { value } => format!("{} {};", ind("return", depth), value),
    Imp::Goto { name } => format!("{} {};", ind("goto", depth), name),
    Imp::Continue => ind("continue;", depth),
    Imp::Break => ind("break;", depth),
    Imp::Pass => ind("pass;", depth),
    Imp::ProcedureDef { name, args, body } => {
      let procedure = ind("procedure", depth);
      let args = args.join(", ");
      let body_block = pprint_imp(body, depth + 2);
      format!("{procedure} {name} ({args})\n{body_block}")
    }
    Imp::Block { stmts } => {
      let stmts = vec_to_string(stmts, &|x| pprint_imp(x, depth + 2), "\n");
      let begin = ind("begin", depth);
      let end = ind("end", depth);
      format!("{begin}\n{stmts}\n{end}")
    }
  }
}

impl std::fmt::Display for Imp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", pprint_imp(self, 0))
  }
}

impl std::fmt::Display for Program {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let procs = vec_to_string(&self.0, &|x| format!("{x}"), "\n");
    write!(f, "{procs}")
  }
}
