use crate::{Imp, CaseStmt, Procedure, Program};

// Display trait

impl std::fmt::Display for Imp {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Imp::Sequence { stmt1, stmt2 } => write!(f, "{}\n{}", stmt1, stmt2),
      Imp::Assignment { name, expr } => write!(f, "{} = {};", name, expr),
      Imp::Expression { expr } => write!(f, "{};", expr),
      Imp::MatchStmt { expr, cases, default } => {
        fn display_case(case: &CaseStmt) -> String {
          let CaseStmt { matched, body } = case;
          format!("{} => {}", matched, body)
        }
        let cases = cases.iter().map(display_case).collect::<Vec<String>>().join("\n");        
        if let Some(default) = default {
          let default_string = format!("default => {}", default);
          let cases = [&cases[..], &default_string].concat();         
          return write!(f, "match {} {{\n{}\n}}", expr, cases)
        }//this is so ugly, maybe there is a less convoluted way of doing this.
        write!(f, "match {} {{\n{}\n}}", expr, cases)
      },
      Imp::IfElse { condition, true_case, false_case } => write!(f, "if {} {{\n{}\n}} else {{\n{}\n}}", condition, true_case, false_case),
      Imp::ForElse { initialize, condition, afterthought, body, else_case } => {
        write!(f, "for ({};{};{}) {{\n{}\n}} else {{\n{}\n}}", initialize, condition, afterthought, body, else_case)
      },
      Imp::ForInElse { target, iterator, body, else_case } => write!(f, "for {} in {} {{\n{}\n}} else {{\n{}\n}}", target, iterator, body, else_case),
      Imp::WhileElse { condition, body, else_case } => write!(f, "while {} {{\n{}\n}} else {{\n{}\n}}", condition, body, else_case),
      Imp::Label { name, stmt } => write!(f, "{}: {}", name, stmt),
      Imp::Return { value } => write!(f, "return {};", value),
      Imp::Goto { name } => write!(f, "goto {};", name),
      Imp::Continue => write!(f, "continue;"),
      Imp::Break => write!(f, "break;"),
      Imp::Pass => write!(f, "pass;"),
    }
  }
}

impl std::fmt::Display for Procedure {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let Procedure {name, args, body} = self;
    let args = args.join(", ");
    write!(f, "{}({}) {{ {} }}", name, args, body)
  }
}

impl std::fmt::Display for Program {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let procs: Vec<String> = self.0.iter().map(|x| format!("{}", x)).collect();
    write!(f, "{}", procs.join("\n"))
  }
}
