use std::fs;
use std::io::Write;
use std::path::Path;

use imp_hvm::Procedure;
use pretty_assertions::assert_eq;
use walkdir::WalkDir;

use imp_hvm::fun::to_hvm::compile_function;
use imp_hvm::imp::Imp;
use imp_hvm::parser::{FuncProgramParser, FunctionParser, ProgramParser};

fn golden_test(path: &Path, run: &dyn Fn(&Path) -> String) {
  let result = run(path);

  let golden_path = path.with_extension("golden");
  if let Ok(to_check) = fs::read_to_string(golden_path.clone()) {
    assert_eq!(result, to_check, "Testing file '{}'", path.display());
  } else {
    let mut file = fs::File::create(golden_path).unwrap();
    file.write_all(result.as_bytes()).unwrap();
  }
}

fn golden_test_dir(root: &Path, ext: &str, run: &dyn Fn(&Path) -> String) {
  for entry in WalkDir::new(root).follow_links(true) {
    let entry = entry.unwrap();
    let path = entry.path();
    if path.is_file() && path.extension().map(|x| x == ext).unwrap_or(false) {
      golden_test(path, run)
    }
  }
}

#[test]
fn test_fun_to_hvm() {
  let run = |path: &Path| {
    let input = fs::read_to_string(path).unwrap();
    match FunctionParser::new().parse(&input) {
      Ok(fun_ast) => match compile_function(fun_ast) {
        Ok(hvm_ast) => hvm_ast.to_string(),
        Err(err) => err.to_string(),
      },
      Err(err) => err.to_string(),
    }
  };
  golden_test_dir(Path::new("./tests/fun_to_hvm"), "hvmcc", &run);
}

#[test]
fn test_fun_eval() {
  let run = |path: &Path| {
    let input = fs::read_to_string(path).unwrap();
    match FuncProgramParser::new().parse(&input) {
      Ok(fprog) => {
        let imp_hvm::FuncProgram(funcs) = fprog;
        let mut env = imp_hvm::eval::Env::new();
        env.add_functions(funcs);
        if let imp_hvm::eval::Callable::Function(main) = env.rules.get("Main").unwrap() {
          let main = main.clone();
          let eval_result = imp_hvm::eval::eval_func(&mut env, &main, &vec![]);
          match eval_result {
            Ok(expr) => expr.to_string(),
            Err(err) => format!("{:?}", err),
          }
        } else {
          String::from("'Main' is a Procedure and not a Function.")
        }
      }
      Err(err) => err.to_string(),
    }
  };
  golden_test_dir(Path::new("./tests/fun_eval"), "hvmcc", &run);
}

#[test]
fn test_imp_ssa() {
  let run = |path: &Path| {
    let input = fs::read_to_string(path).unwrap();
    match ProgramParser::new().parse(&input) {
      Ok(prog) => {
        let body = Imp::Block { stmts: prog.0 };
        let main = Procedure { name: "Main".into(), args: vec![], body };
        let blocks = imp_hvm::imp::to_fun::ssa::procedure_to_ssa(main);
        format!("{blocks:#?}")
      }
      Err(err) => err.to_string(),
    }
  };
  golden_test_dir(Path::new("./tests/imp_ssa"), "hvmcc", &run);
}

#[test]
fn test_imp_parse() {
  let run = |path: &Path| {
    let input = fs::read_to_string(path).unwrap();
    match ProgramParser::new().parse(&input) {
      Ok(prog) => prog.to_string(),
      Err(err) => err.to_string(),
    }
  };
  golden_test_dir(Path::new("./tests/imp_parse"), "hvmcc", &run);
}

#[test]
fn test_fun_parse() {
  let run = |path: &Path| {
    let input = fs::read_to_string(path).unwrap();
    match FuncProgramParser::new().parse(&input) {
      Ok(prog) => prog.to_string(),
      Err(err) => err.to_string(),
    }
  };
  golden_test_dir(Path::new("./tests/fun_parse"), "hvmcc", &run);
}
