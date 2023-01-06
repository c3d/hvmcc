pub mod compiler;

use pyo3::prelude::*;
use rustpython_parser::mode::Mode;
use rustpython_parser::parser as pyparser;
//use imp_hvm::compile_program;
use compiler::Compile;
//use hvm::Term;

#[pyclass]
pub struct Runtime {
  pub runtime: hvm::Runtime,
}

#[pyfunction]
pub fn init_runtime() -> Runtime {
  let size = hvm::default_heap_size();
  let tids = hvm::default_heap_tids();
  Runtime { runtime: hvm::Runtime::new(size, tids, true) }
}

#[pyfunction]
pub fn compile_rules(_rt: &mut Runtime, code: &str) -> PyResult<()> {
  let tree =
    pyparser::parse(code, Mode::Module, "<pyhvm>").expect("Could not parse function source.");
  let program = tree.compile()?;
  println!("{}", program);
  // let rules = compile_program(program);
  //let rulegroup = (ctx.file.rules.len(), rules);
  //hvm::rulebook::add_group(rt.runtime.book, rulegroup);
  // println!("{}", rules);
  Ok(())
}

// #[pyfunction]
// pub fn call_hvm(rt: &mut Runtime, func_name: String, args: Vec<PyObject>) -> PyResult<PyObject> {
//   let args = args.to_imp()?;
//   let call = Box::new(Term::Ctr { name: func_name , args });
//   let heap = hvm::runtime::new_heap(MEMORY_SIZE, THREAD_NUM);
//   let tids = hvm::runtime::new_tids(THREAD_NUM);
//   hvm::runtime::link(&heap, 0, hvm::runtime::Fun(rt.rulebook.name_to_id.get("PYHVM_MAIN_CALL").unwrap(), 0));
//   let host = 0;
//   hvm::runtime::normalize(&heap, &rt.program, &tids, host, false);
//   let term = hvm::language::readback::as_term(&heap, &rt.program, host);
//   let code = format!("{}", term);
//   hvm::runtime::collect(&heap, &rt.program.aris, tids[0], hvm::runtime::load_ptr(&heap, host));
//   hvm::runtime::free(&heap, 0, 0, 1);
//   todo!()
// }

#[pymodule]
fn pyhvm(_py: Python, m: &PyModule) -> PyResult<()> {
  m.add_function(pyo3::wrap_pyfunction!(compile_rules, m)?)?;
  m.add_function(pyo3::wrap_pyfunction!(init_runtime, m)?)?;
  // m.add_function(pyo3::wrap_pyfunction!(call_hvm, m)?)?;
  Ok(())
}
