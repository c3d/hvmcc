mod compile;
use compile::{Compile, Ctx, JSResult};
use imp_hvm::imp::Program as ImpProgram;
use std::path::Path;
use swc_common::SourceMap;
use swc_ecma_parser::{parse_file_as_program, Syntax};

pub fn js_to_imp(filename: &Path) -> JSResult<ImpProgram> {
  let cm: SourceMap = Default::default();
  let filem = cm.load_file(filename).expect("could not load file");
  let prog = parse_file_as_program(
    &filem,
    Syntax::Es(Default::default()),
    Default::default(),
    None,
    &mut vec![],
  )
  .expect("Could not parse program.");
  let prog = prog.compile(&mut Ctx::new())?;
  Ok(prog)
}
