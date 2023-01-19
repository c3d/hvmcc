mod braun;
mod structs;
mod to_ssa;

use crate::imp::Procedure;
pub use structs::*;
pub use to_ssa::Converter;

pub fn procedure_to_ssa(proc: Procedure) -> Converter {
  let mut converter = Converter::new();
  // Block 0 is the exit and 1 the entry, so we don't need to get the return here
  let _ = converter.convert_proc(proc);
  converter
}
