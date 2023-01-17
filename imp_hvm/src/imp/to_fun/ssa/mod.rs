mod braun;
mod structs;
mod to_ssa;

use crate::imp::Procedure;
pub use structs::*;
use to_ssa::Converter;

pub fn procedure_to_ssa(proc: Procedure) -> (Vec<self::Block>, self::BlockId) {
  let mut converter = Converter::new();
  let res_blk = converter.convert_proc(proc);
  (converter.ssa.blocks, res_blk)
}
