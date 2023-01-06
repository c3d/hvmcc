#[macro_use]
extern crate lalrpop_util;
pub mod eval;
pub mod fun;
pub mod imp;
lalrpop_mod!(pub parser, "/parser.rs");

pub use crate::eval::*;
pub use crate::fun::*;
pub use crate::imp::*;
// it's easier to just export everything right now

use hvm::syntax::File;

// export type definitions as public interface

pub fn compile_program(program: Program) -> File {
  todo!()
}
