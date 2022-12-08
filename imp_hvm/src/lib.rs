pub mod imp;
pub mod fun;
pub mod to_hvm;

pub use crate::imp::{Imp, Procedure, Program};
pub use crate::fun::Expr;

use hvm::syntax::File;

// export type definitions as public interface

pub fn compile_program(program: Program) -> File {
  todo!()
}
