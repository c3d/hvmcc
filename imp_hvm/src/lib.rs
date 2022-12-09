pub mod fun;
pub mod imp;
pub mod to_hvm;

pub use crate::imp::*;
pub use crate::fun::*;
// it's easier to just export everything right now

use hvm::syntax::File;

// export type definitions as public interface

pub fn compile_program(program: Program) -> File {
  todo!()
}
