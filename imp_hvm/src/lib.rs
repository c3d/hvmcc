pub mod imp;
pub mod fun;

pub use crate::imp::{Imp, Procedure, Program};
pub use crate::fun::{Fun, Rule};
// export type definitions as public interface

pub fn compile_program(program: Program) -> Vec<Rule> {
  todo!()
}
