use super::ssa;
use crate::fun::{FuncProgram, Id};

pub fn ssa_proc_to_fun(converter: ssa::Converter) -> FuncProgram {
    // For each block in reverse order,
    // w3e want to check the used variables,
    // and convert the block into a lambda of these variables.
    // The previous block does it's own assignments and then calls its successor.
    // Later we can hoist these lambdas into functions if needed,
    // and use constant folding to collapse unnecessary indirections.
    todo!()
}