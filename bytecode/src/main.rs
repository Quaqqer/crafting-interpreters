use crate::{chunk::Chunk, op::Op, value::Value, vm::VM};

mod chunk;
mod lox;
mod op;
mod value;
mod vm;

fn main() {
    lox::cli();
}
