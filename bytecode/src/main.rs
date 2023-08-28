use crate::{chunk::Chunk, op::Op, value::Value, vm::VM};

mod chunk;
mod op;
mod value;
mod vm;

fn main() {
    let mut chunk = Chunk::new();
    let f_offset = chunk.add_constant(Value::Float(69420.));
    chunk.add_op(Op::Constant(f_offset), 0);
    chunk.add_op(Op::Return, 0);
    println!("=== Chunk ===");
    println!("{}", chunk);

    let mut vm = VM::new(chunk);
    let res = vm.run();
    println!("{:?}", res);
}
