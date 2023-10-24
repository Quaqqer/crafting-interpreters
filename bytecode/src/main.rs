//! The entry point to the Lox bytecode compiler and interpreter

#![warn(missing_docs)]

pub mod chunk;
pub mod compiler;
pub mod lox;
pub mod op;
pub mod scanner;
pub mod token;
pub mod value;
pub mod vm;
pub mod func;

fn main() {
    lox::cli();
}
