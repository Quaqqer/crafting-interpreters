mod chunk;
mod compiler;
mod lox;
mod op;
mod scanner;
mod token;
mod value;
mod vm;

fn main() {
    lox::cli();
}
