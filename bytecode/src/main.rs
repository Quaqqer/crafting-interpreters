mod chunk;
mod compiler;
mod lox;
mod op;
mod scanner;
mod value;
mod vm;
mod token;

fn main() {
    lox::cli();
}
