use crate::{
    chunk::Chunk,
    scanner::{self, Scanner},
};

pub struct Compiler {}

#[derive(Debug)]
pub enum Error {
    ScanError(scanner::Error),
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {}
    }

    pub fn compile(&self, source: &str) -> Result<Chunk, Error> {
        let mut scanner = Scanner::new(source);

        let mut tokens = Vec::new();

        while let Some(token) = scanner.scan_token().map_err(|msg| Error::ScanError(msg))? {
            tokens.push(token);
        }

        println!("{:?}", tokens);
        panic!()
    }
}
