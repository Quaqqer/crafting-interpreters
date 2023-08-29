use crate::{
    chunk::Chunk,
    op::Op,
    scanner::Scanner,
    token::{Token, TokenKind},
};

pub struct Compiler {
    prev: Option<Token>,
    current: Option<Token>,
    scanner: Scanner,
    chunk: Chunk,
}

#[derive(Debug)]
pub struct Error {
    at: Option<Token>,
    kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    ScanError,
    Expected(TokenKind),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.at {
            Some(Token { line, .. }) => write!(f, "[line {}]", line)?,
            None => write!(f, "[line ?]")?,
        }

        write!(f, " Error")?;

        match &self.at {
            Some(Token {
                kind: TokenKind::EOF,
                ..
            }) => write!(f, " at end:\n")?,
            Some(Token { source, .. }) => write!(f, " at '{}':\n", source)?,
            None => write!(f, ":\n")?,
        }

        match &self.kind {
            ErrorKind::ScanError => match self.at.as_ref().unwrap().kind {
                TokenKind::UnknownChar => write!(f, "Unknown character"),
                TokenKind::UnterminatedString => write!(f, "Unterminated string"),
                _ => unreachable!(),
            },
            ErrorKind::Expected(expected) => {
                write!(
                    f,
                    "Expected {} but got {}",
                    expected,
                    &self.at.as_ref().unwrap().kind
                )
            }
        }
    }
}

impl Compiler {
    fn new(src: &str) -> Self {
        Compiler {
            current: None,
            prev: None,
            scanner: Scanner::new(src),
            chunk: Chunk::new(),
        }
    }

    fn advance(&mut self) -> Result<(), Error> {
        let next_token = self.scanner.scan_token();

        match &next_token {
            Token { kind, .. } => match kind {
                TokenKind::UnknownChar | TokenKind::UnterminatedString => {
                    return self.make_error(ErrorKind::ScanError)
                }
                _ => {}
            },
        }

        self.prev = self.current.take();
        self.current = Some(next_token);
        Ok(())
    }

    fn make_error<T>(&self, kind: ErrorKind) -> Result<T, Error> {
        Err(Error {
            at: self.current.clone(),
            kind,
        })
    }

    fn consume(&mut self, kind: TokenKind) -> Result<(), Error> {
        if self.current.as_ref().unwrap().kind == kind {
            self.advance()?;
            Ok(())
        } else {
            Err(Error {
                at: self.current.clone(),
                kind: ErrorKind::Expected(kind),
            })
        }
    }

    fn emit(&mut self, op: Op) {
        self.chunk.add_op(op, self.current.as_ref().unwrap().line);
    }

    fn run(&mut self) -> Result<(), Error> {
        self.advance()?;
        self.consume(TokenKind::EOF)?;
        self.emit(Op::Return);

        Ok(())
    }

    pub fn compile(src: &str) -> Result<Chunk, Error> {
        let mut compiler = Compiler::new(src);
        compiler.run()?;

        Ok(compiler.chunk)
    }
}
