use crate::{
    chunk::Chunk,
    op::Op,
    scanner::Scanner,
    token::{Token, TokenKind},
    value::Value,
};

pub struct Compiler {
    peek: Option<Token>,
    scanner: Scanner,
    chunk: Chunk,
}

#[derive(Debug)]
pub struct Error {
    at: Token,
    kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    ScanError,
    ExpectedToken(TokenKind),
    Expected(&'static str),
}

#[derive(PartialEq, PartialOrd)]
enum Prec {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Prec {
    pub fn next(&self) -> Self {
        match self {
            Prec::None => Prec::Assignment,
            Prec::Assignment => Prec::Or,
            Prec::Or => Prec::And,
            Prec::And => Prec::Equality,
            Prec::Equality => Prec::Comparison,
            Prec::Comparison => Prec::Term,
            Prec::Term => Prec::Factor,
            Prec::Factor => Prec::Unary,
            Prec::Unary => Prec::Call,
            Prec::Call => Prec::Primary,
            Prec::Primary => unreachable!(),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.at {
            Token { line, .. } => write!(f, "[line {}]", line)?,
        }

        write!(f, " Error")?;

        match &self.at {
            Token {
                kind: TokenKind::EOF,
                ..
            } => write!(f, " at end:\n")?,
            Token { source, .. } => write!(f, " at '{}':\n", source)?,
        }

        match &self.kind {
            ErrorKind::ScanError => match self.at.kind {
                TokenKind::UnknownChar => write!(f, "Unknown character"),
                TokenKind::UnterminatedString => write!(f, "Unterminated string"),
                _ => unreachable!(),
            },
            ErrorKind::ExpectedToken(expected) => {
                write!(f, "Expected {} but got {}", expected, &self.at.kind)
            }
            ErrorKind::Expected(s) => write!(f, "Expected {} but got {}", s, self.at.kind),
        }
    }
}

impl Compiler {
    fn new(src: &str) -> Self {
        Compiler {
            peek: None,
            scanner: Scanner::new(src),
            chunk: Chunk::new(),
        }
    }

    fn peek(&mut self) -> Result<&Token, Error> {
        if self.peek.is_none() {
            self.peek = Some(self.advance()?);
        }
        Ok(self.peek.as_ref().unwrap())
    }

    fn advance(&mut self) -> Result<Token, Error> {
        self.peek = None;

        let t = self.scanner.scan_token();

        match &t {
            Token { kind, .. } => match kind {
                TokenKind::UnknownChar | TokenKind::UnterminatedString => {
                    return self.make_error(t, ErrorKind::ScanError)
                }
                _ => {}
            },
        }
        Ok(t)
    }

    fn make_error<T>(&self, at: Token, kind: ErrorKind) -> Result<T, Error> {
        Err(Error { at, kind })
    }

    fn consume(&mut self, kind: TokenKind) -> Result<Token, Error> {
        let next = self.advance()?;
        if next.kind == kind {
            Ok(next)
        } else {
            self.make_error(next, ErrorKind::ExpectedToken(kind))
        }
    }

    fn emit(&mut self, token: &Token, op: Op) {
        self.chunk.add_op(op, token.line);
    }

    fn emit_constant(&mut self, v: Value) -> u8 {
        self.chunk.add_constant(v)
    }

    fn run(&mut self) -> Result<(), Error> {
        self.expression()?;

        let t = self.consume(TokenKind::EOF)?;
        self.emit(&t, Op::Return);

        Ok(())
    }

    pub fn compile(src: &str) -> Result<Chunk, Error> {
        let mut compiler = Compiler::new(src);
        compiler.run()?;

        Ok(compiler.chunk)
    }

    fn precedence(&mut self, prec: Prec) -> Result<(), Error> {
        let t = self.advance()?;
        match t.kind {
            TokenKind::LParen => self.grouping(),
            TokenKind::Number => self.number(t),
            TokenKind::Minus => self.unary(t),
            _ => self.make_error(t, ErrorKind::Expected("expression")),
        }
    }

    fn binary(&mut self) -> Result<(), Error> {
        let t = self.advance()?;
        let operator = &t.kind;
        let rule = self.rule(operator.clone());
        self.precedence(rule.next())?;

        match operator {
            TokenKind::Plus => self.emit(&t, Op::Add),
            TokenKind::Minus => self.emit(&t, Op::Subtract),
            TokenKind::Star => self.emit(&t, Op::Multiply),
            TokenKind::Slash => self.emit(&t, Op::Divide),
            _ => unreachable!(),
        };

        Ok(())
    }

    fn expression(&mut self) -> Result<(), Error> {
        self.precedence(Prec::Assignment)
    }

    fn grouping(&mut self) -> Result<(), Error> {
        self.expression()?;
        self.consume(TokenKind::RParen)?;
        Ok(())
    }

    fn number(&mut self, t: Token) -> Result<(), Error> {
        let number: f64 = t.source.parse().unwrap();
        let constant = self.emit_constant(Value::Float(number));
        self.emit(&t, Op::Constant(constant));
        Ok(())
    }

    fn unary(&mut self, t: Token) -> Result<(), Error> {
        let operator = &t.kind;
        self.expression()?;
        match operator {
            TokenKind::Minus => {
                self.emit(&t, Op::Negate);
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn rule(&self, operator: TokenKind) -> Prec {
        todo!()
    }
}
