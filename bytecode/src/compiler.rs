use std::rc::Rc;

use crate::{
    chunk::Chunk,
    op::Op,
    scanner::Scanner,
    token::{Token as T, TokenKind as TK},
    value::{HeapValue, Value},
};

struct Local {
    name: String,
    depth: u32,
}

pub struct Compiler {
    peek: Option<T>,
    scanner: Scanner,
    chunk: Chunk,
    locals: Vec<Local>,
    depth: u32,
}

#[derive(Debug)]
pub struct Error {
    at: T,
    kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    ScanError,
    ExpectedT(TK),
    Expected(&'static str),
}

#[derive(PartialEq, PartialOrd, Debug)]
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
            T { line, .. } => write!(f, "[line {}]", line)?,
        }

        write!(f, " Error")?;

        match &self.at {
            T { kind: TK::EOF, .. } => write!(f, " at end:\n")?,
            T { source, .. } => write!(f, " at '{}':\n", source)?,
        }

        match &self.kind {
            ErrorKind::ScanError => match self.at.kind {
                TK::UnknownChar => write!(f, "Unknown character"),
                TK::UnterminatedString => write!(f, "Unterminated string"),
                _ => unreachable!(),
            },
            ErrorKind::ExpectedT(expected) => {
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
            locals: Vec::new(),
            depth: 0,
        }
    }

    fn peek(&mut self) -> Result<T, Error> {
        if self.peek.is_none() {
            self.peek = Some(self.advance()?);
        }
        Ok(self.peek.as_ref().unwrap().clone())
    }

    fn advance(&mut self) -> Result<T, Error> {
        if let Some(peeked) = self.peek.take() {
            Ok(peeked)
        } else {
            let t = self.scanner.scan_token();

            match &t {
                T { kind, .. } => match kind {
                    TK::UnknownChar | TK::UnterminatedString => {
                        return self.make_error(&t, ErrorKind::ScanError)
                    }
                    _ => {}
                },
            }
            Ok(t)
        }
    }

    fn make_error<T>(&self, at: &T, kind: ErrorKind) -> Result<T, Error> {
        Err(Error {
            at: at.clone(),
            kind,
        })
    }

    fn consume(&mut self, kind: TK) -> Result<T, Error> {
        let next = self.advance()?;
        if next.kind == kind {
            Ok(next)
        } else {
            self.make_error(&next, ErrorKind::ExpectedT(kind))
        }
    }

    fn emit(&mut self, token: &T, op: Op) {
        self.chunk.add_op(op, token.line);
    }

    fn emits(&mut self, token: &T, ops: &[Op]) {
        for op in ops {
            self.chunk.add_op(op.clone(), token.line);
        }
    }

    fn emit_constant(&mut self, v: Value) -> u8 {
        self.chunk.add_constant(v)
    }

    fn run(&mut self) -> Result<(), Error> {
        while self.peek()?.kind != TK::EOF {
            self.declaration()?;
        }

        self.consume(TK::EOF)?;

        #[cfg(feature = "debug_trace")]
        {
            println!("=== Code ===\n{}", self.chunk);
        }

        Ok(())
    }

    pub fn compile(src: &str) -> Result<Chunk, Error> {
        let mut compiler = Compiler::new(src);
        compiler.run()?;

        Ok(compiler.chunk)
    }

    fn precedence(&mut self, prec: Prec) -> Result<(), Error> {
        let t = self.peek()?;
        let rule = self.rule(&t.kind);
        if let Some(prefix) = rule.prefix {
            prefix(self)?;
        } else {
            self.make_error(&t, ErrorKind::Expected("expression"))?;
        }

        loop {
            let t = self.peek()?;
            let rule = self.rule(&t.kind);

            if prec <= rule.prec {
                rule.infix.unwrap()(self)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn binary(&mut self) -> Result<(), Error> {
        let t = self.advance()?;
        let operator = &t.kind;
        let rule = self.rule(operator);
        self.precedence(rule.prec.next())?;

        match operator {
            TK::Plus => self.emit(&t, Op::Add),
            TK::Minus => self.emit(&t, Op::Subtract),
            TK::Star => self.emit(&t, Op::Multiply),
            TK::Slash => self.emit(&t, Op::Divide),
            TK::BangEqual => self.emits(&t, &[Op::Equal, Op::Not]),
            TK::EqualEqual => self.emit(&t, Op::Equal),
            TK::Greater => self.emit(&t, Op::Greater),
            TK::GreaterEqual => self.emits(&t, &[Op::Less, Op::Not]),
            TK::Less => self.emit(&t, Op::Less),
            TK::LessEqual => self.emits(&t, &[Op::Greater, Op::Not]),
            _ => unreachable!(),
        };

        Ok(())
    }

    fn expression(&mut self) -> Result<(), Error> {
        self.precedence(Prec::Assignment)
    }

    fn grouping(&mut self) -> Result<(), Error> {
        self.consume(TK::LParen)?;
        self.expression()?;
        self.consume(TK::RParen)?;
        Ok(())
    }

    fn number(&mut self) -> Result<(), Error> {
        let t = self.advance()?;
        let number: f64 = t.source.parse().unwrap();
        let constant = self.emit_constant(Value::Number(number));
        self.emit(&t, Op::Constant(constant));
        Ok(())
    }

    fn unary(&mut self) -> Result<(), Error> {
        let t = self.advance()?;
        let operator = &t.kind;
        self.precedence(Prec::Unary)?;
        match operator {
            TK::Minus => {
                self.emit(&t, Op::Negate);
                Ok(())
            }
            TK::Bang => {
                self.emit(&t, Op::Not);
                Ok(())
            }
            _ => unreachable!(),
        }
    }

    fn literal(&mut self) -> Result<(), Error> {
        let t = self.advance()?;
        match &t.kind {
            TK::True => {
                self.emit(&t, Op::True);
                Ok(())
            }
            TK::False => {
                self.emit(&t, Op::False);
                Ok(())
            }
            TK::Nil => {
                self.emit(&t, Op::Nil);
                Ok(())
            }
            TK::String => {
                let c = self.emit_constant(Value::HeapValue(Rc::new(HeapValue::String(
                    t.source[1..t.source.len() - 1].to_string(),
                ))));
                self.emit(&t, Op::Constant(c));
                Ok(())
            }
            _ => self.make_error(&t, ErrorKind::Expected("literal")),
        }
    }

    fn rule(&self, operator: &TK) -> ParseRule {
        match operator {
            TK::LParen => p_rule(Some(Box::new(|c| c.grouping())), None, Prec::None),
            TK::RParen => p_rule(None, None, Prec::None),
            TK::LBrace => p_rule(None, None, Prec::None),
            TK::RBrace => p_rule(None, None, Prec::None),
            TK::Comma => p_rule(None, None, Prec::None),
            TK::Dot => p_rule(None, None, Prec::None),
            TK::Minus => p_rule(
                Some(Box::new(|c| c.unary())),
                Some(Box::new(|c| c.binary())),
                Prec::Term,
            ),
            TK::Plus => p_rule(None, Some(Box::new(|c| c.binary())), Prec::Term),
            TK::Semicolon => p_rule(None, Some(Box::new(|c| c.binary())), Prec::None),
            TK::Slash => p_rule(None, Some(Box::new(|c| c.binary())), Prec::Factor),
            TK::Star => p_rule(None, Some(Box::new(|c| c.binary())), Prec::Factor),
            TK::Bang => p_rule(Some(Box::new(|c| c.unary())), None, Prec::None),
            TK::BangEqual => p_rule(None, Some(Box::new(|c| c.binary())), Prec::Equality),
            TK::Equal => p_rule(None, None, Prec::None),
            TK::EqualEqual => p_rule(None, Some(Box::new(|c| c.binary())), Prec::Equality),
            TK::Greater => p_rule(None, Some(Box::new(|c| c.binary())), Prec::Comparison),
            TK::GreaterEqual => p_rule(None, Some(Box::new(|c| c.binary())), Prec::Comparison),
            TK::Less => p_rule(None, Some(Box::new(|c| c.binary())), Prec::Comparison),
            TK::LessEqual => p_rule(None, Some(Box::new(|c| c.binary())), Prec::Comparison),
            TK::Identifier => p_rule(Some(Box::new(|c| c.var())), None, Prec::None),
            TK::String => p_rule(Some(Box::new(|c| c.literal())), None, Prec::None),
            TK::Number => p_rule(Some(Box::new(|c| c.number())), None, Prec::None),
            TK::And => p_rule(None, None, Prec::None),
            TK::Class => p_rule(None, None, Prec::None),
            TK::Else => p_rule(None, None, Prec::None),
            TK::False => p_rule(Some(Box::new(|c| c.literal())), None, Prec::None),
            TK::For => p_rule(None, None, Prec::None),
            TK::Fun => p_rule(None, None, Prec::None),
            TK::If => p_rule(None, None, Prec::None),
            TK::Nil => p_rule(Some(Box::new(|c| c.literal())), None, Prec::None),
            TK::Or => p_rule(None, None, Prec::None),
            TK::Print => p_rule(None, None, Prec::None),
            TK::Return => p_rule(None, None, Prec::None),
            TK::Super => p_rule(None, None, Prec::None),
            TK::This => p_rule(None, None, Prec::None),
            TK::True => p_rule(Some(Box::new(|c| c.literal())), None, Prec::None),
            TK::Var => p_rule(None, None, Prec::None),
            TK::While => p_rule(None, None, Prec::None),
            TK::EOF => p_rule(None, None, Prec::None),
            TK::UnknownChar => p_rule(None, None, Prec::None),
            TK::UnterminatedString => p_rule(None, None, Prec::None),
        }
    }

    fn declaration(&mut self) -> Result<(), Error> {
        if self.peek()?.kind == TK::Var {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<(), Error> {
        self.consume(TK::Var)?;
        let t = self.consume(TK::Identifier)?;

        if self.peek()?.kind == TK::Equal {
            self.advance()?;
            self.expression()?;
        } else {
            self.emit(&t, Op::Nil);
        }

        let c = self.emit_constant(Value::HeapValue(Rc::new(HeapValue::String(
            t.source.clone(),
        ))));
        self.emit(&t, Op::DefineGlobal(c));

        self.consume(TK::Semicolon)?;
        Ok(())
    }

    fn statement(&mut self) -> Result<(), Error> {
        let t = self.peek()?;
        match t.kind {
            TK::Print => self.print(),
            TK::LBrace => {
                self.begin_scope()?;
                self.block()?;
                self.end_scope()?;
                Ok(())
            }
            _ => self.expression_statement(),
        }
    }

    fn print(&mut self) -> Result<(), Error> {
        let t = self.consume(TK::Print)?;
        self.expression()?;
        self.emit(&t, Op::Print);
        self.consume(TK::Semicolon)?;
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), Error> {
        self.expression()?;
        let t = self.consume(TK::Semicolon)?;
        self.emit(&t, Op::Pop);
        Ok(())
    }

    fn var(&mut self) -> Result<(), Error> {
        let t = self.consume(TK::Identifier)?;
        let c = self.emit_constant(Value::HeapValue(Rc::new(HeapValue::String(
            t.source.clone(),
        ))));
        self.emit(&t, Op::GetGlobal(c));
        Ok(())
    }

    fn begin_scope(&mut self) -> Result<(), Error> {
        self.consume(TK::LBrace)?;
        self.depth += 1;
        Ok(())
    }

    fn block(&mut self) -> Result<(), Error> {
        loop {
            let tk = self.peek()?.kind;

            if tk == TK::RBrace {
                break;
            } else {
                self.declaration()?;
            }
        }

        Ok(())
    }

    fn end_scope(&mut self) -> Result<(), Error> {
        self.consume(TK::LBrace)?;
        self.depth -= 1;
        Ok(())
    }
}

struct ParseRule {
    prefix: Option<Box<dyn Fn(&mut Compiler) -> Result<(), Error>>>,
    infix: Option<Box<dyn Fn(&mut Compiler) -> Result<(), Error>>>,
    prec: Prec,
}

fn p_rule(
    prefix: Option<Box<dyn Fn(&mut Compiler) -> Result<(), Error>>>,
    infix: Option<Box<dyn Fn(&mut Compiler) -> Result<(), Error>>>,
    prec: Prec,
) -> ParseRule {
    ParseRule {
        prefix,
        infix,
        prec,
    }
}
