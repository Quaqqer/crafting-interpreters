use std::rc::Rc;

use crate::{
    chunk::Chunk,
    op::Op,
    scanner::Scanner,
    token::{Token as T, TokenKind as TK},
    value::{HeapValue, Value},
};

#[derive(Debug)]
struct Local {
    name: String,
    depth: i32,
}

pub struct Compiler {
    peek: Option<T>,
    scanner: Scanner,
    chunk: Chunk,
    locals: Vec<Local>,
    depth: i32,
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
    LocalLimit,
    DuplicateVar,
    Str(&'static str),
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
            ErrorKind::LocalLimit => write!(f, "Too many local variables in function."),
            ErrorKind::DuplicateVar => write!(f, "Already a variable with this name in this scope"),
            ErrorKind::Str(s) => write!(f, "{}", s),
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

    fn make_error<U>(&self, at: &T, kind: ErrorKind) -> Result<U, Error> {
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

    fn emit_string(&mut self, s: &str) -> u8 {
        self.chunk
            .add_constant(Value::HeapValue(Rc::new(HeapValue::String(s.to_string()))))
    }

    fn parse(&mut self) -> Result<(), Error> {
        while self.peek()?.kind != TK::EOF {
            self.parse_declaration()?;
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
        compiler.parse()?;

        Ok(compiler.chunk)
    }

    fn parse_precedence(&mut self, prec: Prec) -> Result<(), Error> {
        let t = self.peek()?;
        let rule = self.get_parse_rule(&t.kind);

        let can_assign = prec <= Prec::Assignment;

        if let Some(prefix) = rule.prefix {
            prefix(self, can_assign)?;
        } else {
            self.make_error(&t, ErrorKind::Expected("expression"))?;
        }

        loop {
            let t = self.peek()?;
            let rule = self.get_parse_rule(&t.kind);

            if prec <= rule.prec {
                rule.infix.unwrap()(self, can_assign)?;
            } else {
                break;
            }
        }

        if can_assign && self.peek()?.kind == TK::Equal {
            let t = self.peek()?;
            self.make_error(&t, ErrorKind::Str("Invalid assignment target"))?;
        }

        Ok(())
    }

    fn parse_binary(&mut self) -> Result<(), Error> {
        let t = self.advance()?;
        let operator = &t.kind;
        let rule = self.get_parse_rule(operator);
        self.parse_precedence(rule.prec.next())?;

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

    fn parse_expression(&mut self) -> Result<(), Error> {
        self.parse_precedence(Prec::Assignment)
    }

    fn parse_grouping(&mut self, can_assign: bool) -> Result<(), Error> {
        self.consume(TK::LParen)?;
        self.parse_expression()?;
        self.consume(TK::RParen)?;
        Ok(())
    }

    fn parse_number(&mut self, can_assign: bool) -> Result<(), Error> {
        let t = self.advance()?;
        let number: f64 = t.source.parse().unwrap();
        let constant = self.emit_constant(Value::Number(number));
        self.emit(&t, Op::Constant(constant));
        Ok(())
    }

    fn parse_unary(&mut self, can_assign: bool) -> Result<(), Error> {
        let t = self.advance()?;
        let operator = &t.kind;
        self.parse_precedence(Prec::Unary)?;
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

    fn parse_literal(&mut self, can_assign: bool) -> Result<(), Error> {
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
                let c = self.emit_string(&t.source[1..t.source.len() - 1]);
                self.emit(&t, Op::Constant(c));
                Ok(())
            }
            _ => self.make_error(&t, ErrorKind::Expected("literal")),
        }
    }

    fn get_parse_rule(&self, operator: &TK) -> ParseRule {
        match operator {
            TK::LParen => p_rule(Some(Box::new(|c, a| c.parse_grouping(a))), None, Prec::None),
            TK::RParen => p_rule(None, None, Prec::None),
            TK::LBrace => p_rule(None, None, Prec::None),
            TK::RBrace => p_rule(None, None, Prec::None),
            TK::Comma => p_rule(None, None, Prec::None),
            TK::Dot => p_rule(None, None, Prec::None),
            TK::Minus => p_rule(
                Some(Box::new(|c, a| c.parse_unary(a))),
                Some(Box::new(|c, _| c.parse_binary())),
                Prec::Term,
            ),
            TK::Plus => p_rule(None, Some(Box::new(|c, _| c.parse_binary())), Prec::Term),
            TK::Semicolon => p_rule(None, Some(Box::new(|c, _| c.parse_binary())), Prec::None),
            TK::Slash => p_rule(None, Some(Box::new(|c, _| c.parse_binary())), Prec::Factor),
            TK::Star => p_rule(None, Some(Box::new(|c, _| c.parse_binary())), Prec::Factor),
            TK::Bang => p_rule(Some(Box::new(|c, a| c.parse_unary(a))), None, Prec::None),
            TK::BangEqual => p_rule(
                None,
                Some(Box::new(|c, _| c.parse_binary())),
                Prec::Equality,
            ),
            TK::Equal => p_rule(None, None, Prec::None),
            TK::EqualEqual => p_rule(
                None,
                Some(Box::new(|c, _| c.parse_binary())),
                Prec::Equality,
            ),
            TK::Greater => p_rule(
                None,
                Some(Box::new(|c, _| c.parse_binary())),
                Prec::Comparison,
            ),
            TK::GreaterEqual => p_rule(
                None,
                Some(Box::new(|c, _| c.parse_binary())),
                Prec::Comparison,
            ),
            TK::Less => p_rule(
                None,
                Some(Box::new(|c, _| c.parse_binary())),
                Prec::Comparison,
            ),
            TK::LessEqual => p_rule(
                None,
                Some(Box::new(|c, _| c.parse_binary())),
                Prec::Comparison,
            ),
            TK::Identifier => p_rule(Some(Box::new(|c, a| c.parse_var(a))), None, Prec::None),
            TK::String => p_rule(Some(Box::new(|c, a| c.parse_literal(a))), None, Prec::None),
            TK::Number => p_rule(Some(Box::new(|c, a| c.parse_number(a))), None, Prec::None),
            TK::And => p_rule(None, None, Prec::None),
            TK::Class => p_rule(None, None, Prec::None),
            TK::Else => p_rule(None, None, Prec::None),
            TK::False => p_rule(Some(Box::new(|c, a| c.parse_literal(a))), None, Prec::None),
            TK::For => p_rule(None, None, Prec::None),
            TK::Fun => p_rule(None, None, Prec::None),
            TK::If => p_rule(None, None, Prec::None),
            TK::Nil => p_rule(Some(Box::new(|c, a| c.parse_literal(a))), None, Prec::None),
            TK::Or => p_rule(None, None, Prec::None),
            TK::Print => p_rule(None, None, Prec::None),
            TK::Return => p_rule(None, None, Prec::None),
            TK::Super => p_rule(None, None, Prec::None),
            TK::This => p_rule(None, None, Prec::None),
            TK::True => p_rule(Some(Box::new(|c, a| c.parse_literal(a))), None, Prec::None),
            TK::Var => p_rule(None, None, Prec::None),
            TK::While => p_rule(None, None, Prec::None),
            TK::EOF => p_rule(None, None, Prec::None),
            TK::UnknownChar => p_rule(None, None, Prec::None),
            TK::UnterminatedString => p_rule(None, None, Prec::None),
        }
    }

    fn parse_declaration(&mut self) -> Result<(), Error> {
        if self.peek()?.kind == TK::Var {
            self.parse_var_declaration()
        } else {
            self.parse_statement()
        }
    }

    fn parse_var_declaration(&mut self) -> Result<(), Error> {
        self.consume(TK::Var)?;
        let t = self.consume(TK::Identifier)?;

        self.declare_variable(&t)?;

        if self.peek()?.kind == TK::Equal {
            self.advance()?;
            self.parse_expression()?;
        } else {
            self.emit(&t, Op::Nil);
        }

        self.define_variable();

        if self.depth == 0 {
            let c = self.emit_string(t.source.as_str());

            self.emit(&t, Op::DefineGlobal(c));
        };

        self.consume(TK::Semicolon)?;
        Ok(())
    }

    fn declare_variable(&mut self, t: &T) -> Result<(), Error> {
        if self.depth == 0 {
            return Ok(());
        };

        for l in self.locals.iter().rev() {
            if l.depth != self.depth && l.depth != -1 {
                break;
            }

            if l.name == t.source {
                self.make_error(&t, ErrorKind::DuplicateVar)?;
            }
        }

        self.add_local(t)
    }

    fn define_variable(&mut self) {
        self.locals.last_mut().unwrap().depth = self.depth;
    }

    fn parse_statement(&mut self) -> Result<(), Error> {
        let t = self.peek()?;
        match t.kind {
            TK::Print => self.parse_print(),
            TK::LBrace => self.parse_block(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_print(&mut self) -> Result<(), Error> {
        let t = self.consume(TK::Print)?;
        self.parse_expression()?;
        self.emit(&t, Op::Print);
        self.consume(TK::Semicolon)?;
        Ok(())
    }

    fn parse_expression_statement(&mut self) -> Result<(), Error> {
        self.parse_expression()?;
        let t = self.consume(TK::Semicolon)?;
        self.emit(&t, Op::Pop);
        Ok(())
    }

    fn parse_var(&mut self, can_assign: bool) -> Result<(), Error> {
        let t = self.consume(TK::Identifier)?;

        let (get, set): (Op, Op) = {
            if let Some(i) = self.resolve_local(&t)? {
                (Op::GetLocal(i), Op::SetLocal(i))
            } else {
                let c = self.emit_string(&t.source);
                (Op::GetGlobal(c), Op::SetGlobal(c))
            }
        };

        if can_assign && self.peek()?.kind == TK::Equal {
            self.consume(TK::Equal)?;
            self.parse_expression()?;
            self.emit(&t, set);
        } else {
            self.emit(&t, get);
        }

        Ok(())
    }

    fn scoped<U, F>(&mut self, f: F) -> Result<U, Error>
    where
        F: Fn(&mut Self) -> Result<U, Error>,
    {
        self.depth += 1;

        let res = f(self)?;

        while let Some(l) = self.locals.last() {
            if l.depth == self.depth {
                self.locals.pop();
                // FIXME: Just emits a bad line, fix this
                self.emit(
                    &T {
                        kind: TK::EOF,
                        source: "".to_string(),
                        line: 0,
                    },
                    Op::Pop,
                );
            } else {
                break;
            }
        }
        self.depth -= 1;

        Ok(res)
    }

    fn parse_block(&mut self) -> Result<(), Error> {
        self.scoped(|c| {
            c.consume(TK::LBrace)?;

            loop {
                let tk = c.peek()?.kind;

                if tk == TK::RBrace {
                    break;
                } else {
                    c.parse_declaration()?;
                }
            }

            c.consume(TK::RBrace)?;

            Ok(())
        })
    }

    fn add_local(&mut self, t: &T) -> Result<(), Error> {
        let l = Local {
            name: t.source.clone(),
            depth: -1,
        };
        if self.locals.len() == u8::MAX as usize {
            return self.make_error(t, ErrorKind::LocalLimit);
        }
        self.locals.push(l);
        Ok(())
    }

    fn resolve_local(&self, t: &T) -> Result<Option<u8>, Error> {
        for (i, l) in self.locals.iter().enumerate().rev() {
            if l.name == t.source {
                if l.depth == -1 {
                    return self.make_error(
                        t,
                        ErrorKind::Str("Can't read a local variable in its own initializer"),
                    );
                }
                return Ok(Some(i as u8));
            }
        }
        Ok(None)
    }
}

struct ParseRule {
    prefix: Option<Box<dyn Fn(&mut Compiler, bool) -> Result<(), Error>>>,
    infix: Option<Box<dyn Fn(&mut Compiler, bool) -> Result<(), Error>>>,
    prec: Prec,
}

fn p_rule(
    prefix: Option<Box<dyn Fn(&mut Compiler, bool) -> Result<(), Error>>>,
    infix: Option<Box<dyn Fn(&mut Compiler, bool) -> Result<(), Error>>>,
    prec: Prec,
) -> ParseRule {
    ParseRule {
        prefix,
        infix,
        prec,
    }
}
