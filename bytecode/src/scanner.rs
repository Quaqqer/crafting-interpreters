use crate::token::{Token, TokenKind};

pub struct Scanner {
    source: Vec<char>,
    start: usize,
    current: usize,
    line: u32,
}

#[derive(Debug)]
pub enum Error {
    UnknownChar(char),
    UnterminatedString,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Result<Option<Token>, Error> {
        self.skip_whitespace();

        self.start = self.current;

        let c = self.advance();

        match c {
            None => return Ok(None),
            Some(c) => match c {
                '(' => self.make_token(TokenKind::LParen),
                ')' => self.make_token(TokenKind::RParen),
                '{' => self.make_token(TokenKind::LBrace),
                '}' => self.make_token(TokenKind::RBrace),
                ';' => self.make_token(TokenKind::Semicolon),
                ',' => self.make_token(TokenKind::Comma),
                '.' => self.make_token(TokenKind::Dot),
                '-' => self.make_token(TokenKind::Minus),
                '+' => self.make_token(TokenKind::Plus),
                '/' => self.make_token(TokenKind::Slash),
                '*' => self.make_token(TokenKind::Star),
                '!' => {
                    let kind = if self.matches('=') {
                        TokenKind::BangEqual
                    } else {
                        TokenKind::Bang
                    };
                    self.make_token(kind)
                }
                '=' => {
                    let kind = if self.matches('=') {
                        TokenKind::EqualEqual
                    } else {
                        TokenKind::Equal
                    };
                    self.make_token(kind)
                }
                '<' => {
                    let kind = if self.matches('=') {
                        TokenKind::LessEqual
                    } else {
                        TokenKind::Less
                    };
                    self.make_token(kind)
                }
                '>' => {
                    let kind = if self.matches('=') {
                        TokenKind::GreaterEqual
                    } else {
                        TokenKind::Greater
                    };
                    self.make_token(kind)
                }
                '"' => self.make_string(),
                c if c.is_digit(10) => self.make_number(),
                c if is_ident_head(c) => self.make_ident_or_kw(),
                c => Err(Error::UnknownChar(c)),
            },
        }
    }

    fn matches(&mut self, c: char) -> bool {
        match self.peek() {
            Some(c_) if c == c_ => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn peek(&self) -> Option<char> {
        self.peek_offset(0)
    }

    fn peek_offset(&self, offset: usize) -> Option<char> {
        self.source.get(self.current + offset).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peek();
        self.current += 1;
        c
    }

    fn make_token(&self, kind: TokenKind) -> Result<Option<Token>, Error> {
        Ok(Some(Token {
            kind,
            source: self.token_contents(),
            line: self.line,
        }))
    }

    fn make_string(&mut self) -> Result<Option<Token>, Error> {
        loop {
            let c = self.advance().ok_or(Error::UnterminatedString)?;

            if c == '"' {
                return self.make_token(TokenKind::String);
            }
        }
    }

    fn make_number(&mut self) -> Result<Option<Token>, Error> {
        self.advance_while(|c| c.is_ascii_digit());

        if matches!(self.peek(), Some('.'))
            && matches!(self.peek_offset(1), Some(c) if c.is_ascii_digit())
        {
            self.advance();
            self.advance_while(|c| c.is_ascii_digit());
        }

        self.make_token(TokenKind::Number)
    }

    fn make_ident_or_kw(&mut self) -> Result<Option<Token>, Error> {
        self.advance_while(|c| is_ident_tail(c));
        let kind = match self.token_contents().as_str() {
            "and" => TokenKind::And,
            "class" => TokenKind::Class,
            "else" => TokenKind::Else,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "fun" => TokenKind::Fun,
            "if" => TokenKind::If,
            "nil" => TokenKind::Nil,
            "or" => TokenKind::Or,
            "print" => TokenKind::Print,
            "return" => TokenKind::Return,
            "super" => TokenKind::Super,
            "this" => TokenKind::This,
            "true" => TokenKind::True,
            "var" => TokenKind::Var,
            "while" => TokenKind::While,
            _ => TokenKind::Identifier,
        };
        self.make_token(kind)
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(c) if " \r\t".contains(c) => {
                    self.advance();
                }
                Some(c) if c == '\n' => {
                    self.line += 1;
                    self.advance();
                }
                Some(c) if c == '/' && self.peek_offset(1) == Some('/') => {
                    self.advance_while(|c| c != '\n');
                }
                _ => break,
            }
        }
    }

    fn token_contents(&self) -> String {
        String::from_iter(self.source[self.start..self.current].iter())
    }

    fn advance_while<F>(&mut self, mut pred: F)
    where
        F: FnMut(char) -> bool,
    {
        while let Some(c) = self.peek() {
            if pred(c) {
                self.advance();
            } else {
                break;
            }
        }
    }
}

fn is_ident_head(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_tail(c: char) -> bool {
    is_ident_head(c) || c.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use crate::token::{Token, TokenKind};
    use pretty_assertions::assert_eq;

    use super::Scanner;

    fn kinds(v: Vec<Token>) -> Vec<TokenKind> {
        v.iter().map(|t| t.kind.clone()).collect()
    }

    fn expect(src: &'static str, res: Vec<TokenKind>) {
        let mut sc = Scanner::new(src);
        let mut tokens = Vec::new();

        loop {
            match sc.scan_token().unwrap() {
                Some(t) => tokens.push(t),
                None => break,
            }
        }

        assert_eq!(kinds(tokens), res);
    }

    #[test]
    fn scan() {
        use TokenKind::*;

        expect(
            "( ) { } , . - + ; / * ! != = == > >= < <= asd lasd \"bajs\" 1 1.2 and class else false for fun if nil or print return super this true var while",
            vec![
                LParen,
                RParen,
                LBrace,
                RBrace,
                Comma,
                Dot,
                Minus,
                Plus,
                Semicolon,
                Slash,
                Star,
                Bang,
                BangEqual,
                Equal,
                EqualEqual,
                Greater,
                GreaterEqual,
                Less,
                LessEqual,
                Identifier,
                Identifier,
                String,
                Number,
                Number,
                And,
                Class,
                Else,
                False,
                For,
                Fun,
                If,
                Nil,
                Or,
                Print,
                Return,
                Super,
                This,
                True,
                Var,
                While
            ]
        );
    }
}
