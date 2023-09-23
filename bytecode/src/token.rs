//! Lox tokens

#[derive(Debug, Clone)]
/// A lox token
pub struct Token {
    /// The kind of token
    pub kind: TokenKind,
    /// The source text of the token
    pub source: String,
    /// The source line of the token
    pub line: u32,
}

#[derive(Debug, PartialEq, Clone)]
/// A kind of token
pub enum TokenKind {
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `;`
    Semicolon,
    /// `/`
    Slash,
    /// `*`
    Star,
    /// `!`
    Bang,
    /// `!=`
    BangEqual,
    /// `=`
    Equal,
    /// `==`
    EqualEqual,
    /// `>`
    Greater,
    /// `>=`
    GreaterEqual,
    /// `<`
    Less,
    /// `<=`
    LessEqual,

    // Literals
    /// An identifier (variable names)
    ///
    /// Example: `x`
    Identifier,
    /// A string token
    ///
    /// Example: `"i am a string"`
    String,
    /// A number
    ///
    /// Example: `123.3`
    Number,

    // Keywords
    /// `and`
    And,
    /// `class`
    Class,
    /// `else`
    Else,
    /// `false`
    False,
    /// `for`
    For,
    /// `fun`
    Fun,
    /// `if`
    If,
    /// `nil`
    Nil,
    /// `or`
    Or,
    /// `print`
    Print,
    /// `return`
    Return,
    /// `super`
    Super,
    /// `this`
    This,
    /// `true`
    True,
    /// `var`
    Var,
    /// `while`
    While,

    /// The end of file token
    EOF,
    /// An unknown character
    UnknownChar,
    /// An unterminated string token
    ///
    /// Example: `"hello wo<EOF>`
    UnterminatedString,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LParen => write!(f, "'('"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::LBrace => write!(f, "'{{'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::Dot => write!(f, "'.'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Semicolon => write!(f, "';'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Bang => write!(f, "'!'"),
            TokenKind::BangEqual => write!(f, "'!='"),
            TokenKind::Equal => write!(f, "'='"),
            TokenKind::EqualEqual => write!(f, "'=='"),
            TokenKind::Greater => write!(f, "'>'"),
            TokenKind::GreaterEqual => write!(f, "'>='"),
            TokenKind::Less => write!(f, "'<'"),
            TokenKind::LessEqual => write!(f, "'<='"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Number => write!(f, "number"),
            TokenKind::And => write!(f, "'and'"),
            TokenKind::Class => write!(f, "'class'"),
            TokenKind::Else => write!(f, "'else'"),
            TokenKind::False => write!(f, "'false'"),
            TokenKind::For => write!(f, "'for'"),
            TokenKind::Fun => write!(f, "'fun'"),
            TokenKind::If => write!(f, "'if'"),
            TokenKind::Nil => write!(f, "'nil'"),
            TokenKind::Or => write!(f, "'or'"),
            TokenKind::Print => write!(f, "'print'"),
            TokenKind::Return => write!(f, "'return'"),
            TokenKind::Super => write!(f, "'super'"),
            TokenKind::This => write!(f, "'this'"),
            TokenKind::True => write!(f, "'true'"),
            TokenKind::Var => write!(f, "'var'"),
            TokenKind::While => write!(f, "'while'"),
            TokenKind::EOF => write!(f, "eof"),
            TokenKind::UnknownChar | TokenKind::UnterminatedString => unreachable!(),
        }
    }
}
