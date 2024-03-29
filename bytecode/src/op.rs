//! Opcodes and full operations for the lox language

#[allow(missing_docs)]
/// An opcode
pub enum Opcode {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    True,
    False,
    Nil,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        match value {
            Opcode::Return => 0,
            Opcode::Constant => 1,
            Opcode::Negate => 2,
            Opcode::Add => 3,
            Opcode::Subtract => 4,
            Opcode::Multiply => 5,
            Opcode::Divide => 6,
            Opcode::True => 7,
            Opcode::False => 8,
            Opcode::Nil => 9,
            Opcode::Not => 10,
            Opcode::Equal => 11,
            Opcode::Greater => 12,
            Opcode::Less => 13,
            Opcode::Print => 14,
            Opcode::Pop => 15,
            Opcode::DefineGlobal => 16,
            Opcode::GetGlobal => 17,
            Opcode::SetGlobal => 18,
            Opcode::GetLocal => 19,
            Opcode::SetLocal => 20,
            Opcode::JumpIfFalse => 21,
            Opcode::Jump => 22,
            Opcode::Loop => 23,
            Opcode::Call => 24,
        }
    }
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        match value {
            0 => Opcode::Return,
            1 => Opcode::Constant,
            2 => Opcode::Negate,
            3 => Opcode::Add,
            4 => Opcode::Subtract,
            5 => Opcode::Multiply,
            6 => Opcode::Divide,
            7 => Opcode::True,
            8 => Opcode::False,
            9 => Opcode::Nil,
            10 => Opcode::Not,
            11 => Opcode::Equal,
            12 => Opcode::Greater,
            13 => Opcode::Less,
            14 => Opcode::Print,
            15 => Opcode::Pop,
            16 => Opcode::DefineGlobal,
            17 => Opcode::GetGlobal,
            18 => Opcode::SetGlobal,
            19 => Opcode::GetLocal,
            20 => Opcode::SetLocal,
            21 => Opcode::JumpIfFalse,
            22 => Opcode::Jump,
            23 => Opcode::Loop,
            24 => Opcode::Call,
            _ => panic!("No such opcode {}", value),
        }
    }
}

#[allow(missing_docs)]
#[derive(Debug, PartialEq, Clone)]
/// A full lox vm operation, opcode with arguments
pub enum Op {
    Return,
    Constant(u8),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    True,
    False,
    Nil,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal(u8),
    GetGlobal(u8),
    SetGlobal(u8),
    GetLocal(u8),
    SetLocal(u8),
    JumpIfFalse(u16),
    Jump(u16),
    Loop(u16),
    Call(u8),
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Return => write!(f, "RETURN"),
            Op::Constant(offset) => write!(f, "CONSTANT {}", offset),
            Op::Negate => write!(f, "NEGATE"),
            Op::Add => write!(f, "ADD"),
            Op::Subtract => write!(f, "SUBTRACT"),
            Op::Multiply => write!(f, "MULTIPLY"),
            Op::Divide => write!(f, "DIVIDE"),
            Op::True => write!(f, "TRUE"),
            Op::False => write!(f, "FALSE"),
            Op::Nil => write!(f, "NIL"),
            Op::Not => write!(f, "NOT"),
            Op::Equal => write!(f, "EQUAL"),
            Op::Greater => write!(f, "GREATER"),
            Op::Less => write!(f, "LESS"),
            Op::Print => write!(f, "PRINT"),
            Op::Pop => write!(f, "POP"),
            Op::DefineGlobal(g) => write!(f, "DEFINE_GLOBAL {}", g),
            Op::GetGlobal(g) => write!(f, "GET_GLOBAL {}", g),
            Op::SetGlobal(g) => write!(f, "SET_GLOBAL {}", g),
            Op::GetLocal(l) => write!(f, "GET_LOCAL {}", l),
            Op::SetLocal(l) => write!(f, "SET_LOCAL {}", l),
            Op::JumpIfFalse(a) => write!(f, "JUMP_IF_FALSE {}", a),
            Op::Jump(a) => write!(f, "JUMP {}", a),
            Op::Loop(a) => write!(f, "LOOP {}", a),
            Op::Call(argc) => write!(f, "CALL {}", argc),
        }
    }
}
