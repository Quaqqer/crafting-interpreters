pub enum Opcode {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
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
            _ => panic!("No such opcode {}", value),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Return,
    Constant(u8),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
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
        }
    }
}
