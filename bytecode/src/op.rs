pub enum Opcode {
    Return,
    Constant,
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        match value {
            Opcode::Return => 0,
            Opcode::Constant => 1,
        }
    }
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        match value {
            0 => Opcode::Return,
            1 => Opcode::Constant,
            _ => panic!("No such opcode {}", value),
        }
    }
}

pub enum Op {
    Return,
    Constant(u8),
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Return => write!(f, "RETURN"),
            Self::Constant(offset) => write!(f, "CONSTANT {}", offset),
        }
    }
}
