use crate::{
    op::{Op, Opcode},
    value::Value,
};

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<i32>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn decode_instruction(&self, offset: usize) -> Result<(Op, usize), ()> {
        let opcode = Opcode::from(self.code[offset]);

        match opcode {
            Opcode::Return => Ok((Op::Return, 1)),
            Opcode::Constant => {
                let offset = self.code[offset + 1];
                Ok((Op::Constant(offset), 2))
            }
            Opcode::Negate => Ok((Op::Negate, 1)),
            Opcode::Add => Ok((Op::Add, 1)),
            Opcode::Subtract => Ok((Op::Subtract, 1)),
            Opcode::Multiply => Ok((Op::Multiply, 1)),
            Opcode::Divide => Ok((Op::Divide, 1)),
        }
    }

    pub fn add_op(&mut self, op: Op, line: i32) {
        match op {
            Op::Return => self.add_code(u8::from(Opcode::Return), line),
            Op::Constant(offset) => {
                self.add_code(u8::from(Opcode::Constant), line);
                self.add_code(offset, line);
            }
            Op::Negate => self.add_code(u8::from(Opcode::Negate), line),
            Op::Add => self.add_code(u8::from(Opcode::Add), line),
            Op::Subtract => self.add_code(u8::from(Opcode::Subtract), line),
            Op::Multiply => self.add_code(u8::from(Opcode::Multiply), line),
            Op::Divide => self.add_code(u8::from(Opcode::Divide), line),
        }
    }

    pub fn add_code(&mut self, v: u8, line: i32) {
        self.code.push(v);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, v: Value) -> u8 {
        let i = self.constants.len();
        self.constants.push(v);
        i as u8
    }

    pub fn read_constant(&mut self, offset: u8) -> Option<&Value> {
        self.constants.get(offset as usize)
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }
}

impl std::fmt::Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut offset = 0;

        while offset < self.code.len() {
            let (op, len) = self.decode_instruction(offset).unwrap();

            write!(f, "{:04} ", offset)?;

            if offset > 0 && self.lines[offset - 1] == self.lines[offset] {
                write!(f, "   | ")?;
            } else {
                write!(f, "{:>4} ", self.lines[offset])?;
            }

            match op {
                Op::Constant(offset) => write!(
                    f,
                    "CONSTANT {} ({})",
                    offset, self.constants[offset as usize]
                )?,
                _ => write!(f, "{}", op)?,
            };

            offset += len;

            if offset != self.code.len() {
                write!(f, "\n")?;
            }
        }

        Ok(())
    }
}
