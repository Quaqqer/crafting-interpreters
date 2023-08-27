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
        }
    }

    pub fn add_op(&mut self, op: Op, line: i32) {
        match op {
            Op::Return => self.add_code(u8::from(Opcode::Return), line),
            Op::Constant(offset) => {
                self.add_code(u8::from(Opcode::Constant), line);
                self.add_code(offset, line);
            }
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
                Op::Constant(offset) => {
                    write!(f, "CONSTANT {} ({})", offset, self.constants[offset as usize])?
                }
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
