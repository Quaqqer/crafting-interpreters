//! Lox bytecode chunks

use crate::{
    op::{Op, Opcode},
    value::Value,
};

/// A lox bytecode chunk
///
/// The lox bytecode contains the bytecode that the lox vm runs. The chunk also contains a vector
/// of constants since we don't inline values in the bytecode, but rather reference values in the
/// constants vector. It also contains the line in the source code that the instructions came from.
///
/// * `code`: The code in the chunk
/// * `constants`: The constant values referenced in the code vector
/// * `lines`: The lines referenced for each code byte
#[derive(Debug)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<u32>,
}

impl Chunk {
    /// Create a new chunk
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    /// Decode an operation from an offset
    ///
    /// If the offset is invalid or contains an invalid operation this will panic.
    pub fn decode_op(&self, offset: usize) -> Result<(Op, usize), ()> {
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
            Opcode::True => Ok((Op::True, 1)),
            Opcode::False => Ok((Op::False, 1)),
            Opcode::Nil => Ok((Op::Nil, 1)),
            Opcode::Not => Ok((Op::Not, 1)),
            Opcode::Equal => Ok((Op::Equal, 1)),
            Opcode::Greater => Ok((Op::Greater, 1)),
            Opcode::Less => Ok((Op::Less, 1)),
            Opcode::Print => Ok((Op::Print, 1)),
            Opcode::Pop => Ok((Op::Pop, 1)),
            Opcode::DefineGlobal => {
                let g = self.code[offset + 1];
                Ok((Op::DefineGlobal(g), 2))
            }
            Opcode::GetGlobal => {
                let g = self.code[offset + 1];
                Ok((Op::GetGlobal(g), 2))
            }
            Opcode::SetGlobal => {
                let g = self.code[offset + 1];
                Ok((Op::SetGlobal(g), 2))
            }
            Opcode::GetLocal => {
                let l = self.code[offset + 1];
                Ok((Op::GetLocal(l), 2))
            }
            Opcode::SetLocal => {
                let l = self.code[offset + 1];
                Ok((Op::SetLocal(l), 2))
            }
            Opcode::JumpIfFalse => {
                let a = u16::from_le_bytes(self.code[offset + 1..offset + 3].try_into().unwrap());
                Ok((Op::JumpIfFalse(a), 3))
            }
            Opcode::Jump => {
                let a = u16::from_le_bytes(self.code[offset + 1..offset + 3].try_into().unwrap());
                Ok((Op::Jump(a), 3))
            }
            Opcode::Loop => {
                let a = u16::from_le_bytes(self.code[offset + 1..offset + 3].try_into().unwrap());
                Ok((Op::Loop(a), 3))
            }
        }
    }

    /// Push an operation to the chunk.
    pub fn push_op(&mut self, op: Op, line: u32) {
        match op {
            Op::Return => self.push_opcode(Opcode::Return, line),
            Op::Constant(offset) => {
                self.push_opcode(Opcode::Constant, line);
                self.push(offset, line);
            }
            Op::Negate => self.push_opcode(Opcode::Negate, line),
            Op::Add => self.push_opcode(Opcode::Add, line),
            Op::Subtract => self.push_opcode(Opcode::Subtract, line),
            Op::Multiply => self.push_opcode(Opcode::Multiply, line),
            Op::Divide => self.push_opcode(Opcode::Divide, line),
            Op::True => self.push_opcode(Opcode::True, line),
            Op::False => self.push_opcode(Opcode::False, line),
            Op::Nil => self.push_opcode(Opcode::Nil, line),
            Op::Not => self.push_opcode(Opcode::Not, line),
            Op::Equal => self.push_opcode(Opcode::Equal, line),
            Op::Greater => self.push_opcode(Opcode::Greater, line),
            Op::Less => self.push_opcode(Opcode::Less, line),
            Op::Print => self.push_opcode(Opcode::Print, line),
            Op::Pop => self.push_opcode(Opcode::Pop, line),
            Op::DefineGlobal(g) => {
                self.push_opcode(Opcode::DefineGlobal, line);
                self.push(g, line);
            }
            Op::GetGlobal(g) => {
                self.push_opcode(Opcode::GetGlobal, line);
                self.push(g, line);
            }
            Op::SetGlobal(g) => {
                self.push_opcode(Opcode::SetGlobal, line);
                self.push(g, line);
            }
            Op::GetLocal(l) => {
                self.push_opcode(Opcode::GetLocal, line);
                self.push(l, line);
            }
            Op::SetLocal(l) => {
                self.push_opcode(Opcode::SetLocal, line);
                self.push(l, line);
            }
            Op::JumpIfFalse(a) => {
                self.push_opcode(Opcode::JumpIfFalse, line);
                let [l, r] = a.to_le_bytes();
                self.push(l, line);
                self.push(r, line);
            }
            Op::Jump(a) => {
                self.push_opcode(Opcode::Jump, line);
                let [l, r] = a.to_le_bytes();
                self.push(l, line);
                self.push(r, line);
            }
            Op::Loop(a) => {
                self.push_opcode(Opcode::Jump, line);
                let [l, r] = a.to_le_bytes();
                self.push(l, line);
                self.push(r, line);
            }
        }
    }

    /// Push a byte to the chunk
    pub fn push(&mut self, byte: u8, line: u32) {
        self.code.push(byte);
        self.lines.push(line);
    }

    /// Push an opcode to the chunk
    fn push_opcode(&mut self, opcode: Opcode, line: u32) {
        self.push(u8::from(opcode), line);
    }

    /// Push a constant to the chunk, returns the constant offset of the value
    pub fn push_constant(&mut self, v: Value) -> u8 {
        let i = self.constants.len();
        self.constants.push(v);
        i as u8
    }

    /// Get a constant from the chunk
    ///
    /// * `offset`: The constant offset, returned from `push_constant`
    pub fn get_constant(&mut self, offset: u8) -> Option<&Value> {
        self.constants.get(offset as usize)
    }

    /// Get the current offset of the code
    ///
    /// Often used for getting the current offset for patching jumps
    pub fn get_offset(&self) -> usize {
        self.code.len()
    }

    /// Get the source line of a code byte
    pub fn get_line(&self, offset: usize) -> u32 {
        self.lines[offset]
    }

    /// Set a byte in the chunk
    pub fn set_byte(&mut self, offset: usize, byte: u8) {
        self.code[offset] = byte;
    }
}

impl std::fmt::Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut offset = 0;

        while offset < self.code.len() {
            let (op, len) = self.decode_op(offset).unwrap();

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
                Op::DefineGlobal(g) => {
                    write!(f, "DEFINE_GLOBAL {} ({})", g, self.constants[g as usize])?
                }
                Op::GetGlobal(g) => write!(f, "GET_GLOBAL {} ({})", g, self.constants[g as usize])?,
                Op::Jump(j) => write!(f, "JUMP {} ({})", j, offset + len + j as usize)?,
                Op::JumpIfFalse(j) => {
                    write!(f, "JUMP_IF_FALSE {} ({})", j, offset + len + j as usize)?
                }
                Op::Loop(j) => write!(f, "LOOP {} ({})", j, offset + len - j as usize)?,
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

#[cfg(test)]
mod tests {
    use crate::{op::Op, value::Value};
    use pretty_assertions::assert_eq;

    use super::Chunk;

    #[test]
    fn print() {
        let mut c = Chunk::new();
        c.push_constant(Value::Number(0.));
        c.push_op(Op::Return, 0);
        c.push_op(Op::Constant(0), 0);
        c.push_op(Op::Negate, 0);
        c.push_op(Op::Add, 0);
        c.push_op(Op::Subtract, 0);
        c.push_op(Op::Multiply, 0);
        c.push_op(Op::Divide, 0);

        assert_eq!(
            format!("{}", c),
            "0000    0 RETURN
0001    | CONSTANT 0 (0)
0003    | NEGATE
0004    | ADD
0005    | SUBTRACT
0006    | MULTIPLY
0007    | DIVIDE"
        );
    }
}
