use crate::{
    op::{Op, Opcode},
    value::Value,
};

#[derive(Debug)]
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
        }
    }

    pub fn decode_ops(&self) -> Result<Vec<Op>, ()> {
        let mut offset = 0;
        let mut ops = Vec::new();
        while offset < self.code.len() {
            let (op, len) = self.decode_op(offset)?;
            offset += len;
            ops.push(op);
        }

        Ok(ops)
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
        let ops = self.decode_ops();
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
    fn encode_decode() {
        let mut c = Chunk::new();

        c.add_constant(Value::Float(0.));
        assert_eq!(c.constants, vec![Value::Float(0.)]);

        c.add_op(Op::Return, 0);
        c.add_op(Op::Constant(0), 0);
        c.add_op(Op::Negate, 0);
        c.add_op(Op::Add, 0);
        c.add_op(Op::Subtract, 0);
        c.add_op(Op::Multiply, 0);
        c.add_op(Op::Divide, 0);

        let ops = c.decode_ops().unwrap();
        assert_eq!(
            ops,
            vec![
                Op::Return,
                Op::Constant(0),
                Op::Negate,
                Op::Add,
                Op::Subtract,
                Op::Multiply,
                Op::Divide
            ]
        );
    }

    #[test]
    fn print() {
        let mut c = Chunk::new();
        c.add_constant(Value::Float(0.));
        c.add_op(Op::Return, 0);
        c.add_op(Op::Constant(0), 0);
        c.add_op(Op::Negate, 0);
        c.add_op(Op::Add, 0);
        c.add_op(Op::Subtract, 0);
        c.add_op(Op::Multiply, 0);
        c.add_op(Op::Divide, 0);

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
