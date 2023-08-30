use crate::{
    chunk::Chunk,
    compiler::{self, Compiler},
    op::Op,
    value::Value,
};

pub struct VM {
    chunk: Chunk,
    ii: usize,
    stack: Vec<Value>,
}

#[derive(Debug)]
pub enum Error {
    CompileError(compiler::Error),
    DecodeError(String),
    BinaryError { lhs: Value, rhs: Value, op: Op },
    UnaryError { v: Value, op: Op },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::CompileError(e) => write!(f, "{}", e),
            Error::DecodeError(s) => write!(f, "{}", s),
            Error::BinaryError { lhs, rhs, op } => write!(
                f,
                "Cannot {} values of types {} and {}",
                match op {
                    Op::Add => "add",
                    Op::Subtract => "subtract",
                    Op::Multiply => "multiply",
                    Op::Divide => "divide",
                    _ => unreachable!(),
                },
                lhs.type_desc(),
                rhs.type_desc()
            ),
            Error::UnaryError { v, op } => {
                write!(
                    f,
                    "Cannot {} value of type {}",
                    match op {
                        Op::Negate => "negate",
                        Op::Not => "invert",
                        _ => unreachable!(),
                    },
                    v.type_desc()
                )
            }
        }
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            ii: 0,
            stack: Vec::new(),
        }
    }

    fn fetch(&mut self) -> Result<Option<Op>, Error> {
        if self.ii >= self.chunk.len() {
            return Ok(None);
        }

        match self.chunk.decode_op(self.ii) {
            Ok((op, d)) => {
                self.ii += d;
                Ok(Some(op))
            }
            Err(_) => Err(Error::DecodeError(format!(
                "Could not decode op at index {}",
                self.ii,
            ))),
        }
    }

    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), Error> {
        self.chunk = chunk;
        self.ii = 0;

        while let Some(op) = self.fetch()? {
            #[cfg(feature = "debug_trace")]
            {
                println!("{:?}", self.stack);
                println!("{}", op);
            }

            match op {
                Op::Return => {
                    println!("{}", self.pop());
                }
                Op::Constant(offset) => {
                    let v = self.chunk.read_constant(offset).unwrap().clone();
                    self.push(v);
                }
                Op::Negate => {
                    let v = self.pop();
                    match v {
                        Value::Number(f) => self.push(Value::Number(-f)),
                        v => Err(Error::UnaryError { v, op })?,
                    }
                }
                Op::Add => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
                        _ => Err(Error::BinaryError { lhs, rhs, op })?,
                    };
                    self.push(res);
                }
                Op::Subtract => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Number(l), Value::Number(r)) => Value::Number(l - r),
                        _ => Err(Error::BinaryError { lhs, rhs, op })?,
                    };
                    self.push(res);
                }
                Op::Multiply => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Number(l), Value::Number(r)) => Value::Number(l * r),
                        _ => Err(Error::BinaryError { lhs, rhs, op })?,
                    };
                    self.push(res);
                }
                Op::Divide => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Number(l), Value::Number(r)) => Value::Number(l / r),
                        _ => Err(Error::BinaryError { lhs, rhs, op })?,
                    };
                    self.push(res);
                }
                Op::True => self.push(Value::Bool(true)),
                Op::False => self.push(Value::Bool(false)),
                Op::Nil => self.push(Value::Nil),
                Op::Not => {
                    let v = self.pop();
                    match VM::is_falsey(&v) {
                        Some(v) => self.push(Value::Bool(v)),
                        None => Err(Error::UnaryError { v, op })?,
                    }
                }
                Op::Equal => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Number(l), Value::Number(r)) => l == r,
                        (Value::Bool(l), Value::Bool(r)) => l == r,
                        (Value::Nil, Value::Nil) => true,
                        _ => false,
                    };
                    self.push(Value::Bool(res));
                }
                Op::Greater => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Number(l), Value::Number(r)) => l > r,
                        _ => false,
                    };
                    self.push(Value::Bool(res));
                }
                Op::Less => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Number(l), Value::Number(r)) => l < r,
                        _ => false,
                    };
                    self.push(Value::Bool(res));
                }
            }
        }

        Ok(())
    }

    fn is_falsey(v: &Value) -> Option<bool> {
        match v {
            Value::Nil => Some(true),
            Value::Bool(b) => Some(!b),
            _ => None,
        }
    }

    pub fn interpret_str(&mut self, source: &str) -> Result<(), Error> {
        let chunk = Compiler::compile(source).map_err(|e| Error::CompileError(e))?;
        self.interpret(chunk)?;
        Ok(())
    }

    fn push(&mut self, v: Value) {
        self.stack.push(v)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::{chunk::Chunk, op::Op, value::Value, vm::VM};
    use pretty_assertions::assert_eq;

    fn create_chunk(constants: Vec<Value>, ops: Vec<Op>) -> Chunk {
        let mut chunk = Chunk::new();
        for c in constants {
            chunk.add_constant(c);
        }
        for op in ops {
            chunk.add_op(op, 0);
        }
        chunk
    }

    fn test_vm(constants: Vec<Value>, ops: Vec<Op>, stack: Vec<Value>) {
        let chunk = create_chunk(constants, ops);
        let mut vm = VM::new();
        vm.interpret(chunk).unwrap();
        assert_eq!(vm.stack, stack);
    }

    #[test]
    #[should_panic]
    fn wrong_res() {
        test_vm(
            vec![Value::Number(1.)],
            vec![Op::Constant(0)],
            vec![Value::Number(0.)],
        )
    }

    #[test]
    fn basic_ops() {
        test_vm(
            vec![Value::Number(0.)],
            vec![Op::Constant(0)],
            vec![Value::Number(0.)],
        );

        test_vm(
            vec![Value::Number(1.)],
            vec![Op::Constant(0), Op::Negate],
            vec![Value::Number(-1.)],
        );

        test_vm(
            vec![Value::Number(1.)],
            vec![Op::Constant(0), Op::Constant(0), Op::Add],
            vec![Value::Number(2.)],
        );

        test_vm(
            vec![Value::Number(1.)],
            vec![Op::Constant(0), Op::Constant(0), Op::Subtract],
            vec![Value::Number(0.)],
        );

        test_vm(
            vec![Value::Number(2.)],
            vec![Op::Constant(0), Op::Constant(0), Op::Multiply],
            vec![Value::Number(4.)],
        );

        test_vm(
            vec![Value::Number(2.)],
            vec![Op::Constant(0), Op::Constant(0), Op::Divide],
            vec![Value::Number(1.)],
        );
    }
}
