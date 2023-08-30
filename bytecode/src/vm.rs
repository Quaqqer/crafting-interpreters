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
                        Value::Float(f) => self.push(Value::Float(-f)),
                    }
                }
                Op::Add => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
                    };
                    self.push(res);
                }
                Op::Subtract => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
                    };
                    self.push(res);
                }
                Op::Multiply => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
                    };
                    self.push(res);
                }
                Op::Divide => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                    };
                    self.push(res);
                }
            }
        }

        Ok(())
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
            vec![Value::Float(1.)],
            vec![Op::Constant(0)],
            vec![Value::Float(0.)],
        )
    }

    #[test]
    fn basic_ops() {
        test_vm(
            vec![Value::Float(0.)],
            vec![Op::Constant(0)],
            vec![Value::Float(0.)],
        );

        test_vm(
            vec![Value::Float(1.)],
            vec![Op::Constant(0), Op::Negate],
            vec![Value::Float(-1.)],
        );

        test_vm(
            vec![Value::Float(1.)],
            vec![Op::Constant(0), Op::Constant(0), Op::Add],
            vec![Value::Float(2.)],
        );

        test_vm(
            vec![Value::Float(1.)],
            vec![Op::Constant(0), Op::Constant(0), Op::Subtract],
            vec![Value::Float(0.)],
        );

        test_vm(
            vec![Value::Float(2.)],
            vec![Op::Constant(0), Op::Constant(0), Op::Multiply],
            vec![Value::Float(4.)],
        );

        test_vm(
            vec![Value::Float(2.)],
            vec![Op::Constant(0), Op::Constant(0), Op::Divide],
            vec![Value::Float(1.)],
        );
    }
}
