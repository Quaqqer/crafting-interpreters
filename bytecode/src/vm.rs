use crate::{chunk::Chunk, op::Op, value::Value};

pub struct VM {
    chunk: Chunk,
    ii: usize,
    stack: Vec<Value>,
}

#[derive(Debug)]
pub enum Error {
    DecodeError,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ii: 0,
            stack: Vec::new(),
        }
    }

    fn fetch(&mut self) -> Result<Option<Op>, Error> {
        if self.ii >= self.chunk.len() {
            return Ok(None);
        }

        match self.chunk.decode_instruction(self.ii) {
            Ok((op, d)) => {
                self.ii += d;
                Ok(Some(op))
            }
            Err(_) => Err(Error::DecodeError),
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
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
                    let res = match (rhs, lhs) {
                        (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
                    };
                    self.push(res);
                }
                Op::Subtract => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (rhs, lhs) {
                        (Value::Float(l), Value::Float(r)) => Value::Float(l - r),
                    };
                    self.push(res);
                }
                Op::Multiply => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (rhs, lhs) {
                        (Value::Float(l), Value::Float(r)) => Value::Float(l * r),
                    };
                    self.push(res);
                }
                Op::Divide => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (rhs, lhs) {
                        (Value::Float(l), Value::Float(r)) => Value::Float(l / r),
                    };
                    self.push(res);
                }
            }
        }

        Ok(())
    }

    fn push(&mut self, v: Value) {
        self.stack.push(v)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
}
