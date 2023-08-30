use std::{collections::HashMap, rc::Rc};

use crate::{
    chunk::Chunk,
    compiler::{self, Compiler},
    op::Op,
    value::{HeapValue, Value},
};

pub struct VM {
    chunk: Chunk,
    ii: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

#[derive(Debug)]
pub enum Error {
    Compiler(compiler::Error),
    Runtime { line: u32, kind: RuntimeError },
}

#[derive(Debug)]
pub enum RuntimeError {
    Decode(usize),
    Binary { lhs: Value, rhs: Value, op: Op },
    Unary { v: Value, op: Op },
    Undefined { i: String },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Compiler(e) => write!(f, "{}", e),
            Error::Runtime { line, kind } => write!(f, "[line {}] {}", line, kind),
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Decode(s) => write!(f, "{}", s),
            RuntimeError::Binary { lhs, rhs, op } => write!(
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
            RuntimeError::Unary { v, op } => write!(
                f,
                "Cannot {} value of type {}",
                match op {
                    Op::Negate => "negate",
                    Op::Not => "invert",
                    _ => unreachable!(),
                },
                v.type_desc()
            ),

            RuntimeError::Undefined { i } => write!(f, "No such variable '{}'", i),
        }
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            ii: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
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
            Err(_) => self.make_runtime_error(RuntimeError::Decode(self.ii)),
        }
    }

    fn make_runtime_error<T>(&self, kind: RuntimeError) -> Result<T, Error> {
        Err(Error::Runtime {
            line: self.chunk.get_line(self.ii),
            kind,
        })
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
                        v => self.make_runtime_error(RuntimeError::Unary { v, op })?,
                    }
                }
                Op::Add => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (&lhs, &rhs) {
                        (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
                        (Value::HeapValue(lhs), Value::HeapValue(rhs)) => {
                            match (&*lhs.as_ref(), &*rhs.as_ref()) {
                                (HeapValue::String(l), HeapValue::String(r)) => {
                                    Value::HeapValue(Rc::new(HeapValue::String(l.to_owned() + r)))
                                }
                            }
                        }
                        _ => self.make_runtime_error(RuntimeError::Binary { lhs, rhs, op })?,
                    };
                    self.push(res);
                }
                Op::Subtract => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (&lhs, &rhs) {
                        (Value::Number(l), Value::Number(r)) => Value::Number(l - r),
                        _ => self.make_runtime_error(RuntimeError::Binary { lhs, rhs, op })?,
                    };
                    self.push(res);
                }
                Op::Multiply => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (&lhs, &rhs) {
                        (Value::Number(l), Value::Number(r)) => Value::Number(l * r),
                        _ => self.make_runtime_error(RuntimeError::Binary { lhs, rhs, op })?,
                    };
                    self.push(res);
                }
                Op::Divide => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (&lhs, &rhs) {
                        (Value::Number(l), Value::Number(r)) => Value::Number(l / r),
                        _ => self.make_runtime_error(RuntimeError::Binary { lhs, rhs, op })?,
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
                        None => self.make_runtime_error(RuntimeError::Unary { v, op })?,
                    }
                }
                Op::Equal => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    let res = match (lhs, rhs) {
                        (Value::Number(l), Value::Number(r)) => l == r,
                        (Value::Bool(l), Value::Bool(r)) => l == r,
                        (Value::Nil, Value::Nil) => true,
                        (Value::HeapValue(lhs), Value::HeapValue(rhs)) => match (&*lhs, &*rhs) {
                            (HeapValue::String(l), HeapValue::String(r)) => l == r,
                        },
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
                Op::Print => {
                    println!("{}", self.pop());
                }
                Op::Pop => {
                    self.pop();
                }
                Op::DefineGlobal(offset) => {
                    let v = self.chunk.read_constant(offset).unwrap().clone();
                    let s = match &v {
                        Value::HeapValue(h) => match &*h.as_ref() {
                            HeapValue::String(s) => s,
                        },
                        _ => unreachable!(),
                    };
                    let v = self.pop();
                    self.globals.insert(s.to_string(), v);
                }
                Op::GetGlobal(offset) => {
                    let v = self.chunk.read_constant(offset).unwrap().clone();
                    let s = match &v {
                        Value::HeapValue(h) => match &*h.as_ref() {
                            HeapValue::String(s) => s,
                        },
                        _ => unreachable!(),
                    };
                    if let Some(v) = self.globals.get(s) {
                        self.push(v.clone());
                    } else {
                        self.make_runtime_error(RuntimeError::Undefined { i: s.to_string() })?;
                    }
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

    pub fn interpret_str(&mut self, source: &str, repl: bool) -> Result<(), Error> {
        let chunk = Compiler::compile(source).map_err(|e| Error::Compiler(e))?;
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
