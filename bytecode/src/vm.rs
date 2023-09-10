use std::{collections::HashMap, rc::Rc};

use crate::{
    chunk::Chunk,
    compiler::{self, Compiler},
    op::Op,
    value::{HeapValue, Value},
};

pub trait VmIO {
    fn write(&mut self, s: &str);
}

pub struct DefaultVMIO {}

impl DefaultVMIO {
    pub fn new() -> Self {
        Self {}
    }
}

impl VmIO for DefaultVMIO {
    fn write(&mut self, s: &str) {
        println!("{}", s);
    }
}

pub struct VM<'a, IO: VmIO = DefaultVMIO> {
    chunk: Chunk,
    ii: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    io: &'a mut IO,
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
    NonBool { v: Value },
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
            RuntimeError::NonBool { v } => write!(
                f,
                "Cannot cast value of type {} into a boolean",
                v.type_desc()
            ),
        }
    }
}

impl<'a, IO: VmIO> VM<'a, IO> {
    pub fn new(io: &'a mut IO) -> Self {
        Self {
            chunk: Chunk::new(),
            ii: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
            io,
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

    fn make_runtime_error<U>(&self, kind: RuntimeError) -> Result<U, Error> {
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
                    let s = format!("{}", self.pop());
                    self.io.write(s.as_str());
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
                    let falsey = self.is_falsey(&v)?;
                    self.push(Value::Bool(falsey));
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
                    let s = format!("{}", self.pop());
                    self.io.write(s.as_str());
                }
                Op::Pop => {
                    self.pop();
                }
                Op::DefineGlobal(c) => {
                    let v = self.chunk.read_constant(c).unwrap().clone();
                    let s = match &v {
                        Value::HeapValue(h) => match &*h.as_ref() {
                            HeapValue::String(s) => s,
                        },
                        _ => unreachable!(),
                    };
                    let v = self.pop();
                    self.globals.insert(s.to_string(), v);
                }
                Op::GetGlobal(c) => {
                    let v = self.chunk.read_constant(c).unwrap().clone();
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
                Op::SetGlobal(c) => {
                    let v = self.chunk.read_constant(c).unwrap().clone();
                    let s = match &v {
                        Value::HeapValue(h) => match &*h.as_ref() {
                            HeapValue::String(s) => s,
                        },
                        _ => unreachable!(),
                    };
                    let val = self.peek();
                    if let Some(v) = self.globals.get_mut(s) {
                        *v = val;
                    } else {
                        self.make_runtime_error(RuntimeError::Undefined { i: s.to_string() })?;
                    }
                }
                Op::GetLocal(l) => {
                    self.push(self.stack[l as usize].clone());
                }
                Op::SetLocal(l) => {
                    self.stack[l as usize] = self.peek();
                }
                Op::JumpIfFalse(addr) => {
                    let v = self.peek();
                    if self.is_falsey(&v)? {
                        self.ii += addr as usize;
                    }
                }
                Op::Jump(addr) => {
                    self.ii += addr as usize;
                }
            }
        }

        Ok(())
    }

    fn is_falsey(&mut self, v: &Value) -> Result<bool, Error> {
        match v {
            Value::Nil => Ok(true),
            Value::Bool(b) => Ok(!b),
            _ => self.make_runtime_error(RuntimeError::NonBool { v: v.clone() }),
        }
    }

    pub fn interpret_str(&mut self, source: &str) -> Result<(), Error> {
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

    fn peek(&self) -> Value {
        self.stack.last().unwrap().clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        chunk::Chunk,
        op::Op,
        value::Value,
        vm::{DefaultVMIO, VM},
    };
    use pretty_assertions::assert_eq;

    use super::VmIO;

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
        let mut io = DefaultVMIO::new();
        let mut vm = VM::new(&mut io);
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

    struct MockedIO {
        out: String,
    }

    impl VmIO for MockedIO {
        fn write(&mut self, s: &str) {
            self.out += s;
            self.out += "\n";
        }
    }

    impl MockedIO {
        fn new() -> Self {
            Self { out: String::new() }
        }
    }

    fn check_out(src: &str, expected: &str) {
        let mut io = MockedIO::new();
        let mut vm = VM::new(&mut io);
        vm.interpret_str(src).unwrap();
        assert_eq!(io.out, expected.to_string() + "\n");
    }

    #[test]
    fn cor_basic() {
        check_out(
            "var a = 2;
             print a;",
            "2",
        );
    }

    #[test]
    fn cor_precedence() {
        check_out("print 2 * 3 + 5 * 7 + 1;", "42");
    }
}
