//! The lox virtual machine implementation

use std::{collections::HashMap, rc::Rc};

use crate::{
    chunk::Chunk,
    compiler::{self, Compiler},
    func::{Func, FuncKind},
    op::Op,
    value::{HeapValue, Value},
};

/// The trait providing the VM with IO
pub trait VmIO {
    /// Write a string to the output
    fn write(&mut self, s: &str);
}

/// The default VM IO, uses stdout
pub struct DefaultVMIO {}

impl DefaultVMIO {
    /// Create the default VM IO
    pub fn new() -> Self {
        Self {}
    }
}

impl VmIO for DefaultVMIO {
    fn write(&mut self, s: &str) {
        println!("{}", s);
    }
}

/// The VM
pub struct VM<'a, IO: VmIO = DefaultVMIO> {
    globals: HashMap<String, Value>,
    io: &'a mut IO,
    pub stack: Vec<Value>,
    call_frames: Vec<CallFrame>,
}

struct CallFrame {
    pub func: Func,
    pub ii: usize,
    pub stack_i: usize,
}

#[derive(Debug)]
/// A VM error
pub enum Error {
    /// A compile error
    Compiler(compiler::Error),
    /// A runtime error
    Runtime {
        /// The line causing the error
        line: u32,
        /// The kind of error
        kind: RuntimeError,
    },
}

#[derive(Debug)]
/// A runtime error
pub enum RuntimeError {
    /// A operation decode error
    Decode(usize),
    /// An unsupported binary operation between two values
    Binary {
        /// The left hand side of the operation
        lhs: Value,
        /// The right hand side of the operation
        rhs: Value,
        /// The binary operation
        op: Op,
    },
    /// An unsupported unary operation on a value
    Unary {
        /// The value the unary was tried to operate on
        v: Value,
        /// The unary operation
        op: Op,
    },
    /// An undefined global variable
    Undefined(String),
    /// A non-boolean value used in a boolean context
    NonBool(Value),
    MismatchedNArgs {
        expected: u8,
        got: u8,
    },
    Uncallable {
        v: Value,
    },
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

            RuntimeError::Undefined(i) => write!(f, "No such variable '{}'", i),
            RuntimeError::NonBool(v) => write!(
                f,
                "Cannot cast value of type {} into a boolean",
                v.type_desc()
            ),
            RuntimeError::MismatchedNArgs { expected, got } => write!(
                f,
                "Wrong number of arguments when calling function, expected {} but got {}",
                expected, got
            ),
            RuntimeError::Uncallable { v } => {
                write!(f, "Cannot call value of type {}", v.type_desc())
            }
        }
    }
}

impl<'a, IO: VmIO> VM<'a, IO> {
    /// Create a new VM
    pub fn new(io: &'a mut IO) -> Self {
        Self {
            globals: HashMap::new(),
            io,
            call_frames: vec![],
            stack: vec![],
        }
    }

    fn frame(&self) -> &CallFrame {
        self.call_frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.call_frames.last_mut().unwrap()
    }

    fn chunk(&self) -> &Chunk {
        &self.frame().func.chunk
    }

    fn ii(&self) -> usize {
        self.frame().ii
    }

    fn ii_mut(&mut self) -> &mut usize {
        &mut self.frame_mut().ii
    }

    fn fetch(&mut self) -> Result<Option<Op>, Error> {
        if self.frame().ii >= self.frame().func.chunk.get_offset() {
            return Ok(None);
        }

        match self.frame().func.chunk.decode_op(self.frame().ii) {
            Ok((op, d)) => {
                self.frame_mut().ii += d;
                Ok(Some(op))
            }
            Err(_) => self.make_runtime_error(RuntimeError::Decode(self.frame().ii)),
        }
    }

    fn make_runtime_error<U>(&self, kind: RuntimeError) -> Result<U, Error> {
        Err(Error::Runtime {
            line: self.frame().func.chunk.get_line(self.frame().ii),
            kind,
        })
    }

    /// Interpret a code chunk
    pub fn interpret(&mut self, chunk: Chunk) -> Result<(), Error> {
        self.call_frames = vec![CallFrame {
            func: Func {
                chunk,
                kind: FuncKind::Script,
            },
            ii: 0,
            stack_i: 0,
        }];

        loop {
            #[cfg(feature = "debug_trace")]
            let prev_ii = self.ii();

            let op = if let Some(op) = self.fetch()? {
                op
            } else {
                break;
            };
            #[cfg(feature = "debug_trace")]
            {
                eprint!("{}-{} {} [", prev_ii, self.ii(), op);
                for i in 0..self.stack.len() {
                    eprint!("{}", self.stack[i]);
                    if i != self.stack.len() - 1 {
                        eprint!(", ");
                    }
                }
                eprintln!("]");
            }

            match op {
                Op::Return => {
                    let v = self.pop();
                    while self.frame().stack_i < self.stack.len() {
                        self.pop();
                    }
                    self.call_frames.pop().unwrap();

                    if self.call_frames.len() == 0 {
                        self.pop();
                        return Ok(());
                    }

                    self.push(v);
                }
                Op::Constant(offset) => {
                    let v = self.chunk().get_constant(offset).unwrap().clone();
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
                        (Value::HeapValue(hlhs), Value::HeapValue(hrhs)) => {
                            match (&*hlhs.as_ref(), &*hrhs.as_ref()) {
                                (HeapValue::String(l), HeapValue::String(r)) => {
                                    Value::HeapValue(Rc::new(HeapValue::String(l.to_owned() + r)))
                                }
                                _ => {
                                    self.make_runtime_error(RuntimeError::Binary { lhs, rhs, op })?
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
                            (HeapValue::Function { .. }, HeapValue::Function { .. }) => {
                                Rc::ptr_eq(&lhs, &rhs)
                            }
                            _ => false,
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
                    let v = self.frame().func.chunk.get_constant(c).unwrap().clone();
                    let s = match &v {
                        Value::HeapValue(h) => match &*h.as_ref() {
                            HeapValue::String(s) => s,
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    let v = self.pop();
                    self.globals.insert(s.to_string(), v);
                }
                Op::GetGlobal(c) => {
                    let v = self.frame().func.chunk.get_constant(c).unwrap().clone();
                    let s = match &v {
                        Value::HeapValue(h) => match &*h.as_ref() {
                            HeapValue::String(s) => s,
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    if let Some(v) = self.globals.get(s) {
                        self.push(v.clone());
                    } else {
                        self.make_runtime_error(RuntimeError::Undefined(s.to_string()))?;
                    }
                }
                Op::SetGlobal(c) => {
                    let v = self.chunk().get_constant(c).unwrap().clone();
                    let s = match &v {
                        Value::HeapValue(h) => match &*h.as_ref() {
                            HeapValue::String(s) => s,
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    let val = self.peek();
                    if let Some(v) = self.globals.get_mut(s) {
                        *v = val;
                    } else {
                        self.make_runtime_error(RuntimeError::Undefined(s.to_string()))?;
                    }
                }
                Op::GetLocal(l) => {
                    self.push(self.stack[self.frame().stack_i + l as usize].clone());
                }
                Op::SetLocal(l) => {
                    let stack_offset = self.frame().stack_i;
                    self.stack[stack_offset + l as usize] = self.peek();
                }
                Op::JumpIfFalse(j) => {
                    let v = self.peek();
                    if self.is_falsey(&v)? {
                        *self.ii_mut() += j as usize;
                    }
                }
                Op::Jump(j) => {
                    *self.ii_mut() += j as usize;
                }
                Op::Loop(j) => {
                    *self.ii_mut() -= j as usize;
                }
                Op::Call(argc) => {
                    let v = self.peek_n(argc as usize);
                    let ok = match &v {
                        Value::HeapValue(hv) => match hv.as_ref() {
                            HeapValue::Function(
                                f @ Func {
                                    kind: FuncKind::Function { arity, .. },
                                    ..
                                },
                            ) => {
                                if *arity != argc {
                                    self.make_runtime_error(RuntimeError::MismatchedNArgs {
                                        expected: *arity,
                                        got: argc,
                                    })?;
                                }

                                let stack_i = self.stack.len() - argc as usize - 1;
                                // TODO: Call the function
                                // TODO: Stack traces on errors
                                self.call_frames.push(CallFrame {
                                    func: f.clone(),
                                    ii: 0,
                                    stack_i,
                                });

                                true
                            }
                            _ => false,
                        },
                        _ => false,
                    };

                    if !ok {
                        self.make_runtime_error(RuntimeError::Uncallable { v: v.clone() })?;
                    }
                }
            }
        }

        Ok(())
    }

    fn is_falsey(&mut self, v: &Value) -> Result<bool, Error> {
        match v {
            Value::Nil => Ok(true),
            Value::Bool(b) => Ok(!b),
            _ => self.make_runtime_error(RuntimeError::NonBool(v.clone())),
        }
    }

    /// Interpret a string by compiling and interpreting the resulting code chunk
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
        self.peek_n(0)
    }

    fn peek_n(&self, offset: usize) -> Value {
        let i = self.stack.len() - 1 - offset;
        self.stack[i].clone()
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
            chunk.push_constant(c);
        }
        for op in ops {
            chunk.push_op(op, 0);
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
        assert_eq!(io.out.trim_end(), expected);
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

    #[test]
    fn globals() {
        check_out("var a; print a; a = 3; print a;", "nil\n3");
    }

    #[test]
    fn locals() {
        check_out("var a = 3; { var a = 2; print a; } print a;", "2\n3");
    }

    #[test]
    fn if_() {
        check_out("if (true) print \"a\";", "a");
        check_out("if (false) print \"a\";", "");
    }

    #[test]
    fn if_else() {
        check_out(r#"if (true) print "a"; else print "b";"#, "a");
        check_out(r#"if (false) print "a"; else print "b";"#, "b");
    }

    #[test]
    fn while_loop() {
        check_out(
            r#"
            var a = 0;
            while (a < 5) {
                print a;
                a = a + 1;
            }
            "#,
            "0\n1\n2\n3\n4",
        )
    }

    #[test]
    fn for_loop() {
        check_out(
            r#"
            for (var i = 0; i < 5; i = i + 1)
                print i;
            "#,
            "0\n1\n2\n3\n4",
        );

        check_out(
            r#"
            var i = 0;
            for (; i < 5; i = i + 1)
                print i;
            "#,
            "0\n1\n2\n3\n4",
        );

        check_out(
            r#"
            var i = 0;
            for (; i < 5;) {
                print i;
                i = i + 1;
            }
            "#,
            "0\n1\n2\n3\n4",
        );

        check_out(
            r#"
            for (var i = 0; i < 5;) {
                print i;
                i = i + 1;
            }
            "#,
            "0\n1\n2\n3\n4",
        );
    }

    #[test]
    fn comparisons() {
        check_out(
            r#"
            print 5 > 4;
            print 4 < 5;
            print 4 > 5;
            print 5 < 4;
            print 5 == 5;
            print 5 != 5;
            "#,
            "true\ntrue\nfalse\nfalse\ntrue\nfalse",
        )
    }

    #[test]
    fn strings() {
        check_out(
            r#"
            print "asd";
            print "asd" + "basd";
            print "asd" == "asd";
            "#,
            "asd\nasdbasd\ntrue",
        )
    }

    #[test]
    fn bool_comparison() {
        check_out(
            r#"
            print true and "lala";
            print false or "asd";
            "#,
            "lala\nasd",
        )
    }

    #[test]
    fn arithmetic() {
        check_out(
            r#"
            print 1 + 2 + 3 * 4 - 7 / 7;
            "#,
            "14",
        )
    }
}
