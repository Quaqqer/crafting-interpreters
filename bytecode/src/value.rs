//! The values in the lox VM
//!
use std::rc::Rc;

use crate::{
    chunk::Chunk,
    func::{Func, FuncKind},
};

#[derive(Clone, Debug, PartialEq)]
/// A basic value
pub enum Value {
    /// A number value
    Number(f64),
    /// A boolean value
    Bool(bool),
    /// The nil value
    Nil,
    /// A value stored on the heap (reference counted)
    HeapValue(Rc<HeapValue>),
}

impl Value {
    /// The type name of the value
    pub fn type_desc(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::Bool(_) => "bool",
            Value::Nil => "nil",
            Value::HeapValue(h) => h.type_desc(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Bool(b) => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Value::Nil => write!(f, "nil"),
            Value::HeapValue(h) => write!(f, "{}", h),
        }
    }
}

#[derive(Debug, PartialEq)]
/// A value stored on the heap
pub enum HeapValue {
    /// A string
    String(String),

    Function(Func),
}

impl HeapValue {
    /// The type name of the value
    pub fn type_desc(&self) -> &'static str {
        match self {
            HeapValue::String(_) => "string",
            HeapValue::Function { .. } => "function",
        }
    }
}

impl std::fmt::Display for HeapValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapValue::String(s) => write!(f, "{}", s),
            HeapValue::Function(Func { kind, .. }) => match kind {
                FuncKind::Script => write!(f, "script"),
                FuncKind::Function { arity, name } => write!(f, "{}({}...)", name, arity),
            },
        }
    }
}
