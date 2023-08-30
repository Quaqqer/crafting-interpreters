use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    HeapValue(Rc<HeapValue>),
}

impl Value {
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

#[derive(Clone, PartialEq, Debug)]
pub enum HeapValue {
    String(String),
}

impl HeapValue {
    pub fn type_desc(&self) -> &'static str {
        match self {
            HeapValue::String(_) => "string",
        }
    }
}

impl std::fmt::Display for HeapValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapValue::String(s) => write!(f, "\"{}\"", s),
        }
    }
}
