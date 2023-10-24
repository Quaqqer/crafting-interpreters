use crate::chunk::Chunk;

// Chunk should be RC so it is not cloned...
#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    pub chunk: Chunk,
    pub kind: FuncKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FuncKind {
    Script,
    Function { arity: u8, name: String },
}
