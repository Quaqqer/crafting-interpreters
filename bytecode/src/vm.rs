use crate::chunk::Chunk;

struct VM {
    chunk: Chunk,
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        Self { chunk }
    }
}
