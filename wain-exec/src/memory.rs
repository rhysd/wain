pub struct Memory {
    bytes: Vec<u8>,
}

impl Memory {
    pub fn new() -> Self {
        Memory { bytes: vec![] }
    }
}
