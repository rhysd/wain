pub struct Parser<'source> {
    input: &'source [u8],
}

impl<'s> Parser<'s> {
    pub fn new(input: &'s [u8]) -> Parser<'s> {
        Self { input }
    }

    fn end(&self) -> bool {
        self.input.is_empty()
    }

    fn eat(&mut self, bytes: usize) {
        self.input = &self.input[bytes..];
    }
}
