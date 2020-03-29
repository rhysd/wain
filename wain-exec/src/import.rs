use crate::memory::Memory;
use crate::stack::Stack;
use std::io::{self, Read, Write};

pub enum ImportError {
    Fatal { message: String },
    NotFound,
}

pub trait Importer {
    fn call(
        &mut self,
        name: &str,
        stack: &mut Stack,
        memory: &mut Memory,
    ) -> Result<(), ImportError>;
}

pub struct DefaultImporter<R: Read, W: Write> {
    stdout: W,
    stdin: R,
}

impl Default for DefaultImporter<io::BufReader<io::Stdin>, io::BufWriter<io::Stdout>> {
    fn default() -> Self {
        let stdin = io::BufReader::new(io::stdin());
        let stdout = io::BufWriter::new(io::stdout());
        Self { stdin, stdout }
    }
}

impl<R: Read, W: Write> Drop for DefaultImporter<R, W> {
    fn drop(&mut self) {
        let _ = self.stdout.flush();
    }
}

impl<R: Read, W: Write> DefaultImporter<R, W> {
    pub fn with_stdio(stdin: R, stdout: W) -> Self {
        Self { stdin, stdout }
    }

    fn putchar(&mut self, stack: &mut Stack) {
        let v: i32 = stack.pop();
        let b = v as u8;
        let ret = match self.stdout.write(&[b]) {
            Ok(_) => b as i32,
            Err(_) => -1, // EOF
        };
        stack.push(ret);
    }

    fn getchar(&mut self, stack: &mut Stack) {
        let mut buf = [0u8];
        let v = match self.stdin.read_exact(&mut buf) {
            Ok(()) => buf[0] as i32,
            Err(_) => -1, // EOF
        };
        stack.push(v);
    }
}

impl<R: Read, W: Write> Importer for DefaultImporter<R, W> {
    fn call(&mut self, name: &str, stack: &mut Stack, _: &mut Memory) -> Result<(), ImportError> {
        match name {
            "putchar" => {
                self.putchar(stack);
                Ok(())
            }
            "getchar" => {
                self.getchar(stack);
                Ok(())
            }
            _ => Err(ImportError::NotFound),
        }
    }
}
