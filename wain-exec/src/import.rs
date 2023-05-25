use crate::memory::Memory;
use crate::stack::Stack;
use std::io::{Read, Write};
use wain_ast::ValType;

pub enum ImportInvalidError {
    NotFound,
    SignatureMismatch {
        expected_params: &'static [ValType],
        expected_ret: Option<ValType>,
    },
}

pub enum ImportInvokeError {
    Fatal { message: String },
}

pub trait Importer {
    const MODULE_NAME: &'static str = "env";
    fn validate(&self, name: &str, params: &[ValType], ret: Option<ValType>) -> Option<ImportInvalidError>;
    fn call(&mut self, name: &str, stack: &mut Stack, memory: &mut Memory) -> Result<(), ImportInvokeError>;
}

pub fn check_func_signature(
    actual_params: &[ValType],
    actual_ret: Option<ValType>,
    expected_params: &'static [ValType],
    expected_ret: Option<ValType>,
) -> Option<ImportInvalidError> {
    if actual_params.eq(expected_params) && actual_ret == expected_ret {
        return None;
    }
    Some(ImportInvalidError::SignatureMismatch {
        expected_params,
        expected_ret,
    })
}

pub struct DefaultImporter<R: Read, W: Write> {
    stdout: W,
    stdin: R,
}

impl<R: Read, W: Write> Drop for DefaultImporter<R, W> {
    fn drop(&mut self) {
        let _ = self.stdout.flush();
    }
}

impl<R: Read, W: Write> DefaultImporter<R, W> {
    pub fn with_stdio(stdin: R, stdout: W) -> Self {
        Self { stdout, stdin }
    }

    // (func (param i32) (result i32))
    fn putchar(&mut self, stack: &mut Stack) {
        let v: i32 = stack.pop();
        let b = v as u8;
        let ret = match self.stdout.write(&[b]) {
            Ok(_) => b as i32,
            Err(_) => -1, // EOF
        };
        stack.push(ret);
    }

    // (func () (result i32))
    fn getchar(&mut self, stack: &mut Stack) {
        let mut buf = [0u8];
        let v = match self.stdin.read_exact(&mut buf) {
            Ok(()) => buf[0] as i32,
            Err(_) => -1, // EOF
        };
        stack.push(v);
    }

    // (func (param i32 i32 i32) (result i32))
    fn memcpy(&mut self, stack: &mut Stack, memory: &mut Memory) -> Result<(), ImportInvokeError> {
        // memcpy(void *dest, void *src, size_t n)
        let size = stack.pop::<i32>() as u32 as usize;
        let src_start = stack.pop::<i32>() as u32 as usize;
        let dest_i32: i32 = stack.pop();
        let dest_start = dest_i32 as u32 as usize;
        let src_end = src_start + size;
        let dest_end = dest_start + size;

        let (dest, src) = if dest_end <= src_start {
            let (dest, src) = memory.data_mut().split_at_mut(src_start);
            (&mut dest[dest_start..dest_end], &mut src[..size])
        } else if src_end <= dest_start {
            let (src, dest) = memory.data_mut().split_at_mut(dest_start);
            (&mut dest[..size], &mut src[src_start..src_end])
        } else {
            return Err(ImportInvokeError::Fatal {
                message: format!(
                    "range overwrap on memcpy: src={}..{} and dest={}..{}",
                    src_start, src_end, dest_start, dest_end
                ),
            });
        };

        dest.copy_from_slice(src);
        stack.push(dest_i32);
        Ok(())
    }
}

impl<R: Read, W: Write> Importer for DefaultImporter<R, W> {
    fn validate(&self, name: &str, params: &[ValType], ret: Option<ValType>) -> Option<ImportInvalidError> {
        use ValType::*;
        match name {
            "putchar" => check_func_signature(params, ret, &[I32], Some(I32)),
            "getchar" => check_func_signature(params, ret, &[], Some(I32)),
            "memcpy" => check_func_signature(params, ret, &[I32, I32, I32], Some(I32)),
            "abort" => check_func_signature(params, ret, &[], None),
            _ => Some(ImportInvalidError::NotFound),
        }
    }

    fn call(&mut self, name: &str, stack: &mut Stack, memory: &mut Memory) -> Result<(), ImportInvokeError> {
        match name {
            "putchar" => {
                self.putchar(stack);
                Ok(())
            }
            "getchar" => {
                self.getchar(stack);
                Ok(())
            }
            "abort" => Err(ImportInvokeError::Fatal {
                message: "aborted".to_string(),
            }),
            "memcpy" => self.memcpy(stack, memory),
            _ => unreachable!("fatal: invalid import function '{}'", name),
        }
    }
}
