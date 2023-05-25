use wain_ast::ValType;
use wain_exec::{check_func_signature, ImportInvalidError, ImportInvokeError, Importer, Memory, Stack};

pub struct SpecTestImporter;

// Definition of the host environment for spec test is defined in the reference implementation as
// follows:
// https://github.com/WebAssembly/spec/blob/master/interpreter/host/spectest.ml

impl Importer for SpecTestImporter {
    const MODULE_NAME: &'static str = "spectest";

    fn validate(&self, name: &str, params: &[ValType], ret: Option<ValType>) -> Option<ImportInvalidError> {
        use ValType::*;
        match name {
            "print" => check_func_signature(params, ret, &[], None),
            "print_i32" => check_func_signature(params, ret, &[I32], None),
            "print_i32_f32" => check_func_signature(params, ret, &[I32, F32], None),
            "print_f64_f64" => check_func_signature(params, ret, &[F64, F64], None),
            "print_f32" => check_func_signature(params, ret, &[F32], None),
            "print_f64" => check_func_signature(params, ret, &[F64], None),
            // TODO: Add
            // - "global_i32", "global_f32", "global_f64" external globals
            // - "table" external table
            // - "memory" external memory
            _ => Some(ImportInvalidError::NotFound),
        }
    }

    fn call(&mut self, name: &str, stack: &mut Stack, _: &mut Memory) -> Result<(), ImportInvokeError> {
        match name {
            "print" => Ok(()),
            "print_i32" => {
                let _: i32 = stack.pop();
                Ok(())
            }
            "print_i32_f32" => {
                let _: f32 = stack.pop();
                let _: i32 = stack.pop();
                Ok(())
            }
            "print_f64_f64" => {
                let _: f64 = stack.pop();
                let _: f64 = stack.pop();
                Ok(())
            }
            "print_f32" => {
                let _: f32 = stack.pop();
                Ok(())
            }
            "print_f64" => {
                let _: f64 = stack.pop();
                Ok(())
            }
            // TODO: Add
            // - "global_i32", "global_f32", "global_f64" external globals
            // - "table" external table
            // - "memory" external memory
            _ => Err(ImportInvokeError::Fatal {
                message: format!("not found: {}", name),
            }),
        }
    }
}
