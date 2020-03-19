// Module validation for Wasm module
// - https://webassembly.github.io/spec/core/valid/index.html
// - https://webassembly.github.io/spec/core/appendix/algorithm.html#algo-valid
extern crate wain_ast;

mod error;
mod insn;

use error::{Error, ErrorKind, Result};
use wain_ast::*;

// Validation context
// https://webassembly.github.io/spec/core/valid/conventions.html#context
struct Context<'module, 'a: 'module> {
    module: &'module Module<'a>,
    source: &'a str,
}

impl<'module, 'a> Context<'module, 'a> {
    fn error<T>(&self, kind: ErrorKind, offset: usize) -> Result<'a, T> {
        Err(Error::new(kind, offset, self.source))
    }

    fn validate_idx<T>(
        &self,
        s: &'module [T],
        idx: u32,
        what: &'static str,
        offset: usize,
    ) -> Result<'a, &'module T> {
        if let Some(item) = s.get(idx as usize) {
            Ok(item)
        } else {
            self.error(
                ErrorKind::IndexOutOfBounds {
                    idx,
                    upper: s.len(),
                    what,
                },
                offset,
            )
        }
    }

    fn type_from_idx(&self, idx: u32, offset: usize) -> Result<'a, &'module FuncType> {
        self.validate_idx(&self.module.types, idx, "type", offset)
    }

    fn func_from_idx(&self, idx: u32, offset: usize) -> Result<'a, &'module Func> {
        self.validate_idx(&self.module.funcs, idx, "function", offset)
    }

    fn table_from_idx(&self, idx: u32, offset: usize) -> Result<'a, &'module Table> {
        self.validate_idx(&self.module.tables, idx, "table", offset)
    }

    fn global_from_idx(&self, idx: u32, offset: usize) -> Result<'a, &'module Global> {
        self.validate_idx(&self.module.globals, idx, "global variable", offset)
    }

    fn memory_from_idx(&self, idx: u32, offset: usize) -> Result<'a, &'module Memory> {
        self.validate_idx(&self.module.memories, idx, "memory", offset)
    }
}

pub fn validate<'module, 'a>(module: &'module Module<'a>, source: &'a str) -> Result<'a, ()> {
    let mut ctx = Context {
        module: &module,
        source,
    };
    module.validate(&mut ctx)
}

trait Validate<'a> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()>;
}

impl<'a, V: Validate<'a>> Validate<'a> for Vec<V> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        self.iter().map(|n| n.validate(ctx)).collect()
    }
}

impl<'a, V: Validate<'a>> Validate<'a> for Option<V> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        match self {
            Some(node) => node.validate(ctx),
            None => Ok(()),
        }
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#valid-module
impl<'a> Validate<'a> for Module<'a> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        self.types.validate(ctx)?;
        self.tables.validate(ctx)?;
        self.memories.validate(ctx)?;
        self.globals.validate(ctx)?;
        // TODO self.elems
        // TODO self.exports
        // TODO self.data
        self.funcs.validate(ctx)?;
        // TODO self.entrypoint
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/types.html#valid-functype
impl<'a> Validate<'a> for FuncType {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        if self.results.len() > 1 {
            ctx.error(
                ErrorKind::MultipleReturnTypes(self.results.clone()),
                self.start,
            )
        } else {
            Ok(())
        }
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#imports
// Not implement Validate<'a> since the offset parameter is necessary for better error message
#[derive(PartialEq, Eq)]
enum ImportKind {
    Func,
    Table,
    Memory,
    Global,
}
impl ImportKind {
    fn name(self) -> &'static str {
        match self {
            ImportKind::Func => "function",
            ImportKind::Table => "table",
            ImportKind::Memory => "memory",
            ImportKind::Global => "global variable",
        }
    }
}
fn validate_import<'module, 'a>(
    import: &Import<'a>,
    kind: ImportKind,
    ctx: &mut Context<'module, 'a>,
    offset: usize,
) -> Result<'a, ()> {
    if import.mod_name.0 != "env" && import.name.0 != "print" {
        let mod_name = import.mod_name.0.to_string();
        let name = import.name.0.to_string();
        ctx.error(ErrorKind::UnknownImport { mod_name, name }, offset)
    } else if kind != ImportKind::Func {
        let mod_name = import.mod_name.0.to_string();
        let name = import.name.0.to_string();
        ctx.error(
            ErrorKind::ImportKindMismatch {
                mod_name,
                name,
                expected: ImportKind::Func.name(),
                actual: kind.name(),
            },
            offset,
        )
    } else {
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#tables
impl<'a> Validate<'a> for Table<'a> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        // Validation for table type is unnecessary here
        // https://webassembly.github.io/spec/core/syntax/types.html#syntax-tabletype
        // Limits should be within 2**32 but the values are already u32. It should be validated by parser

        if let Some(import) = &self.import {
            validate_import(import, ImportKind::Table, ctx, self.start)?;
        }

        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#memories
impl<'a> Validate<'a> for Memory<'a> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        // https://webassembly.github.io/spec/core/valid/types.html#valid-memtype
        let limit = 1 << 16;
        let invalid = match self.ty.limit {
            Limits::From(min) if min > limit => Some(min),
            Limits::Range(min, _) if min > limit => Some(min),
            Limits::Range(_, max) if max > limit => Some(max),
            _ => None,
        };

        if let Some(value) = invalid {
            return ctx.error(
                ErrorKind::LimitsOutOfRange {
                    value,
                    min: 0,
                    max: limit,
                    what: "memory type",
                },
                self.start,
            );
        }

        if let Some(import) = &self.import {
            validate_import(import, ImportKind::Memory, ctx, self.start)?;
        }

        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#globals
impl<'a> Validate<'a> for Global<'a> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        // https://webassembly.github.io/spec/core/valid/types.html#valid-globaltype
        // Nothing to do for validating GlobalType

        match &self.kind {
            GlobalKind::Import(import) => {
                validate_import(import, ImportKind::Global, ctx, self.start)
            }
            GlobalKind::Init(init) => crate::insn::validate_constant(
                init,
                &ctx.module.globals,
                self.ty,
                ctx.source,
                self.start,
            ),
        }
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#functions
impl<'a> Validate<'a> for Func<'a> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        let func_ty = ctx.type_from_idx(self.idx, self.start)?;
        match &self.kind {
            FuncKind::Import(import) => validate_import(import, ImportKind::Func, ctx, self.start),
            FuncKind::Body { locals, expr } => {
                if locals.len() < func_ty.params.len() {
                    return ctx.error(
                        ErrorKind::TooFewFuncLocalsForParams {
                            params: func_ty.params.len(),
                            locals: locals.len(),
                        },
                        self.start,
                    );
                }
                for (i, param) in func_ty.params.iter().enumerate() {
                    let local = locals[i];
                    if local != *param {
                        return ctx.error(
                            ErrorKind::ParamTypeMismatchWithLocal {
                                idx: i,
                                param: *param,
                                local,
                            },
                            self.start,
                        );
                    }
                }

                // FuncType validated func_ty has at most one result type
                let ret = func_ty.results.get(0).copied();
                crate::insn::validate_func_body(expr, locals, ret, ctx, self.start)
            }
        }
    }
}
