// Module validation for Wasm module
// - https://webassembly.github.io/spec/core/valid/index.html
// - https://webassembly.github.io/spec/core/appendix/algorithm.html#algo-valid
extern crate wain_ast;

mod error;
mod insn;

use error::{Error, ErrorKind, Result};
use std::collections::HashMap;
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
        self.funcs.validate(ctx)?;
        self.tables.validate(ctx)?;
        self.memories.validate(ctx)?;
        self.globals.validate(ctx)?;
        self.elems.validate(ctx)?;
        self.data.validate(ctx)?;
        self.entrypoint.validate(ctx)?;
        self.exports.validate(ctx)?;

        if self.tables.len() > 1 {
            return ctx.error(ErrorKind::MultipleTables(self.tables.len()), self.start);
        }
        if self.memories.len() > 1 {
            return ctx.error(ErrorKind::MultipleMemories(self.memories.len()), self.start);
        }

        // Export name in module must be unique
        let mut seen = HashMap::new();
        for (name, offset) in self.exports.iter().map(|e| (e.name.0.as_ref(), e.start)) {
            if let Some(prev_offset) = seen.insert(name, offset) {
                return ctx.error(
                    ErrorKind::AlreadyExported {
                        name: name.to_string(),
                        prev_offset,
                    },
                    offset,
                );
            }
        }

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

// https://webassembly.github.io/spec/core/valid/modules.html#element-segments
impl<'a> Validate<'a> for ElemSegment {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        ctx.table_from_idx(self.idx, self.start)?;
        crate::insn::validate_constant(
            &self.offset,
            &ctx.module.globals,
            ValType::I32,
            ctx.source,
            self.start,
        )?;
        for funcidx in self.init.iter() {
            ctx.func_from_idx(*funcidx, self.start)?;
        }
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#data-segments
impl<'a> Validate<'a> for DataSegment<'a> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        ctx.memory_from_idx(self.idx, self.start)?;
        crate::insn::validate_constant(
            &self.offset,
            &ctx.module.globals,
            ValType::I32,
            ctx.source,
            self.start,
        )?;
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#start-function
impl<'a> Validate<'a> for StartFunction {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        let func = ctx.func_from_idx(self.idx, self.start)?;
        let fty = ctx.type_from_idx(func.idx, self.start)?;
        if !fty.params.is_empty() || !fty.results.is_empty() {
            return ctx.error(
                ErrorKind::StartFunctionSignature {
                    idx: self.idx,
                    params: fty.params.clone(),
                    results: fty.results.clone(),
                },
                self.start,
            );
        }
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#exports
impl<'a> Validate<'a> for Export<'a> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        match self.kind {
            ExportKind::Func(idx) => {
                ctx.func_from_idx(idx, self.start)?;
            }
            ExportKind::Table(idx) => {
                ctx.table_from_idx(idx, self.start)?;
            }
            ExportKind::Memory(idx) => {
                ctx.memory_from_idx(idx, self.start)?;
            }
            ExportKind::Global(idx) => {
                ctx.global_from_idx(idx, self.start)?;
            }
        }
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#functions
impl<'a> Validate<'a> for Func<'a> {
    fn validate<'module>(&self, ctx: &mut Context<'module, 'a>) -> Result<'a, ()> {
        let func_ty = ctx.type_from_idx(self.idx, self.start)?;
        match &self.kind {
            FuncKind::Import(import) => validate_import(import, ImportKind::Func, ctx, self.start),
            FuncKind::Body { locals, expr } => {
                crate::insn::validate_func_body(expr, func_ty, locals, ctx, self.start)
            }
        }
    }
}
