// Module validation for Wasm module
// - https://webassembly.github.io/spec/core/valid/index.html
// - https://webassembly.github.io/spec/core/appendix/algorithm.html#algo-valid
#![forbid(unsafe_code)]

extern crate wain_ast;

mod error;
mod insn;

use error::{Error, ErrorKind, Result};
use std::borrow::Cow;
use std::collections::HashMap;
use wain_ast::source::Source;
use wain_ast::*;

// Validation context
// https://webassembly.github.io/spec/core/valid/conventions.html#context
struct Context<'module, 'source: 'module, S: Source> {
    module: &'module Module<'source>,
    source: &'module S,
}

impl<'m, 's, S: Source> Context<'m, 's, S> {
    fn error<T>(&self, kind: ErrorKind, when: &'static str, offset: usize) -> Result<T, S> {
        Err(Error::new(kind, Cow::Borrowed(when), offset, self.source))
    }

    fn validate_idx<T>(
        &self,
        s: &'m [T],
        idx: u32,
        what: &'static str,
        when: &'static str,
        offset: usize,
    ) -> Result<&'m T, S> {
        if let Some(item) = s.get(idx as usize) {
            Ok(item)
        } else {
            self.error(
                ErrorKind::IndexOutOfBounds {
                    idx,
                    upper: s.len(),
                    what,
                },
                when,
                offset,
            )
        }
    }

    fn type_from_idx(
        &self,
        idx: u32,
        when: &'static str,
        offset: usize,
    ) -> Result<&'m FuncType, S> {
        self.validate_idx(&self.module.types, idx, "type", when, offset)
    }

    fn func_from_idx(&self, idx: u32, when: &'static str, offset: usize) -> Result<&'m Func, S> {
        self.validate_idx(&self.module.funcs, idx, "function", when, offset)
    }

    fn table_from_idx(&self, idx: u32, when: &'static str, offset: usize) -> Result<&'m Table, S> {
        self.validate_idx(&self.module.tables, idx, "table", when, offset)
    }

    fn global_from_idx(
        &self,
        idx: u32,
        when: &'static str,
        offset: usize,
    ) -> Result<&'m Global, S> {
        self.validate_idx(&self.module.globals, idx, "global variable", when, offset)
    }

    fn memory_from_idx(
        &self,
        idx: u32,
        when: &'static str,
        offset: usize,
    ) -> Result<&'m Memory, S> {
        self.validate_idx(&self.module.memories, idx, "memory", when, offset)
    }
}

pub fn validate<'m, 's, S: Source>(root: &'m Root<'s, S>) -> Result<(), S> {
    let mut ctx = Context {
        module: &root.module,
        source: &root.source,
    };
    root.module.validate(&mut ctx)
}

trait Validate<'s, S: Source> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S>;
}

impl<'s, S: Source, V: Validate<'s, S>> Validate<'s, S> for Vec<V> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        self.iter().map(|n| n.validate(ctx)).collect()
    }
}

impl<'s, S: Source, V: Validate<'s, S>> Validate<'s, S> for Option<V> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        match self {
            Some(node) => node.validate(ctx),
            None => Ok(()),
        }
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#valid-module
impl<'s, S: Source> Validate<'s, S> for Module<'s> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
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
            return ctx.error(
                ErrorKind::MultipleTables(self.tables.len()),
                "tables in module",
                self.start,
            );
        }
        if self.memories.len() > 1 {
            return ctx.error(
                ErrorKind::MultipleMemories(self.memories.len()),
                "memories in module",
                self.start,
            );
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
                    "exports in module",
                    offset,
                );
            }
        }

        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/types.html#valid-functype
impl<'s, S: Source> Validate<'s, S> for FuncType {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        if self.results.len() > 1 {
            ctx.error(
                ErrorKind::MultipleReturnTypes(self.results.clone()),
                "result types in function type",
                self.start,
            )
        } else {
            Ok(())
        }
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#tables
impl<'s, S: Source> Validate<'s, S> for Table<'s> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        // Validation for table type is unnecessary here
        // https://webassembly.github.io/spec/core/syntax/types.html#syntax-tabletype
        // Limits should be within 2**32 but the values are already u32. It should be validated by parser

        if let Limits::Range(min, max) = self.ty.limit {
            if min > max {
                return ctx.error(
                    ErrorKind::InvalidLimitRange(min, max),
                    "limits in table type",
                    self.start,
                );
            }
        }
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#memories
impl<'s, S: Source> Validate<'s, S> for Memory<'s> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
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
                "limits in memory",
                self.start,
            );
        }

        if let Limits::Range(min, max) = self.ty.limit {
            if min > max {
                return ctx.error(
                    ErrorKind::InvalidLimitRange(min, max),
                    "limits in memory type",
                    self.start,
                );
            }
        }

        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#globals
impl<'s, S: Source> Validate<'s, S> for Global<'s> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        // https://webassembly.github.io/spec/core/valid/types.html#valid-globaltype
        // Nothing to do for validating GlobalType

        match &self.kind {
            GlobalKind::Import(_) => Ok(()),
            GlobalKind::Init(init) => crate::insn::validate_constant(
                init,
                ctx,
                self.ty,
                "init expression for global variable",
                self.start,
            ),
        }
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#element-segments
impl<'s, S: Source> Validate<'s, S> for ElemSegment {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        ctx.table_from_idx(self.idx, "element segment", self.start)?;
        crate::insn::validate_constant(
            &self.offset,
            ctx,
            ValType::I32,
            "offset expression in element segment",
            self.start,
        )?;
        for funcidx in self.init.iter() {
            ctx.func_from_idx(*funcidx, "init in element segment", self.start)?;
        }
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#data-segments
impl<'s, S: Source> Validate<'s, S> for DataSegment<'s> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        ctx.memory_from_idx(self.idx, "data segment", self.start)?;
        crate::insn::validate_constant(
            &self.offset,
            ctx,
            ValType::I32,
            "offset expression in data segment",
            self.start,
        )?;
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#start-function
impl<'s, S: Source> Validate<'s, S> for StartFunction {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        let func = ctx.func_from_idx(self.idx, "start function in module", self.start)?;
        let fty = ctx.type_from_idx(func.idx, "start function in module", self.start)?;
        if !fty.params.is_empty() || !fty.results.is_empty() {
            return ctx.error(
                ErrorKind::StartFunctionSignature {
                    idx: self.idx,
                    params: fty.params.clone(),
                    results: fty.results.clone(),
                },
                "start function in 'start' section",
                self.start,
            );
        }
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#exports
impl<'s, S: Source> Validate<'s, S> for Export<'s> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        match self.kind {
            ExportKind::Func(idx) => {
                let func = ctx.func_from_idx(idx, "exported function", self.start)?;
                if self.name.0 == "_start" {
                    // Functions were already validated
                    let fty = &ctx.module.types[func.idx as usize];
                    if !fty.params.is_empty() || !fty.results.is_empty() {
                        return ctx.error(
                            ErrorKind::StartFunctionSignature {
                                idx,
                                params: fty.params.clone(),
                                results: fty.results.clone(),
                            },
                            "start function exported as '_start'",
                            self.start,
                        );
                    }
                }
            }
            ExportKind::Table(idx) => {
                ctx.table_from_idx(idx, "exported table", self.start)?;
            }
            ExportKind::Memory(idx) => {
                ctx.memory_from_idx(idx, "exported memory", self.start)?;
            }
            ExportKind::Global(idx) => {
                ctx.global_from_idx(idx, "exported global variable", self.start)?;
            }
        }
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/modules.html#functions
impl<'s, S: Source> Validate<'s, S> for Func<'s> {
    fn validate<'m>(&self, ctx: &mut Context<'m, 's, S>) -> Result<(), S> {
        let func_ty = ctx.type_from_idx(self.idx, "function", self.start)?;
        match &self.kind {
            FuncKind::Import(_) => Ok(()),
            FuncKind::Body { locals, expr } => {
                crate::insn::validate_func_body(expr, func_ty, locals, ctx, self.start)
            }
        }
    }
}
