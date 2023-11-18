use crate::ast as wat;
use crate::source::{describe_position, TextSource};
use std::collections::HashMap;
use std::fmt;
use std::mem;
use wain_ast as wasm;

#[cfg_attr(test, derive(Debug))]
pub enum TransformErrorKind<'source> {
    IdIsNotDefined {
        id: &'source str,
        what: &'static str,
    },
    IdAlreadyDefined {
        id: &'source str,
        idx: u32,
        what: &'static str,
    },
    LabelAndIdMismatch {
        label: Option<&'source str>,
        id: &'source str,
    },
    FuncTypeMismatch {
        left_params: Vec<wat::ValType>,
        left_results: Vec<wat::ValType>,
        right_params: Vec<wat::ValType>,
        right_results: Vec<wat::ValType>,
    },
}

#[cfg_attr(test, derive(Debug))]
pub struct TransformError<'s> {
    kind: TransformErrorKind<'s>,
    offset: usize,
    source: &'s str,
}

impl<'s> TransformError<'s> {
    fn new(kind: TransformErrorKind<'s>, offset: usize, source: &'s str) -> Box<Self> {
        Box::new(TransformError { kind, offset, source })
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn source(&self) -> &'s str {
        self.source
    }
}

impl<'s> fmt::Display for TransformError<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TransformErrorKind::*;
        match &self.kind {
            IdIsNotDefined { id, what } => write!(f, "No identifier '{}' for {} is found", id, what)?,
            IdAlreadyDefined { id, idx, what } => write!(
                f,
                "Identifier '{}' is already defined to be mapped to index {} for {}",
                id, idx, what
            )?,
            LabelAndIdMismatch { label: Some(label), id } => write!(
                f,
                "in control instruction, label '{}' and identifier '{}' must be the same",
                label, id
            )?,
            LabelAndIdMismatch { label: None, id } => write!(
                f,
                "in control instruction, no label specified but identifier '{}' is set",
                id
            )?,
            FuncTypeMismatch {
                left_params,
                left_results,
                right_params,
                right_results,
            } => {
                fn write_type_seq(f: &mut fmt::Formatter<'_>, tys: &[wat::ValType]) -> fmt::Result {
                    f.write_str("[")?;
                    let mut tys = tys.iter();
                    if let Some(ty) = tys.next() {
                        write!(f, "{}", ty)?;
                    }
                    for ty in tys {
                        write!(f, " {}", ty)?;
                    }
                    f.write_str("]")
                }
                write!(f, "function type mismatch between ")?;
                write_type_seq(f, left_params)?;
                write!(f, " -> ")?;
                write_type_seq(f, left_results)?;
                write!(f, " and ")?;
                write_type_seq(f, right_params)?;
                write!(f, " -> ")?;
                write_type_seq(f, right_results)?;
            }
        }

        describe_position(f, self.source, self.offset)
    }
}

type Result<'s, T> = ::std::result::Result<T, Box<TransformError<'s>>>;

type Indices<'s> = HashMap<&'s str, u32>;

struct LabelStack<'s> {
    source: &'s str,
    stack: Vec<Option<&'s str>>,
}

impl<'s> LabelStack<'s> {
    fn new(source: &'s str) -> Self {
        Self { source, stack: vec![] }
    }

    fn resolve(&self, idx: wat::Index<'s>, offset: usize) -> Result<'s, u32> {
        match idx {
            wat::Index::Num(idx) => Ok(idx),
            wat::Index::Ident(id) => {
                if let Some(u) = self.find(id) {
                    Ok(u)
                } else {
                    Err(TransformError::new(
                        TransformErrorKind::IdIsNotDefined { id, what: "label" },
                        offset,
                        self.source,
                    ))
                }
            }
        }
    }

    fn push(&mut self, label: Option<&'s str>, id: Option<&'s str>, offset: usize) -> Result<'s, ()> {
        let label = match (label, id) {
            (Some(label), Some(id)) if label == id => label,
            (_, Some(id)) => {
                // When id is set, label must be specified since id indicates matching delimiters.
                // https://webassembly.github.io/spec/core/text/instructions.html#control-instructions
                return Err(TransformError::new(
                    TransformErrorKind::LabelAndIdMismatch { label, id },
                    offset,
                    self.source,
                ));
            }
            (Some(label), None) => label,
            (None, None) => {
                self.stack.push(None);
                return Ok(());
            }
        };

        // Note: Labels can be redefined.
        //
        //   (block $l (result i32)
        //     (block $l (result i32) (i32.const 2))
        //   )

        self.stack.push(Some(label));
        Ok(())
    }

    fn pop(&mut self) {
        let popped = self.stack.pop().is_some();
        assert!(popped);
    }

    // Each structured control instruction introduces an implicit label. Labels are targets for branch instructions that
    // reference them with label indices. Indexing of labels is relative by nesting depth, that is, label 0 refers to the
    // innermost structured control instruction enclosing the referring branch instruction, while increasing indices refer
    // to those farther out.
    //   https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
    fn find(&self, label: &'s str) -> Option<u32> {
        self.stack.iter().rev().enumerate().find_map(|(i, l)| match l {
            Some(l) if *l == label => Some(i as u32),
            _ => None,
        })
    }
}

struct Context<'s> {
    source: &'s str,
    type_indices: Indices<'s>,
    func_indices: Indices<'s>,
    table_indices: Indices<'s>,
    mem_indices: Indices<'s>,
    global_indices: Indices<'s>,
    local_indices: Indices<'s>,
    next_local_idx: u32,
    label_stack: LabelStack<'s>,
    implicit_type_uses: Vec<u32>,
    types: Vec<wat::FuncType<'s>>,
}

impl<'s> Context<'s> {
    fn resolve_index(
        &self,
        indices: &Indices,
        idx: wat::Index<'s>,
        offset: usize,
        what: &'static str,
    ) -> Result<'s, u32> {
        match idx {
            wat::Index::Num(i) => Ok(i),
            wat::Index::Ident(id) => {
                if let Some(idx) = indices.get(id) {
                    Ok(*idx)
                } else {
                    Err(TransformError::new(
                        TransformErrorKind::IdIsNotDefined { id, what },
                        offset,
                        self.source,
                    ))
                }
            }
        }
    }

    fn resolve_type_idx(&mut self, ty: wat::TypeUse<'s>, offset: usize) -> Result<'s, u32> {
        match ty.idx {
            wat::TypeIndex::Explicit(idx) => {
                let idx = self.resolve_index(&self.type_indices, idx, offset, "type")?;
                if idx as usize >= self.types.len() {
                    return Ok(idx);
                }
                let wat::FuncType { params, results, .. } = &self.types[idx as usize];
                if ty.params.is_empty() && ty.results.is_empty()
                    || ty.params.iter().map(|p| p.ty).eq(params.iter().map(|p| p.ty))
                        && ty.results.iter().map(|r| r.ty).eq(results.iter().map(|r| r.ty))
                {
                    Ok(idx)
                } else {
                    Err(TransformError::new(
                        TransformErrorKind::FuncTypeMismatch {
                            left_params: ty.params.iter().map(|p| p.ty).collect(),
                            left_results: ty.results.iter().map(|p| p.ty).collect(),
                            right_params: params.iter().map(|p| p.ty).collect(),
                            right_results: results.iter().map(|p| p.ty).collect(),
                        },
                        offset,
                        self.source,
                    ))
                }
            }
            wat::TypeIndex::Implicit(idx) => Ok(self.implicit_type_uses[idx as usize]),
        }
    }

    fn resolve_func_idx(&self, idx: wat::Index<'s>, offset: usize) -> Result<'s, u32> {
        self.resolve_index(&self.func_indices, idx, offset, "function")
    }

    fn resolve_table_idx(&self, idx: wat::Index<'s>, offset: usize) -> Result<'s, u32> {
        self.resolve_index(&self.table_indices, idx, offset, "table")
    }

    fn resolve_mem_idx(&self, idx: wat::Index<'s>, offset: usize) -> Result<'s, u32> {
        self.resolve_index(&self.mem_indices, idx, offset, "memory")
    }

    fn resolve_global_idx(&self, idx: wat::Index<'s>, offset: usize) -> Result<'s, u32> {
        self.resolve_index(&self.global_indices, idx, offset, "global")
    }

    fn start_func_scope(&mut self) {
        self.next_local_idx = 0;
        self.local_indices.clear();
    }

    fn new_local_idx(&mut self, id: Option<&'s str>, offset: usize) -> Result<'s, u32> {
        let idx = self.next_local_idx;
        if let Some(id) = id {
            if let Some(idx) = self.local_indices.insert(id, idx) {
                return Err(TransformError::new(
                    TransformErrorKind::IdAlreadyDefined {
                        id,
                        idx,
                        what: "local variable",
                    },
                    offset,
                    self.source,
                ));
            }
        }
        self.next_local_idx += 1;
        Ok(idx)
    }

    fn resolve_local_idx(&self, idx: wat::Index<'s>, offset: usize) -> Result<'s, u32> {
        self.resolve_index(&self.local_indices, idx, offset, "local variable")
    }
}

pub fn wat2wasm<'s>(mut parsed: wat::Parsed<'s>, source: &'s str) -> Result<'s, wasm::Root<'s, TextSource<'s>>> {
    let mut ctx = Context {
        source,
        type_indices: parsed.type_indices,
        func_indices: parsed.func_indices,
        table_indices: parsed.table_indices,
        mem_indices: parsed.mem_indices,
        global_indices: parsed.global_indices,
        local_indices: Indices::new(),
        next_local_idx: 0,
        label_stack: LabelStack::new(source),
        implicit_type_uses: mem::take(&mut parsed.module.implicit_type_uses),
        types: Vec::new(),
    };
    let module = parsed.module.transform(&mut ctx)?;
    Ok(wasm::Root {
        module,
        source: TextSource(source),
    })
}

trait Transform<'s>: Sized {
    type Target;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target>;
}

impl<'s, T: Transform<'s>> Transform<'s> for Vec<T> {
    type Target = Vec<T::Target>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        self.into_iter().map(|n| n.transform(ctx)).collect()
    }
}

impl<'s, T: Transform<'s>> Transform<'s> for Option<T> {
    type Target = Option<T::Target>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        self.map(|n| n.transform(ctx)).transpose()
    }
}

impl<'s> Transform<'s> for wat::Module<'s> {
    type Target = wasm::Module<'s>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        ctx.types.reserve(self.types.len());
        for type_def in self.types.iter() {
            ctx.types.push(type_def.ty.clone());
        }
        Ok(wasm::Module {
            start: self.start,
            id: self.id,
            types: self.types.transform(ctx)?,
            exports: self.exports.transform(ctx)?,
            funcs: self.funcs.transform(ctx)?,
            elems: self.elems.transform(ctx)?,
            tables: self.tables.transform(ctx)?,
            data: self.data.transform(ctx)?,
            memories: self.memories.transform(ctx)?,
            globals: self.globals.transform(ctx)?,
            entrypoint: self.entrypoint.transform(ctx)?,
        })
    }
}

impl<'s> Transform<'s> for wat::ValType {
    type Target = wasm::ValType;
    fn transform(self, _ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(match self {
            wat::ValType::I32 => wasm::ValType::I32,
            wat::ValType::I64 => wasm::ValType::I64,
            wat::ValType::F32 => wasm::ValType::F32,
            wat::ValType::F64 => wasm::ValType::F64,
        })
    }
}

impl<'s> Transform<'s> for wat::TypeDef<'s> {
    type Target = wasm::FuncType;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::FuncType {
            start: self.start,
            params: self
                .ty
                .params
                .iter()
                .map(|p| p.ty.transform(ctx))
                .collect::<Result<'_, _>>()?,
            results: self
                .ty
                .results
                .iter()
                .map(|p| p.ty.transform(ctx))
                .collect::<Result<'_, _>>()?,
        })
    }
}

impl<'s> Transform<'s> for wat::Name<'s> {
    type Target = wasm::Name<'s>;
    fn transform(self, _ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::Name(self.0))
    }
}

impl<'s> Transform<'s> for wat::Export<'s> {
    type Target = wasm::Export<'s>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        let start = self.start;
        Ok(wasm::Export {
            start,
            name: self.name.transform(ctx)?,
            kind: match self.kind {
                wat::ExportKind::Func => {
                    let idx = ctx.resolve_func_idx(self.idx, start)?;
                    wasm::ExportKind::Func(idx)
                }
                wat::ExportKind::Table => {
                    let idx = ctx.resolve_table_idx(self.idx, start)?;
                    wasm::ExportKind::Table(idx)
                }
                wat::ExportKind::Memory => {
                    let idx = ctx.resolve_mem_idx(self.idx, start)?;
                    wasm::ExportKind::Memory(idx)
                }
                wat::ExportKind::Global => {
                    let idx = ctx.resolve_global_idx(self.idx, start)?;
                    wasm::ExportKind::Global(idx)
                }
            },
        })
    }
}

impl<'s> Transform<'s> for wat::Import<'s> {
    type Target = wasm::Import<'s>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::Import {
            mod_name: self.mod_name.transform(ctx)?,
            name: self.name.transform(ctx)?,
        })
    }
}

impl<'s> Transform<'s> for wat::Mem {
    type Target = wasm::Mem;
    fn transform(self, _ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::Mem {
            align: self.align,
            offset: self.offset,
        })
    }
}

impl<'s> Transform<'s> for wat::Instruction<'s> {
    type Target = wasm::Instruction;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        let start = self.start;
        let kind = match self.kind {
            wat::InsnKind::Block { label, ty, body, id } => {
                ctx.label_stack.push(label, id, start)?;
                let body = body.transform(ctx)?;
                ctx.label_stack.pop();
                wasm::InsnKind::Block {
                    ty: ty.transform(ctx)?,
                    body,
                }
            }
            wat::InsnKind::Loop { label, ty, body, id } => {
                ctx.label_stack.push(label, id, start)?;
                let body = body.transform(ctx)?;
                ctx.label_stack.pop();
                wasm::InsnKind::Loop {
                    ty: ty.transform(ctx)?,
                    body,
                }
            }
            wat::InsnKind::If {
                label,
                ty,
                then_body,
                else_id,
                else_body,
                end_id,
            } => {
                ctx.label_stack.push(label, else_id, start)?;
                let then_body = then_body.transform(ctx)?;
                ctx.label_stack.pop();
                ctx.label_stack.push(label, end_id, start)?;
                let else_body = else_body.transform(ctx)?;
                ctx.label_stack.pop();
                wasm::InsnKind::If {
                    ty: ty.transform(ctx)?,
                    then_body,
                    else_body,
                }
            }
            wat::InsnKind::Unreachable => wasm::InsnKind::Unreachable,
            wat::InsnKind::Nop => wasm::InsnKind::Nop,
            wat::InsnKind::Br(idx) => wasm::InsnKind::Br(ctx.label_stack.resolve(idx, start)?),
            wat::InsnKind::BrIf(idx) => wasm::InsnKind::BrIf(ctx.label_stack.resolve(idx, start)?),
            wat::InsnKind::BrTable { labels, default_label } => wasm::InsnKind::BrTable {
                labels: labels
                    .into_iter()
                    .map(|idx| ctx.label_stack.resolve(idx, start))
                    .collect::<Result<'_, _>>()?,
                default_label: ctx.label_stack.resolve(default_label, start)?,
            },
            wat::InsnKind::Return => wasm::InsnKind::Return,
            wat::InsnKind::Call(idx) => wasm::InsnKind::Call(ctx.resolve_func_idx(idx, start)?),
            wat::InsnKind::CallIndirect(ty) => wasm::InsnKind::CallIndirect(ctx.resolve_type_idx(ty, start)?),
            // Parametric instructions
            wat::InsnKind::Drop => wasm::InsnKind::Drop,
            wat::InsnKind::Select => wasm::InsnKind::Select,
            // Variable instructions
            wat::InsnKind::LocalGet(idx) => wasm::InsnKind::LocalGet(ctx.resolve_local_idx(idx, start)?),
            wat::InsnKind::LocalSet(idx) => wasm::InsnKind::LocalSet(ctx.resolve_local_idx(idx, start)?),
            wat::InsnKind::LocalTee(idx) => wasm::InsnKind::LocalTee(ctx.resolve_local_idx(idx, start)?),
            wat::InsnKind::GlobalGet(idx) => wasm::InsnKind::GlobalGet(ctx.resolve_global_idx(idx, start)?),
            wat::InsnKind::GlobalSet(idx) => wasm::InsnKind::GlobalSet(ctx.resolve_global_idx(idx, start)?),
            // Memory instructions
            wat::InsnKind::I32Load(mem) => wasm::InsnKind::I32Load(mem.transform(ctx)?),
            wat::InsnKind::I64Load(mem) => wasm::InsnKind::I64Load(mem.transform(ctx)?),
            wat::InsnKind::F32Load(mem) => wasm::InsnKind::F32Load(mem.transform(ctx)?),
            wat::InsnKind::F64Load(mem) => wasm::InsnKind::F64Load(mem.transform(ctx)?),
            wat::InsnKind::I32Load8S(mem) => wasm::InsnKind::I32Load8S(mem.transform(ctx)?),
            wat::InsnKind::I32Load8U(mem) => wasm::InsnKind::I32Load8U(mem.transform(ctx)?),
            wat::InsnKind::I32Load16S(mem) => wasm::InsnKind::I32Load16S(mem.transform(ctx)?),
            wat::InsnKind::I32Load16U(mem) => wasm::InsnKind::I32Load16U(mem.transform(ctx)?),
            wat::InsnKind::I64Load8S(mem) => wasm::InsnKind::I64Load8S(mem.transform(ctx)?),
            wat::InsnKind::I64Load8U(mem) => wasm::InsnKind::I64Load8U(mem.transform(ctx)?),
            wat::InsnKind::I64Load16S(mem) => wasm::InsnKind::I64Load16S(mem.transform(ctx)?),
            wat::InsnKind::I64Load16U(mem) => wasm::InsnKind::I64Load16U(mem.transform(ctx)?),
            wat::InsnKind::I64Load32S(mem) => wasm::InsnKind::I64Load32S(mem.transform(ctx)?),
            wat::InsnKind::I64Load32U(mem) => wasm::InsnKind::I64Load32U(mem.transform(ctx)?),
            wat::InsnKind::I32Store(mem) => wasm::InsnKind::I32Store(mem.transform(ctx)?),
            wat::InsnKind::I64Store(mem) => wasm::InsnKind::I64Store(mem.transform(ctx)?),
            wat::InsnKind::F32Store(mem) => wasm::InsnKind::F32Store(mem.transform(ctx)?),
            wat::InsnKind::F64Store(mem) => wasm::InsnKind::F64Store(mem.transform(ctx)?),
            wat::InsnKind::I32Store8(mem) => wasm::InsnKind::I32Store8(mem.transform(ctx)?),
            wat::InsnKind::I32Store16(mem) => wasm::InsnKind::I32Store16(mem.transform(ctx)?),
            wat::InsnKind::I64Store8(mem) => wasm::InsnKind::I64Store8(mem.transform(ctx)?),
            wat::InsnKind::I64Store16(mem) => wasm::InsnKind::I64Store16(mem.transform(ctx)?),
            wat::InsnKind::I64Store32(mem) => wasm::InsnKind::I64Store32(mem.transform(ctx)?),
            wat::InsnKind::MemorySize => wasm::InsnKind::MemorySize,
            wat::InsnKind::MemoryGrow => wasm::InsnKind::MemoryGrow,
            // Numeric instructions
            // https://webassembly.github.io/spec/core/text/instructions.html#numeric-instructions
            // Constants
            wat::InsnKind::I32Const(val) => wasm::InsnKind::I32Const(val),
            wat::InsnKind::I64Const(val) => wasm::InsnKind::I64Const(val),
            wat::InsnKind::F32Const(val) => wasm::InsnKind::F32Const(val),
            wat::InsnKind::F64Const(val) => wasm::InsnKind::F64Const(val),
            // i32 operations
            wat::InsnKind::I32Clz => wasm::InsnKind::I32Clz,
            wat::InsnKind::I32Ctz => wasm::InsnKind::I32Ctz,
            wat::InsnKind::I32Popcnt => wasm::InsnKind::I32Popcnt,
            wat::InsnKind::I32Add => wasm::InsnKind::I32Add,
            wat::InsnKind::I32Sub => wasm::InsnKind::I32Sub,
            wat::InsnKind::I32Mul => wasm::InsnKind::I32Mul,
            wat::InsnKind::I32DivS => wasm::InsnKind::I32DivS,
            wat::InsnKind::I32DivU => wasm::InsnKind::I32DivU,
            wat::InsnKind::I32RemS => wasm::InsnKind::I32RemS,
            wat::InsnKind::I32RemU => wasm::InsnKind::I32RemU,
            wat::InsnKind::I32And => wasm::InsnKind::I32And,
            wat::InsnKind::I32Or => wasm::InsnKind::I32Or,
            wat::InsnKind::I32Xor => wasm::InsnKind::I32Xor,
            wat::InsnKind::I32Shl => wasm::InsnKind::I32Shl,
            wat::InsnKind::I32ShrS => wasm::InsnKind::I32ShrS,
            wat::InsnKind::I32ShrU => wasm::InsnKind::I32ShrU,
            wat::InsnKind::I32Rotl => wasm::InsnKind::I32Rotl,
            wat::InsnKind::I32Rotr => wasm::InsnKind::I32Rotr,
            // i64 operations
            wat::InsnKind::I64Clz => wasm::InsnKind::I64Clz,
            wat::InsnKind::I64Ctz => wasm::InsnKind::I64Ctz,
            wat::InsnKind::I64Popcnt => wasm::InsnKind::I64Popcnt,
            wat::InsnKind::I64Add => wasm::InsnKind::I64Add,
            wat::InsnKind::I64Sub => wasm::InsnKind::I64Sub,
            wat::InsnKind::I64Mul => wasm::InsnKind::I64Mul,
            wat::InsnKind::I64DivS => wasm::InsnKind::I64DivS,
            wat::InsnKind::I64DivU => wasm::InsnKind::I64DivU,
            wat::InsnKind::I64RemS => wasm::InsnKind::I64RemS,
            wat::InsnKind::I64RemU => wasm::InsnKind::I64RemU,
            wat::InsnKind::I64And => wasm::InsnKind::I64And,
            wat::InsnKind::I64Or => wasm::InsnKind::I64Or,
            wat::InsnKind::I64Xor => wasm::InsnKind::I64Xor,
            wat::InsnKind::I64Shl => wasm::InsnKind::I64Shl,
            wat::InsnKind::I64ShrS => wasm::InsnKind::I64ShrS,
            wat::InsnKind::I64ShrU => wasm::InsnKind::I64ShrU,
            wat::InsnKind::I64Rotl => wasm::InsnKind::I64Rotl,
            wat::InsnKind::I64Rotr => wasm::InsnKind::I64Rotr,
            // f32 operations
            wat::InsnKind::F32Abs => wasm::InsnKind::F32Abs,
            wat::InsnKind::F32Neg => wasm::InsnKind::F32Neg,
            wat::InsnKind::F32Ceil => wasm::InsnKind::F32Ceil,
            wat::InsnKind::F32Floor => wasm::InsnKind::F32Floor,
            wat::InsnKind::F32Trunc => wasm::InsnKind::F32Trunc,
            wat::InsnKind::F32Nearest => wasm::InsnKind::F32Nearest,
            wat::InsnKind::F32Sqrt => wasm::InsnKind::F32Sqrt,
            wat::InsnKind::F32Add => wasm::InsnKind::F32Add,
            wat::InsnKind::F32Sub => wasm::InsnKind::F32Sub,
            wat::InsnKind::F32Mul => wasm::InsnKind::F32Mul,
            wat::InsnKind::F32Div => wasm::InsnKind::F32Div,
            wat::InsnKind::F32Min => wasm::InsnKind::F32Min,
            wat::InsnKind::F32Max => wasm::InsnKind::F32Max,
            wat::InsnKind::F32Copysign => wasm::InsnKind::F32Copysign,
            // f64 operations
            wat::InsnKind::F64Abs => wasm::InsnKind::F64Abs,
            wat::InsnKind::F64Neg => wasm::InsnKind::F64Neg,
            wat::InsnKind::F64Ceil => wasm::InsnKind::F64Ceil,
            wat::InsnKind::F64Floor => wasm::InsnKind::F64Floor,
            wat::InsnKind::F64Trunc => wasm::InsnKind::F64Trunc,
            wat::InsnKind::F64Nearest => wasm::InsnKind::F64Nearest,
            wat::InsnKind::F64Sqrt => wasm::InsnKind::F64Sqrt,
            wat::InsnKind::F64Add => wasm::InsnKind::F64Add,
            wat::InsnKind::F64Sub => wasm::InsnKind::F64Sub,
            wat::InsnKind::F64Mul => wasm::InsnKind::F64Mul,
            wat::InsnKind::F64Div => wasm::InsnKind::F64Div,
            wat::InsnKind::F64Min => wasm::InsnKind::F64Min,
            wat::InsnKind::F64Max => wasm::InsnKind::F64Max,
            wat::InsnKind::F64Copysign => wasm::InsnKind::F64Copysign,
            // i32 comparison
            wat::InsnKind::I32Eqz => wasm::InsnKind::I32Eqz,
            wat::InsnKind::I32Eq => wasm::InsnKind::I32Eq,
            wat::InsnKind::I32Ne => wasm::InsnKind::I32Ne,
            wat::InsnKind::I32LtS => wasm::InsnKind::I32LtS,
            wat::InsnKind::I32LtU => wasm::InsnKind::I32LtU,
            wat::InsnKind::I32GtS => wasm::InsnKind::I32GtS,
            wat::InsnKind::I32GtU => wasm::InsnKind::I32GtU,
            wat::InsnKind::I32LeS => wasm::InsnKind::I32LeS,
            wat::InsnKind::I32LeU => wasm::InsnKind::I32LeU,
            wat::InsnKind::I32GeS => wasm::InsnKind::I32GeS,
            wat::InsnKind::I32GeU => wasm::InsnKind::I32GeU,
            // i64 comparison
            wat::InsnKind::I64Eqz => wasm::InsnKind::I64Eqz,
            wat::InsnKind::I64Eq => wasm::InsnKind::I64Eq,
            wat::InsnKind::I64Ne => wasm::InsnKind::I64Ne,
            wat::InsnKind::I64LtS => wasm::InsnKind::I64LtS,
            wat::InsnKind::I64LtU => wasm::InsnKind::I64LtU,
            wat::InsnKind::I64GtS => wasm::InsnKind::I64GtS,
            wat::InsnKind::I64GtU => wasm::InsnKind::I64GtU,
            wat::InsnKind::I64LeS => wasm::InsnKind::I64LeS,
            wat::InsnKind::I64LeU => wasm::InsnKind::I64LeU,
            wat::InsnKind::I64GeS => wasm::InsnKind::I64GeS,
            wat::InsnKind::I64GeU => wasm::InsnKind::I64GeU,
            // f32 comparison
            wat::InsnKind::F32Eq => wasm::InsnKind::F32Eq,
            wat::InsnKind::F32Ne => wasm::InsnKind::F32Ne,
            wat::InsnKind::F32Lt => wasm::InsnKind::F32Lt,
            wat::InsnKind::F32Gt => wasm::InsnKind::F32Gt,
            wat::InsnKind::F32Le => wasm::InsnKind::F32Le,
            wat::InsnKind::F32Ge => wasm::InsnKind::F32Ge,
            // f64 comparison
            wat::InsnKind::F64Eq => wasm::InsnKind::F64Eq,
            wat::InsnKind::F64Ne => wasm::InsnKind::F64Ne,
            wat::InsnKind::F64Lt => wasm::InsnKind::F64Lt,
            wat::InsnKind::F64Gt => wasm::InsnKind::F64Gt,
            wat::InsnKind::F64Le => wasm::InsnKind::F64Le,
            wat::InsnKind::F64Ge => wasm::InsnKind::F64Ge,
            // Conversion
            wat::InsnKind::I32WrapI64 => wasm::InsnKind::I32WrapI64,
            wat::InsnKind::I32TruncF32S => wasm::InsnKind::I32TruncF32S,
            wat::InsnKind::I32TruncF32U => wasm::InsnKind::I32TruncF32U,
            wat::InsnKind::I32TruncF64S => wasm::InsnKind::I32TruncF64S,
            wat::InsnKind::I32TruncF64U => wasm::InsnKind::I32TruncF64U,
            wat::InsnKind::I64ExtendI32S => wasm::InsnKind::I64ExtendI32S,
            wat::InsnKind::I64ExtendI32U => wasm::InsnKind::I64ExtendI32U,
            wat::InsnKind::I64TruncF32S => wasm::InsnKind::I64TruncF32S,
            wat::InsnKind::I64TruncF32U => wasm::InsnKind::I64TruncF32U,
            wat::InsnKind::I64TruncF64S => wasm::InsnKind::I64TruncF64S,
            wat::InsnKind::I64TruncF64U => wasm::InsnKind::I64TruncF64U,
            wat::InsnKind::F32ConvertI32S => wasm::InsnKind::F32ConvertI32S,
            wat::InsnKind::F32ConvertI32U => wasm::InsnKind::F32ConvertI32U,
            wat::InsnKind::F32ConvertI64S => wasm::InsnKind::F32ConvertI64S,
            wat::InsnKind::F32ConvertI64U => wasm::InsnKind::F32ConvertI64U,
            wat::InsnKind::F32DemoteF64 => wasm::InsnKind::F32DemoteF64,
            wat::InsnKind::F64ConvertI32S => wasm::InsnKind::F64ConvertI32S,
            wat::InsnKind::F64ConvertI32U => wasm::InsnKind::F64ConvertI32U,
            wat::InsnKind::F64ConvertI64S => wasm::InsnKind::F64ConvertI64S,
            wat::InsnKind::F64ConvertI64U => wasm::InsnKind::F64ConvertI64U,
            wat::InsnKind::F64PromoteF32 => wasm::InsnKind::F64PromoteF32,
            wat::InsnKind::I32ReinterpretF32 => wasm::InsnKind::I32ReinterpretF32,
            wat::InsnKind::I64ReinterpretF64 => wasm::InsnKind::I64ReinterpretF64,
            wat::InsnKind::F32ReinterpretI32 => wasm::InsnKind::F32ReinterpretI32,
            wat::InsnKind::F64ReinterpretI64 => wasm::InsnKind::F64ReinterpretI64,
            // Sign extension
            wat::InsnKind::I32Extend8S => wasm::InsnKind::I32Extend8S,
            wat::InsnKind::I32Extend16S => wasm::InsnKind::I32Extend16S,
            wat::InsnKind::I64Extend8S => wasm::InsnKind::I64Extend8S,
            wat::InsnKind::I64Extend16S => wasm::InsnKind::I64Extend16S,
            wat::InsnKind::I64Extend32S => wasm::InsnKind::I64Extend32S,
        };
        Ok(wasm::Instruction { start, kind })
    }
}

impl<'s> Transform<'s> for wat::Func<'s> {
    type Target = wasm::Func<'s>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::Func {
            start: self.start,
            kind: match self.kind {
                wat::FuncKind::Import(import) => wasm::FuncKind::Import(import.transform(ctx)?),
                wat::FuncKind::Body { locals, body } => {
                    ctx.start_func_scope();
                    for param in self.ty.params.iter() {
                        ctx.new_local_idx(param.id, self.start)?;
                    }
                    for local in locals.iter() {
                        ctx.new_local_idx(local.id, self.start)?;
                    }

                    wasm::FuncKind::Body {
                        locals: locals.iter().map(|l| l.ty.transform(ctx)).collect::<Result<'_, _>>()?,
                        expr: body.transform(ctx)?,
                    }
                }
            },
            idx: ctx.resolve_type_idx(self.ty, self.start)?,
        })
    }
}

impl<'s> Transform<'s> for wat::Elem<'s> {
    type Target = wasm::ElemSegment;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        let start = self.start;
        Ok(wasm::ElemSegment {
            start,
            idx: ctx.resolve_table_idx(self.idx, start)?,
            offset: self.offset.transform(ctx)?,
            init: self
                .init
                .into_iter()
                .map(|idx| ctx.resolve_func_idx(idx, start))
                .collect::<Result<'_, _>>()?,
        })
    }
}

impl<'s> Transform<'s> for wat::Limits {
    type Target = wasm::Limits;
    fn transform(self, _ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(match self {
            wat::Limits::Range { min, max } => wasm::Limits::Range(min, max),
            wat::Limits::From { min } => wasm::Limits::From(min),
        })
    }
}

impl<'s> Transform<'s> for wat::Table<'s> {
    type Target = wasm::Table<'s>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::Table {
            start: self.start,
            ty: wasm::TableType {
                limit: self.ty.limit.transform(ctx)?,
            },
            import: self.import.transform(ctx)?,
        })
    }
}

impl<'s> Transform<'s> for wat::Data<'s> {
    type Target = wasm::DataSegment<'s>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::DataSegment {
            start: self.start,
            idx: ctx.resolve_mem_idx(self.idx, self.start)?,
            offset: self.offset.transform(ctx)?,
            data: self.data,
        })
    }
}

impl<'s> Transform<'s> for wat::Memory<'s> {
    type Target = wasm::Memory<'s>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::Memory {
            start: self.start,
            ty: wasm::MemType {
                limit: self.ty.limit.transform(ctx)?,
            },
            import: self.import.transform(ctx)?,
        })
    }
}

impl<'s> Transform<'s> for wat::Global<'s> {
    type Target = wasm::Global<'s>;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::Global {
            start: self.start,
            mutable: self.ty.mutable,
            ty: self.ty.ty.transform(ctx)?,
            kind: match self.kind {
                wat::GlobalKind::Import(import) => wasm::GlobalKind::Import(import.transform(ctx)?),
                wat::GlobalKind::Init(init) => wasm::GlobalKind::Init(init.transform(ctx)?),
            },
        })
    }
}

impl<'s> Transform<'s> for wat::Start<'s> {
    type Target = wasm::StartFunction;
    fn transform(self, ctx: &mut Context<'s>) -> Result<'s, Self::Target> {
        Ok(wasm::StartFunction {
            start: self.start,
            idx: ctx.resolve_func_idx(self.idx, self.start)?,
        })
    }
}
