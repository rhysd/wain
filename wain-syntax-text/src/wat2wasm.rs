use crate::ast as wat;
use crate::util::describe_position;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use wain_ast as wasm;

#[cfg_attr(test, derive(Debug))]
pub enum TransformErrorKind<'a> {
    IdIsNotDefined {
        id: &'a str,
        what: &'static str,
    },
    IdAlreadyDefined {
        id: &'a str,
        idx: u32,
        what: &'static str,
    },
    LabelAndIdMismatch {
        label: &'a str,
        id: &'a str,
    },
    ModulesNotComposable {
        dest_mod_id: Option<&'a str>,
        dest_mod_offset: usize,
        src_mod_id: Option<&'a str>,
        src_mod_offset: usize,
        msg: String,
    },
}

#[cfg_attr(test, derive(Debug))]
pub struct TransformError<'a> {
    kind: TransformErrorKind<'a>,
    offset: usize,
    source: &'a str,
}

impl<'a> TransformError<'a> {
    fn new(kind: TransformErrorKind<'a>, offset: usize, source: &'a str) -> Box<Self> {
        Box::new(TransformError {
            kind,
            offset,
            source,
        })
    }
}

impl<'a> fmt::Display for TransformError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TransformErrorKind::*;
        match &self.kind {
            IdIsNotDefined { id, what } => {
                write!(f, "no identifier '{}' for {} is found", id, what)?
            }
            IdAlreadyDefined { id, idx, what } => write!(
                f,
                "identifier '{}' is already defined to be mapped to index {} for {}",
                id, idx, what
            )?,
            LabelAndIdMismatch { label, id } => write!(
                f,
                "in control instruction, label '{}' and identifier '{}' must be the same",
                label, id
            )?,
            ModulesNotComposable {
                dest_mod_id,
                dest_mod_offset,
                src_mod_id,
                src_mod_offset,
                msg,
            } => {
                f.write_str("module ")?;
                if let Some(id) = src_mod_id {
                    write!(f, "'{}' ", id)?;
                }
                write!(
                    f,
                    "at offset {} cannot be merged into existing module ",
                    src_mod_offset
                )?;

                if let Some(id) = dest_mod_id {
                    write!(f, "'{}' ", id)?;
                }
                write!(f, "at offset {}: {}", dest_mod_offset, msg)?;
            }
        }

        describe_position(f, self.source, self.offset)
    }
}

type Result<'a, T> = ::std::result::Result<T, Box<TransformError<'a>>>;

struct Indices<'a> {
    map: HashMap<&'a str, u32>,
    base: u32,
    what: &'static str,
    source: &'a str,
}
impl<'a> Indices<'a> {
    fn new(map: HashMap<&'a str, u32>, base: usize, what: &'static str, source: &'a str) -> Self {
        Indices {
            map,
            base: base as u32, // pass usize for length of Vec
            what,
            source,
        }
    }

    fn resolve(&self, idx: wat::Index<'a>, offset: usize) -> Result<'a, u32> {
        match idx {
            wat::Index::Num(i) => Ok(i + self.base),
            wat::Index::Ident(id) => {
                if let Some(idx) = self.map.get(id) {
                    Ok(*idx + self.base)
                } else {
                    Err(TransformError::new(
                        TransformErrorKind::IdIsNotDefined {
                            id,
                            what: self.what,
                        },
                        offset,
                        self.source,
                    ))
                }
            }
        }
    }
}

struct LabelStack<'a> {
    source: &'a str,
    stack: Vec<Option<&'a str>>,
}

impl<'a> LabelStack<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            stack: vec![],
        }
    }

    fn resolve(&self, idx: wat::Index<'a>, offset: usize) -> Result<'a, u32> {
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

    fn push(
        &mut self,
        label: Option<&'a str>,
        id: Option<&'a str>,
        offset: usize,
    ) -> Result<'a, ()> {
        let label = match (label, id) {
            (Some(label), Some(id)) if label == id => label,
            (Some(label), Some(id)) => {
                return Err(TransformError::new(
                    TransformErrorKind::LabelAndIdMismatch { label, id },
                    offset,
                    self.source,
                ))
            }
            (Some(label), None) => label,
            (None, Some(id)) => id,
            (None, None) => {
                self.stack.push(None);
                return Ok(());
            }
        };

        if let Some(idx) = self.find(label) {
            return Err(TransformError::new(
                TransformErrorKind::IdAlreadyDefined {
                    id: label,
                    idx,
                    what: "label",
                },
                offset,
                self.source,
            ));
        }

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
    fn find(&self, label: &'a str) -> Option<u32> {
        self.stack
            .iter()
            .rev()
            .enumerate()
            .find_map(|(i, l)| match l {
                Some(l) if *l == label => Some(i as u32),
                _ => None,
            })
    }
}

struct Context<'a> {
    source: &'a str,
    type_indices: Indices<'a>,
    func_indices: Indices<'a>,
    table_indices: Indices<'a>,
    mem_indices: Indices<'a>,
    global_indices: Indices<'a>,
    local_indices: Indices<'a>,
    next_local_idx: u32,
    label_stack: LabelStack<'a>,
}

impl<'a> Context<'a> {
    fn start_func_scope(&mut self) {
        self.next_local_idx = 0;
        self.local_indices.map.clear();
    }

    fn new_local_idx(&mut self, id: Option<&'a str>, offset: usize) -> Result<'a, u32> {
        let idx = self.next_local_idx;
        if let Some(id) = id {
            if let Some(idx) = self.local_indices.map.insert(id, idx) {
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
}

pub fn from_wat<'a>(parsed: wat::Parsed<'a>, source: &'a str) -> Result<'a, wasm::Root<'a>> {
    let mut ctx = Context {
        source,
        type_indices: Indices::new(parsed.type_indices, 0, "type", source),
        func_indices: Indices::new(parsed.func_indices, 0, "function", source),
        table_indices: Indices::new(parsed.table_indices, 0, "table", source),
        mem_indices: Indices::new(parsed.mem_indices, 0, "memory", source),
        global_indices: Indices::new(parsed.global_indices, 0, "global", source),
        local_indices: Indices::new(HashMap::new(), 0, "local variable", source),
        next_local_idx: 0,
        label_stack: LabelStack::new(source),
    };
    let module = parsed.module.transform(&mut ctx)?;
    Ok(wasm::Root { module })
}

// Compose a new module into existing one with transform
pub fn compose<'a>(
    dest: &mut wasm::Module<'a>,
    parsed: wat::Parsed<'a>,
    source: &'a str,
) -> Result<'a, ()> {
    // https://webassembly.github.io/spec/core/text/modules.html#text-module
    //
    // The following restrictions are imposed on the composition of modules: `m1 ⊕ m2` is
    // defined if and only if
    //
    // - m1.start = ϵ ∨ m2.start = ϵ
    // - m1.funcs = m1.tables = m1.mems = m1.globals = ϵ ∨ m2.imports = ϵ
    //

    // Check m1.start = ϵ ∨ m2.start = ϵ
    if let (Some(s1), Some(s2)) = (&dest.entrypoint, &parsed.module.entrypoint) {
        let msg = format!(
            "start function can appear only once across modules: previous start function was defined at offset {}",
            s1.start
        );
        return Err(TransformError::new(
            TransformErrorKind::ModulesNotComposable {
                dest_mod_id: dest.id,
                dest_mod_offset: dest.start,
                src_mod_id: parsed.module.id,
                src_mod_offset: parsed.module.start,
                msg,
            },
            s2.start,
            source,
        ));
    }

    // Check m1.funcs = m1.tables = m1.mems = m1.globals = ϵ ∨ m2.imports = ϵ
    let module = parsed.module;
    let no_func_import =
        module.funcs.is_empty() || matches!(module.funcs[0].kind, wat::FuncKind::Body{..});
    let no_table_import = module.tables.is_empty() || module.tables[0].import.is_none();
    let no_mem_import = module.memories.is_empty() || module.memories[0].import.is_none();
    let no_global_import =
        module.globals.is_empty() || matches!(module.globals[0].kind, wat::GlobalKind::Init(_));
    let no_import = no_func_import && no_table_import && no_mem_import && no_global_import;
    if !(dest.funcs.is_empty()
        && dest.tables.is_empty()
        && dest.memories.is_empty()
        && dest.globals.is_empty()
        || no_import)
    {
        let msg = "when module M1 is merged into module M2, one of (1) or (2) must be met. \
                        (1) M1 has no 'import' section. \
                        (2) M2 has no 'func', 'table', 'memory' sections"
            .to_string();
        return Err(TransformError::new(
            TransformErrorKind::ModulesNotComposable {
                dest_mod_id: dest.id,
                dest_mod_offset: dest.start,
                src_mod_id: module.id,
                src_mod_offset: module.start,
                msg,
            },
            dest.start,
            source,
        ));
    }

    // Adjust fields of AST nodes for composing one module into another.
    // Indices of functions, tables, memories, globals and types are module local things. When two
    // modules are composed, these indices in the merged module need to be updated.
    // This visitor also checks the condition of composing two modules.
    let ctx = &mut Context {
        source,
        type_indices: Indices::new(parsed.type_indices, dest.types.len(), "type", source),
        func_indices: Indices::new(parsed.func_indices, dest.funcs.len(), "function", source),
        table_indices: Indices::new(parsed.table_indices, dest.tables.len(), "table", source),
        mem_indices: Indices::new(parsed.mem_indices, dest.memories.len(), "memory", source),
        global_indices: Indices::new(parsed.global_indices, dest.globals.len(), "global", source),
        local_indices: Indices::new(HashMap::new(), 0, "local variable", source),
        next_local_idx: 0,
        label_stack: LabelStack::new(source),
    };

    fn transform_append<'a, T: Transform<'a>>(
        dest: &mut Vec<T::Target>,
        src: Vec<T>,
        ctx: &mut Context<'a>,
    ) -> Result<'a, ()> {
        for item in src.into_iter() {
            dest.push(item.transform(ctx)?);
        }
        Ok(())
    }

    // Compose all module fields
    transform_append(&mut dest.types, module.types, ctx)?;
    transform_append(&mut dest.exports, module.exports, ctx)?;
    transform_append(&mut dest.funcs, module.funcs, ctx)?;
    transform_append(&mut dest.elems, module.elems, ctx)?;
    transform_append(&mut dest.tables, module.tables, ctx)?;
    transform_append(&mut dest.data, module.data, ctx)?;
    transform_append(&mut dest.memories, module.memories, ctx)?;
    transform_append(&mut dest.globals, module.globals, ctx)?;
    if let Some(start) = module.entrypoint.transform(ctx)? {
        dest.entrypoint = Some(start);
    }

    Ok(())
}

trait Transform<'a>: Sized {
    type Target;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target>;
}

impl<'a, T: Transform<'a>> Transform<'a> for Vec<T> {
    type Target = Vec<T::Target>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        self.into_iter().map(|n| n.transform(ctx)).collect()
    }
}

impl<'a, T: Transform<'a>> Transform<'a> for Option<T> {
    type Target = Option<T::Target>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        self.map(|n| n.transform(ctx)).transpose()
    }
}

impl<'a> Transform<'a> for wat::Module<'a> {
    type Target = wasm::Module<'a>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
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

impl<'a> Transform<'a> for wat::ValType {
    type Target = wasm::ValType;
    fn transform(self, _ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(match self {
            wat::ValType::I32 => wasm::ValType::I32,
            wat::ValType::I64 => wasm::ValType::I64,
            wat::ValType::F32 => wasm::ValType::F32,
            wat::ValType::F64 => wasm::ValType::F64,
        })
    }
}

impl<'a> Transform<'a> for wat::TypeDef<'a> {
    type Target = wasm::FuncType;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
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

impl<'a> Transform<'a> for wat::Name {
    type Target = wasm::Name<'a>;
    fn transform(self, _ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::Name(Cow::Owned(self.0)))
    }
}

impl<'a> Transform<'a> for wat::Export<'a> {
    type Target = wasm::Export<'a>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        let start = self.start;
        Ok(wasm::Export {
            start,
            name: self.name.transform(ctx)?,
            kind: match self.kind {
                wat::ExportKind::Func => {
                    let idx = ctx.func_indices.resolve(self.idx, start)?;
                    wasm::ExportKind::Func(idx)
                }
                wat::ExportKind::Table => {
                    let idx = ctx.table_indices.resolve(self.idx, start)?;
                    wasm::ExportKind::Table(idx)
                }
                wat::ExportKind::Memory => {
                    let idx = ctx.mem_indices.resolve(self.idx, start)?;
                    wasm::ExportKind::Memory(idx)
                }
                wat::ExportKind::Global => {
                    let idx = ctx.global_indices.resolve(self.idx, start)?;
                    wasm::ExportKind::Global(idx)
                }
            },
        })
    }
}

impl<'a> Transform<'a> for wat::Import {
    type Target = wasm::Import<'a>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::Import {
            mod_name: self.mod_name.transform(ctx)?,
            name: self.name.transform(ctx)?,
        })
    }
}

impl<'a> Transform<'a> for wat::Mem {
    type Target = wasm::Mem;
    fn transform(self, _ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::Mem {
            align: self.align,
            offset: self.offset,
        })
    }
}

impl<'a> Transform<'a> for wat::Instruction<'a> {
    type Target = wasm::Instruction;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        let start = self.start;
        let kind = match self.kind {
            wat::InsnKind::Block {
                label,
                ty,
                body,
                id,
            } => {
                ctx.label_stack.push(label, id, start)?;
                let body = body.transform(ctx)?;
                ctx.label_stack.pop();
                wasm::InsnKind::Block {
                    ty: ty.transform(ctx)?,
                    body,
                }
            }
            wat::InsnKind::Loop {
                label,
                ty,
                body,
                id,
            } => {
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
            wat::InsnKind::BrTable {
                labels,
                default_label,
            } => wasm::InsnKind::BrTable {
                labels: labels
                    .into_iter()
                    .map(|idx| ctx.label_stack.resolve(idx, start))
                    .collect::<Result<'_, _>>()?,
                default_label: ctx.label_stack.resolve(default_label, start)?,
            },
            wat::InsnKind::Return => wasm::InsnKind::Return,
            wat::InsnKind::Call(idx) => wasm::InsnKind::Call(ctx.func_indices.resolve(idx, start)?),
            wat::InsnKind::CallIndirect(ty) => {
                wasm::InsnKind::CallIndirect(ctx.type_indices.resolve(ty.idx, start)?)
            }
            // Parametric instructions
            wat::InsnKind::Drop => wasm::InsnKind::Drop,
            wat::InsnKind::Select => wasm::InsnKind::Select,
            // Variable instructions
            wat::InsnKind::LocalGet(idx) => {
                wasm::InsnKind::LocalGet(ctx.local_indices.resolve(idx, start)?)
            }
            wat::InsnKind::LocalSet(idx) => {
                wasm::InsnKind::LocalSet(ctx.local_indices.resolve(idx, start)?)
            }
            wat::InsnKind::LocalTee(idx) => {
                wasm::InsnKind::LocalTee(ctx.local_indices.resolve(idx, start)?)
            }
            wat::InsnKind::GlobalGet(idx) => {
                wasm::InsnKind::GlobalGet(ctx.global_indices.resolve(idx, start)?)
            }
            wat::InsnKind::GlobalSet(idx) => {
                wasm::InsnKind::GlobalSet(ctx.global_indices.resolve(idx, start)?)
            }
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
        };
        Ok(wasm::Instruction { start, kind })
    }
}

impl<'a> Transform<'a> for wat::Func<'a> {
    type Target = wasm::Func<'a>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::Func {
            start: self.start,
            idx: ctx.type_indices.resolve(self.ty.idx, self.start)?,
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
                        locals: locals
                            .iter()
                            .map(|l| l.ty.transform(ctx))
                            .collect::<Result<'_, _>>()?,
                        expr: body.transform(ctx)?,
                    }
                }
            },
        })
    }
}

impl<'a> Transform<'a> for wat::Elem<'a> {
    type Target = wasm::ElemSegment;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        let start = self.start;
        Ok(wasm::ElemSegment {
            start,
            idx: ctx.table_indices.resolve(self.idx, start)?,
            offset: self.offset.transform(ctx)?,
            init: self
                .init
                .into_iter()
                .map(|idx| ctx.func_indices.resolve(idx, start))
                .collect::<Result<'_, _>>()?,
        })
    }
}

impl<'a> Transform<'a> for wat::Limits {
    type Target = wasm::Limits;
    fn transform(self, _ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(match self {
            wat::Limits::Range { min, max } => wasm::Limits::Range(min, max),
            wat::Limits::From { min } => wasm::Limits::From(min),
        })
    }
}

impl<'a> Transform<'a> for wat::Table<'a> {
    type Target = wasm::Table<'a>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::Table {
            start: self.start,
            ty: wasm::TableType {
                limit: self.ty.limit.transform(ctx)?,
            },
            import: self.import.transform(ctx)?,
        })
    }
}

impl<'a> Transform<'a> for wat::Data<'a> {
    type Target = wasm::DataSegment<'a>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::DataSegment {
            start: self.start,
            idx: ctx.mem_indices.resolve(self.idx, self.start)?,
            offset: self.offset.transform(ctx)?,
            data: self.data,
        })
    }
}

impl<'a> Transform<'a> for wat::Memory<'a> {
    type Target = wasm::Memory<'a>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::Memory {
            start: self.start,
            ty: wasm::MemType {
                limit: self.ty.limit.transform(ctx)?,
            },
            import: self.import.transform(ctx)?,
        })
    }
}

impl<'a> Transform<'a> for wat::GlobalType {
    type Target = wasm::GlobalType;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::GlobalType {
            mutable: self.mutable,
            ty: self.ty.transform(ctx)?,
        })
    }
}

impl<'a> Transform<'a> for wat::Global<'a> {
    type Target = wasm::Global<'a>;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::Global {
            start: self.start,
            ty: self.ty.transform(ctx)?,
            kind: match self.kind {
                wat::GlobalKind::Import(import) => wasm::GlobalKind::Import(import.transform(ctx)?),
                wat::GlobalKind::Init(init) => wasm::GlobalKind::Init(init.transform(ctx)?),
            },
        })
    }
}

impl<'a> Transform<'a> for wat::Start<'a> {
    type Target = wasm::StartFunction;
    fn transform(self, ctx: &mut Context<'a>) -> Result<'a, Self::Target> {
        Ok(wasm::StartFunction {
            start: self.start,
            idx: ctx.func_indices.resolve(self.idx, self.start)?,
        })
    }
}
