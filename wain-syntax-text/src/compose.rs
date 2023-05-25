// https://webassembly.github.io/spec/core/text/modules.html#text-module
//
// The following restrictions are imposed on the composition of modules: `m1 ⊕ m2` is
// defined if and only if
//
// - m1.start = ϵ ∨ m2.start = ϵ
// - m1.funcs = m1.tables = m1.mems = m1.globals = ϵ ∨ m2.imports = ϵ
//
use crate::source::describe_position;
use std::fmt;
use wain_ast::*;

pub struct ComposeError<'source> {
    dest_mod_id: Option<&'source str>,
    dest_mod_offset: usize,
    src_mod_id: Option<&'source str>,
    src_mod_offset: usize,
    offset: usize,
    source: &'source str,
    msg: String,
}

impl<'s> ComposeError<'s> {
    pub fn source(&self) -> &'s str {
        self.source
    }
    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl<'s> fmt::Display for ComposeError<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("module ")?;
        if let Some(id) = self.src_mod_id {
            write!(f, "'{}' ", id)?;
        }
        write!(
            f,
            "at offset {} cannot be merged into existing module ",
            self.src_mod_offset
        )?;

        if let Some(id) = self.dest_mod_id {
            write!(f, "'{}' ", id)?;
        }
        write!(f, "at offset {}: {}", self.dest_mod_offset, self.msg)?;

        describe_position(f, self.source, self.offset)
    }
}

type Result<'s, T> = ::std::result::Result<T, Box<ComposeError<'s>>>;

pub(crate) struct Composer<'source> {
    source: &'source str,
    target: Module<'source>,
    composed_mod_id: Option<&'source str>,
    composed_mod_offset: usize,
    composing_imports: bool,
}

impl<'s> Composer<'s> {
    pub(crate) fn new(target: Module<'s>, source: &'s str) -> Self {
        Composer {
            source,
            target,
            composed_mod_id: None,
            composed_mod_offset: Default::default(),
            composing_imports: false,
        }
    }

    pub(crate) fn error<T>(&self, msg: String, offset: usize) -> Result<'s, T> {
        Err(Box::new(ComposeError {
            dest_mod_id: self.target.id,
            dest_mod_offset: self.target.start,
            src_mod_id: self.composed_mod_id,
            src_mod_offset: self.composed_mod_offset,
            offset,
            source: self.source,
            msg,
        }))
    }

    pub(crate) fn compose(mut self, mut composed: Module<'s>) -> Result<Module<'s>> {
        self.composed_mod_id = composed.id;
        self.composed_mod_offset = composed.start;

        // Check m1.start = ϵ ∨ m2.start = ϵ
        if let (Some(s1), Some(s2)) = (&self.target.entrypoint, &composed.entrypoint) {
            let msg = format!(
                "start function can appear only once across modules: previous start function was defined at offset {}",
                s1.start
            );
            return self.error(msg, s2.start);
        }

        // Adjust indices and check import exists
        composed.adjust(&mut self)?;

        // Check m1.funcs = m1.tables = m1.mems = m1.globals = ϵ ∨ m2.imports = ϵ
        if !(self.target.funcs.is_empty()
            && self.target.tables.is_empty()
            && self.target.memories.is_empty()
            && self.target.globals.is_empty()
            || !self.composing_imports)
        {
            let msg = "when module M1 is merged into module M2, one of (1) or (2) must be met. \
                        (1) M1 has no 'import' section. \
                        (2) M2 has no 'func', 'table', 'memory' sections"
                .to_string();
            return self.error(msg, self.target.start);
        }

        // Compose module fields
        self.target.types.append(&mut composed.types);
        self.target.exports.append(&mut composed.exports);
        self.target.funcs.append(&mut composed.funcs);
        self.target.elems.append(&mut composed.elems);
        self.target.tables.append(&mut composed.tables);
        self.target.data.append(&mut composed.data);
        self.target.memories.append(&mut composed.memories);
        self.target.globals.append(&mut composed.globals);
        if let Some(start) = composed.entrypoint {
            self.target.entrypoint = Some(start);
        }

        Ok(self.target)
    }

    fn saw_import(&mut self, has_import: bool) {
        self.composing_imports = self.composing_imports || has_import;
    }

    fn adjust_func_idx(&self, idx: &mut u32) {
        *idx += self.target.funcs.len() as u32;
    }

    fn adjust_type_idx(&self, idx: &mut u32) {
        *idx += self.target.types.len() as u32;
    }

    fn adjust_global_idx(&self, idx: &mut u32) {
        *idx += self.target.globals.len() as u32;
    }

    fn adjust_mem_idx(&self, idx: &mut u32) {
        *idx += self.target.memories.len() as u32;
    }

    fn adjust_table_idx(&self, idx: &mut u32) {
        *idx += self.target.tables.len() as u32;
    }
}

// Adjust fields of AST nodes for composing one module into another.
// Indices of functions, tables, memories, globals and types are module local things. When two modules
// are composed, these indices in the merged module need to be updated.
// This visitor also checks the condition of composing two modules.
trait Adjust<'s>: Sized {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()>;
}

impl<'s, A: Adjust<'s>> Adjust<'s> for Vec<A> {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        self.iter_mut().try_for_each(|a| a.adjust(composer))
    }
}

impl<'s, A: Adjust<'s>> Adjust<'s> for Option<A> {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        match self {
            Some(node) => node.adjust(composer),
            None => Ok(()),
        }
    }
}

impl<'s> Adjust<'s> for Module<'s> {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        self.entrypoint.adjust(composer)?;
        self.exports.adjust(composer)?;
        self.funcs.adjust(composer)?;
        self.elems.adjust(composer)?;
        for table in self.tables.iter() {
            composer.saw_import(table.import.is_some());
        }
        self.data.adjust(composer)?;
        self.memories.adjust(composer)?;
        self.globals.adjust(composer)?;
        Ok(())
    }
}

impl<'s> Adjust<'s> for StartFunction {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        composer.adjust_func_idx(&mut self.idx);
        Ok(())
    }
}

impl<'s> Adjust<'s> for Func<'s> {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        composer.adjust_type_idx(&mut self.idx);
        match &mut self.kind {
            FuncKind::Import(_) => composer.saw_import(true),
            FuncKind::Body { expr, .. } => expr.adjust(composer)?,
        }
        Ok(())
    }
}

impl<'s> Adjust<'s> for Instruction {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        use InsnKind::*;
        match &mut self.kind {
            Block { body, .. } => body.adjust(composer)?,
            Loop { body, .. } => body.adjust(composer)?,
            If {
                then_body, else_body, ..
            } => {
                then_body.adjust(composer)?;
                else_body.adjust(composer)?;
            }
            Call(idx) => composer.adjust_func_idx(idx),
            CallIndirect(idx) => composer.adjust_type_idx(idx),
            GlobalGet(idx) => composer.adjust_global_idx(idx),
            GlobalSet(idx) => composer.adjust_global_idx(idx),
            _ => {}
        }
        Ok(())
    }
}

impl<'s> Adjust<'s> for Export<'s> {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        match &mut self.kind {
            ExportKind::Func(idx) => composer.adjust_func_idx(idx),
            ExportKind::Table(idx) => composer.adjust_table_idx(idx),
            ExportKind::Memory(idx) => composer.adjust_mem_idx(idx),
            ExportKind::Global(idx) => composer.adjust_global_idx(idx),
        }
        Ok(())
    }
}

impl<'s> Adjust<'s> for ElemSegment {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        composer.adjust_table_idx(&mut self.idx);
        for idx in self.init.iter_mut() {
            composer.adjust_func_idx(idx);
        }
        self.offset.adjust(composer)
    }
}

impl<'s> Adjust<'s> for DataSegment<'s> {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        composer.adjust_mem_idx(&mut self.idx);
        self.offset.adjust(composer)
    }
}

impl<'s> Adjust<'s> for Memory<'s> {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        composer.saw_import(self.import.is_some());
        Ok(())
    }
}

impl<'s> Adjust<'s> for Global<'s> {
    fn adjust(&mut self, composer: &mut Composer) -> Result<'s, ()> {
        match &mut self.kind {
            GlobalKind::Import(_) => composer.saw_import(true),
            GlobalKind::Init(init) => init.adjust(composer)?,
        }
        Ok(())
    }
}
