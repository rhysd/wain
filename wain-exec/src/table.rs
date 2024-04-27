use crate::globals::Globals;
use crate::trap::{Result, Trap, TrapReason};
use wain_ast as ast;

// Table instance
pub struct Table {
    max: Option<usize>,
    elems: Vec<Option<u32>>, // function indices
}

impl Table {
    // https://webassembly.github.io/spec/core/exec/modules.html#alloc-table
    pub fn allocate(tables: &[ast::Table]) -> Result<Self> {
        // Note: Only one table exists thanks to validation
        assert!(tables.len() <= 1);
        if let Some(table) = tables.first() {
            if let Some(i) = &table.import {
                Err(Trap::unknown_import(i, "table", table.start))
            } else {
                let (min, max) = match &table.ty.limit {
                    ast::Limits::Range(min, max) => (*min, Some(*max as usize)),
                    ast::Limits::From(min) => (*min, None),
                };
                let elems = if min == 0 {
                    vec![]
                } else {
                    let mut v = Vec::with_capacity(min as usize);
                    v.resize(min as usize, None);
                    v
                };
                Ok(Self { max, elems })
            }
        } else {
            // When no table is set use dummy empty table
            Ok(Self {
                max: Some(0),
                elems: vec![],
            })
        }
    }

    // 9. and 13. https://webassembly.github.io/spec/core/exec/modules.html#allocation
    pub fn new_elem(&mut self, elem: &ast::ElemSegment, globals: &Globals) -> Result<()> {
        // By validation of constant expression, at least one instruction in the sequence is guaranteed
        // and type must be i32
        let offset = match &elem.offset[elem.offset.len() - 1].kind {
            ast::InsnKind::GlobalGet(idx) => globals.get(*idx),
            ast::InsnKind::I32Const(i) => *i,
            _ => unreachable!("unexpected instruction for element offset"),
        };
        let offset = offset as usize;
        let end_idx = offset + elem.init.len();

        if let Some(max) = self.max {
            if end_idx > max {
                return Err(Trap::new(
                    TrapReason::OutOfLimit {
                        max,
                        idx: end_idx,
                        kind: "table element",
                    },
                    elem.start,
                ));
            }
        }

        if self.elems.len() < end_idx {
            return Err(Trap::new(
                TrapReason::ElemSegmentLargerThanTable {
                    segment_end: end_idx,
                    table_size: self.elems.len(),
                },
                elem.start,
            ));
        }

        for i in 0..elem.init.len() {
            self.elems[offset + i] = Some(elem.init[i]);
        }

        Ok(())
    }

    pub fn at(&self, idx: usize, source_offset: usize) -> Result<u32> {
        if idx >= self.elems.len() {
            return Err(Trap::new(
                TrapReason::IdxOutOfTable {
                    idx,
                    table_size: self.elems.len(),
                },
                source_offset,
            ));
        }

        if let Some(funcidx) = self.elems[idx] {
            Ok(funcidx)
        } else {
            Err(Trap::new(TrapReason::UninitializedElem(idx), source_offset))
        }
    }
}
