use crate::globals::Globals;
use crate::trap::{Result, Trap, TrapReason};
use wain_ast as ast;

// Table instance
pub struct Table {
    max: Option<usize>,
    elems: Vec<u32>, // function indices
}

impl Table {
    // https://webassembly.github.io/spec/core/exec/modules.html#alloc-table
    pub fn allocate(tables: &[ast::Table]) -> Result<Self> {
        // Note: Only one table exists thanks to validation
        assert!(tables.len() <= 1);
        if let Some(table) = tables.get(0) {
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
                    v.resize(min as usize, u32::max_value());
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
            self.elems.resize(end_idx, u32::max_value());
        }

        self.elems[offset..end_idx].copy_from_slice(&elem.init);

        Ok(())
    }
}
