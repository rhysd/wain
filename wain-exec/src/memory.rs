use crate::globals::Globals;
use crate::trap::{Result, Trap, TrapReason};
use wain_ast as ast;

const PAGE_SIZE: usize = 65536; // 64Ki

fn page_size(addr: usize) -> usize {
    PAGE_SIZE * ((addr as usize) % PAGE_SIZE + 1)
}

// Memory instance
pub struct Memory {
    max: Option<usize>,
    data: Vec<u8>,
}

impl Memory {
    // https://webassembly.github.io/spec/core/exec/modules.html#alloc-mem
    pub fn allocate(memories: &[ast::Memory]) -> Result<Self> {
        // Note: Only one memory exists thanks to validation
        assert!(memories.len() <= 1);
        if let Some(memory) = memories.get(0) {
            if let Some(i) = &memory.import {
                Err(Trap::unknown_import(i, "memory", memory.start))
            } else {
                let (min, max) = match &memory.ty.limit {
                    ast::Limits::Range(min, max) => (*min, Some(*max as usize)),
                    ast::Limits::From(min) => (*min, None),
                };
                let data = if min == 0 {
                    vec![]
                } else {
                    let mut v = Vec::with_capacity(min as usize);
                    v.resize(page_size(min as usize), 0);
                    v
                };
                Ok(Self { max, data })
            }
        } else {
            // When no table is set use dummy empty table
            Ok(Self {
                max: Some(0),
                data: vec![],
            })
        }
    }

    // 10. and 14. https://webassembly.github.io/spec/core/exec/modules.html#allocation
    pub fn new_data(&mut self, segment: &ast::DataSegment, globals: &Globals) -> Result<()> {
        // By validation of constant expression, at least one instruction in the sequence is guaranteed
        // and type must be i32
        let offset = match &segment.offset[segment.offset.len() - 1].kind {
            ast::InsnKind::GlobalGet(idx) => globals.get_i32(*idx),
            ast::InsnKind::I32Const(i) => *i,
            _ => unreachable!("unexpected instruction for element offset"),
        };
        let offset = offset as usize;
        let data = &segment.data;
        let end_idx = page_size(offset + data.len());

        if let Some(max) = self.max {
            if end_idx > max {
                return Err(Trap::new(
                    TrapReason::OutOfLimit {
                        max,
                        idx: end_idx,
                        kind: "data element",
                    },
                    segment.start,
                ));
            }
        }

        if self.data.len() < end_idx {
            self.data.resize(end_idx, 0);
        }

        self.data[offset..end_idx].copy_from_slice(&segment.data);

        Ok(())
    }
}
