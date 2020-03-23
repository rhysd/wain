use crate::globals::Globals;
use crate::trap::{Result, Trap, TrapReason};
use wain_ast as ast;

const PAGE_SIZE: usize = 65536; // 64Ki

// Memory instance
pub struct Memory {
    max: Option<u32>,
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
                    ast::Limits::Range(min, max) => (*min, Some(*max)),
                    ast::Limits::From(min) => (*min, None),
                };
                let data = if min == 0 {
                    vec![]
                } else {
                    let len = (min as usize) * PAGE_SIZE;
                    let mut v = Vec::with_capacity(len);
                    v.resize(len, 0);
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

    // 10. and 14. https://webassembly.github.io/spec/core/exec/modules.html#instantiation
    pub fn new_data(&mut self, segment: &ast::DataSegment, globals: &Globals) -> Result<()> {
        // By validation of constant expression, at least one instruction in the sequence is guaranteed
        // and type must be i32
        let offset = match &segment.offset[segment.offset.len() - 1].kind {
            ast::InsnKind::GlobalGet(idx) => globals.get(*idx),
            ast::InsnKind::I32Const(i) => *i,
            _ => unreachable!("unexpected instruction for element offset"),
        };
        let offset = offset as usize;
        let data = &segment.data;
        let end_addr = offset + data.len();

        if let Some(max) = self.max {
            let max = max as usize;
            if end_addr > max * PAGE_SIZE {
                return Err(Trap::new(
                    TrapReason::OutOfLimit {
                        max,
                        idx: end_addr,
                        kind: "data element",
                    },
                    segment.start,
                ));
            }
        }

        if self.data.len() <= end_addr {
            return Err(Trap::new(
                TrapReason::DataSegmentOutOfBuffer {
                    segment_end: end_addr,
                    buffer_size: self.data.len(),
                },
                segment.start,
            ));
        }

        self.data[offset..end_addr].copy_from_slice(&segment.data);

        Ok(())
    }

    pub fn size(&self) -> u32 {
        (self.data.len() / PAGE_SIZE) as u32
    }

    pub fn grow(&mut self, num_pages: u32) -> i32 {
        // https://webassembly.github.io/spec/core/exec/instructions.html#exec-memory-grow
        let prev = self.size();
        let next = prev + num_pages;
        if let Some(max) = self.max {
            if next > max {
                return -1;
            }
        }
        let next_len = (next as usize) * PAGE_SIZE;
        self.data.resize(next_len, 0);
        prev as i32
    }
}
