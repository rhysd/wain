use crate::trap::{Result, Trap, TrapReason};
use crate::value::Value;
use std::convert::TryInto;
use wain_ast::{Global, GlobalKind, InsnKind};

// Fixed-size any values store indexed in advance
#[cfg_attr(test, derive(Debug))]
pub struct Globals {
    values: Box<[u8]>,
    offsets: Box<[u32]>,
}

impl Globals {
    // 5. https://webassembly.github.io/spec/core/exec/modules.html#instantiation
    pub fn instantiate<'a>(ast: &[Global<'a>]) -> Result<Self> {
        let mut offsets = Vec::with_capacity(ast.len());

        fn global_value<'a>(idx: usize, globals: &[Global<'a>]) -> Result<Value> {
            let g = &globals[idx];
            match &g.kind {
                GlobalKind::Import(i) => {
                    // Currently no global variable cannot be imported
                    Err(Trap::unknown_import(i, "global variable", g.start))
                }
                GlobalKind::Init(init) => {
                    // By validation at least one instruction is guaranteed to be put in `init`
                    match &init[init.len() - 1].kind {
                        InsnKind::GlobalGet(idx) => global_value(*idx as usize, globals),
                        InsnKind::I32Const(i) => Ok(Value::I32(*i)),
                        InsnKind::I64Const(i) => Ok(Value::I64(*i)),
                        InsnKind::F32Const(f) => Ok(Value::F32(*f)),
                        InsnKind::F64Const(f) => Ok(Value::F64(*f)),
                        _ => unreachable!("invalid instruction for constant"), // Never reach here thanks to validation
                    }
                }
            }
        }

        let mut idx = 0;
        let mut values = vec![];
        for i in 0..ast.len() {
            offsets.push(idx);
            // Do not allocate space for imported global variables. They are defined in other
            // module instances
            match global_value(i, ast)? {
                Value::I32(i) => {
                    values.extend_from_slice(&i.to_le_bytes());
                    idx += 4;
                }
                Value::I64(i) => {
                    values.extend_from_slice(&i.to_le_bytes());
                    idx += 8;
                }
                Value::F32(f) => {
                    values.extend_from_slice(&f.to_le_bytes());
                    idx += 4;
                }
                Value::F64(f) => {
                    values.extend_from_slice(&f.to_le_bytes());
                    idx += 8;
                }
            }
        }

        Ok(Globals {
            values: values.into_boxed_slice(),
            offsets: offsets.into_boxed_slice(),
        })
    }

    fn set_4_bytes(&mut self, idx: u32, bytes: [u8; 4]) {
        assert!((idx as usize) < self.offsets.len());
        let offset = self.offsets[idx as usize] as usize;
        for i in 0..4 {
            self.values[offset + i] = bytes[i];
        }
    }

    fn set_8_bytes(&mut self, idx: u32, bytes: [u8; 8]) {
        assert!((idx as usize) < self.offsets.len());
        let offset = self.offsets[idx as usize] as usize;
        for i in 0..8 {
            self.values[offset + i] = bytes[i];
        }
    }

    pub fn set_i32(&mut self, idx: u32, i: i32) {
        self.set_4_bytes(idx, i.to_le_bytes());
    }

    pub fn set_f32(&mut self, idx: u32, f: f32) {
        self.set_4_bytes(idx, f.to_le_bytes());
    }

    pub fn set_i64(&mut self, idx: u32, i: i64) {
        self.set_8_bytes(idx, i.to_le_bytes());
    }

    pub fn set_f64(&mut self, idx: u32, f: f64) {
        self.set_8_bytes(idx, f.to_le_bytes());
    }

    fn get_4_bytes(&self, idx: u32) -> [u8; 4] {
        assert!((idx as usize) < self.offsets.len());
        let offset = self.offsets[idx as usize] as usize;
        self.values[offset..offset + 4]
            .try_into()
            .expect("4 bytes for i32 or f32 variable")
    }

    fn get_8_bytes(&self, idx: u32) -> [u8; 8] {
        assert!((idx as usize) < self.offsets.len());
        let offset = self.offsets[idx as usize] as usize;
        self.values[offset..offset + 8]
            .try_into()
            .expect("4 bytes for i64 or f64 variable")
    }

    pub fn get_i32(&self, idx: u32) -> i32 {
        i32::from_le_bytes(self.get_4_bytes(idx))
    }

    pub fn get_f32(&self, idx: u32) -> f32 {
        f32::from_le_bytes(self.get_4_bytes(idx))
    }

    pub fn get_i64(&self, idx: u32) -> i64 {
        i64::from_le_bytes(self.get_8_bytes(idx))
    }

    pub fn get_f64(&self, idx: u32) -> f64 {
        f64::from_le_bytes(self.get_8_bytes(idx))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::borrow::Cow;
    use wain_ast::{Import, InsnKind, Instruction, Name, ValType};

    #[test]
    fn globals_ok() {
        let ast = [
            Global {
                start: 0,
                mutable: false,
                ty: ValType::I32,
                kind: GlobalKind::Init(vec![Instruction {
                    start: 0,
                    kind: InsnKind::I32Const(3),
                }]),
            },
            Global {
                start: 0,
                mutable: false,
                ty: ValType::I64,
                kind: GlobalKind::Init(vec![Instruction {
                    start: 0,
                    kind: InsnKind::I64Const(123456),
                }]),
            },
            Global {
                start: 0,
                mutable: false,
                ty: ValType::F32,
                kind: GlobalKind::Init(vec![Instruction {
                    start: 0,
                    kind: InsnKind::F32Const(3.14),
                }]),
            },
            Global {
                start: 0,
                mutable: false,
                ty: ValType::F64,
                kind: GlobalKind::Init(vec![Instruction {
                    start: 0,
                    kind: InsnKind::F64Const(54.3e21),
                }]),
            },
            Global {
                start: 0,
                mutable: false,
                ty: ValType::I32,
                kind: GlobalKind::Init(vec![Instruction {
                    start: 0,
                    kind: InsnKind::GlobalGet(0),
                }]),
            },
        ];
        let mut globals = Globals::instantiate(&ast).unwrap();

        assert_eq!(globals.get_i32(0), 3);
        assert_eq!(globals.get_i64(1), 123456);
        assert_eq!(globals.get_f32(2), 3.14);
        assert_eq!(globals.get_f64(3), 54.3e21);
        assert_eq!(globals.get_i32(4), 3);

        globals.set_i32(0, 42);
        globals.set_i64(1, 12345);
        globals.set_f32(2, 3.14);
        globals.set_f64(3, 12.3e10);

        assert_eq!(globals.get_i32(0), 42);
        assert_eq!(globals.get_i64(1), 12345);
        assert_eq!(globals.get_f32(2), 3.14);
        assert_eq!(globals.get_f64(3), 12.3e10);
        assert_eq!(globals.get_i32(4), 3);
    }

    #[test]
    fn globals_error() {
        fn import() -> Import<'static> {
            Import {
                mod_name: Name(Cow::Borrowed("module")),
                name: Name(Cow::Borrowed("name")),
            }
        }

        // For now all global variable imports cause an error
        let globals = [Global {
            start: 0,
            mutable: true,
            ty: ValType::I32,
            kind: GlobalKind::Import(import()),
        }];

        let err = Globals::instantiate(&globals).unwrap_err();
        assert!(matches!(err.reason, TrapReason::UnknownImport{..}));
    }
}
