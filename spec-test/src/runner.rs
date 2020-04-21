use crate::error::{Error, ErrorKind, Result, RunKind};
use crate::parser::Parser;
use crate::wast;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use wain_ast as ast;
use wain_exec::{DefaultImporter, Machine, Value};
use wain_syntax_binary as binary;
use wain_syntax_text as wat;
use wain_validate::validate;

const SKIPPED: &[&str] = &["linking.wast"];

#[cfg(not(windows))]
mod color {
    pub const RESET: &[u8] = b"\x1b[0m";
    pub const RED: &[u8] = b"\x1b[91m";
    pub const GREEN: &[u8] = b"\x1b[92m";
    pub const YELLOW: &[u8] = b"\x1b[93m";
    pub const BLUE: &[u8] = b"\x1b[94m";
}

#[cfg(windows)]
mod color {
    pub const RESET: &[u8] = b"";
    pub const RED: &[u8] = b"";
    pub const GREEN: &[u8] = b"";
    pub const YELLOW: &[u8] = b"";
    pub const BLUE: &[u8] = b"";
}

struct Discard;

impl io::Read for Discard {
    fn read(&mut self, _buf: &mut [u8]) -> io::Result<usize> {
        Ok(0)
    }
}

impl io::Write for Discard {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Ok(buf.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[derive(Default)]
pub struct Summary {
    total: u32,
    passed: u32,
    failed: u32,
    skipped: u32,
}

impl Summary {
    fn new(passed: u32, failed: u32, skipped: u32) -> Self {
        Self {
            total: passed + failed + skipped,
            passed,
            failed,
            skipped,
        }
    }

    fn println<W: Write>(&self, mut out: W) {
        writeln!(
            out,
            "  total: {}, passed: {}, failed: {}, skipped: {}",
            self.total, self.passed, self.failed, self.skipped
        )
        .unwrap();
    }

    fn merge(&mut self, other: &Summary) {
        self.total += other.total;
        self.passed += other.passed;
        self.failed += other.failed;
        self.skipped += other.skipped;
    }

    fn fail(&mut self) {
        self.total += 1;
        self.failed += 1;
    }

    fn pass(&mut self) {
        self.total += 1;
        self.passed += 1;
    }

    fn color(&self) -> &'static [u8] {
        if self.failed > 0 {
            color::RED
        } else if self.skipped > 0 {
            color::YELLOW
        } else {
            color::GREEN
        }
    }

    pub fn success(&self) -> bool {
        self.failed == 0
    }
}

// Test runner for one .wast file
pub struct Runner<W: Write> {
    out: W,
}

impl<W: Write> Runner<W> {
    pub fn new(out: W) -> Self {
        Runner { out }
    }

    fn report<D: fmt::Display>(&mut self, nth: usize, total: usize, err: D) {
        // Note: Path is included in error message
        self.out.write_all(color::RED).unwrap();
        write!(&mut self.out, "\n[{}/{}] ", nth, total).unwrap();
        self.out.write_all(color::RESET).unwrap();
        writeln!(&mut self.out, "{}", err).unwrap();
    }

    pub fn run_dir(&mut self, dir: &Path) -> io::Result<bool> {
        let mut total = Summary::default();
        let mut num_files = 0;

        let entries = fs::read_dir(dir)?;
        for entry in entries {
            let path = entry?.path();
            let file = if let Some(f) = path.file_name().and_then(|f| f.to_str()) {
                f
            } else {
                continue;
            };
            if !file.ends_with(".wast") {
                continue;
            }

            total.merge(&self.run_file(&path, file)?);
            num_files += 1;
        }

        self.out.write_all(total.color()).unwrap();
        writeln!(&mut self.out, "\nResults of {} files:", num_files).unwrap();
        total.println(&mut self.out);
        self.out.write_all(color::RESET).unwrap();
        Ok(total.failed == 0)
    }

    pub fn run_file(&mut self, path: &Path, file: &str) -> io::Result<Summary> {
        self.out.write_all(color::BLUE).unwrap();
        writeln!(&mut self.out, "\nStart: {:?}", path).unwrap();
        self.out.write_all(color::RESET).unwrap();

        let source = fs::read_to_string(&path)?;

        let sum = if file == "inline-module.wast" {
            // special case
            if let Err(err) = wat::parse(&source) {
                self.report(1, 1, err);
                Summary::new(0, 1, 0)
            } else {
                Summary::new(1, 0, 0)
            }
        } else {
            match Parser::new(&source).parse::<wast::Root<'_>>() {
                Err(err) => {
                    self.report(1, 1, err);
                    Summary::new(0, 1, 0)
                }
                Ok(root) if SKIPPED.contains(&file) => {
                    // All directives are skipped for this file
                    Summary::new(1, 0, root.directives.len() as u32)
                }
                Ok(root) => {
                    let mut tester = Tester {
                        sum: Summary::new(1, 0, 0),
                        errs: vec![],
                        source: &source,
                        root: &root,
                    };
                    tester.test();
                    let num_errs = tester.errs.len();
                    for (idx, err) in tester.errs.iter_mut().enumerate() {
                        let nth = idx + 1;
                        err.set_path(&path);
                        self.report(nth, num_errs, err);
                    }
                    tester.sum
                }
            }
        };

        let color = sum.color();
        self.out.write_all(color).unwrap();
        writeln!(&mut self.out, "\nEnd {:?}:", path).unwrap();
        sum.println(&mut self.out);
        self.out.write_all(color::RESET).unwrap();

        Ok(sum)
    }
}

type MachineForTest<'m, 's> = Machine<'m, 's, DefaultImporter<Discard, Discard>>;
type IndexToModule<'s> = HashMap<usize, (ast::Module<'s>, usize)>;

struct Instances<'mods, 'src: 'mods> {
    machines: Vec<(MachineForTest<'mods, 'src>, usize)>,
    idx_to_mod: &'mods IndexToModule<'src>,
    source: &'src str,
}

impl<'m, 's> Instances<'m, 's> {
    fn new(idx_to_mod: &'m IndexToModule<'s>, source: &'s str) -> Self {
        Instances {
            machines: vec![],
            idx_to_mod,
            source,
        }
    }

    fn new_machine(
        &self,
        m: &'m ast::Module<'s>,
        pos: usize,
    ) -> Result<'s, MachineForTest<'m, 's>> {
        let importer = DefaultImporter::with_stdio(Discard, Discard);
        let machine = Machine::instantiate(m, importer)
            .map_err(|err| Error::run_error(RunKind::Trapped(*err), self.source, pos))?;
        Ok(machine)
    }

    fn push_with_idx(&mut self, directive_idx: usize) -> Result<'s, ()> {
        let (module, pos) = self.idx_to_mod.get(&directive_idx).unwrap();
        self.push(module, *pos)
    }

    fn push(&mut self, module: &'m ast::Module<'s>, pos: usize) -> Result<'s, ()> {
        let machine = self.new_machine(module, pos)?;
        self.machines.push((machine, pos));
        Ok(())
    }

    fn find(
        &mut self,
        id: Option<&'s str>,
        pos: usize,
    ) -> Result<'s, (&mut MachineForTest<'m, 's>, usize)> {
        let searched = if let Some(id) = id {
            self.machines
                .iter_mut()
                .rev()
                .find(|(m, _)| m.module().id == Some(id))
        } else {
            self.machines.last_mut()
        };

        // Note: ok_or_else is not available due to nested borrow of `self`
        if let Some((m, mod_pos)) = searched {
            Ok((m, *mod_pos))
        } else {
            Err(Error::run_error(
                RunKind::ModuleNotFound(id),
                self.source,
                pos,
            ))
        }
    }

    fn invoke(&mut self, invoke: &wast::Invoke<'s>) -> Result<'s, Option<Value>> {
        let (machine, mod_pos) = self.find(invoke.id, invoke.start)?;

        let args: Box<[Value]> = invoke.args.iter().map(|c| c.to_value().unwrap()).collect();
        let ret = machine
            .invoke(&invoke.name, &args)
            .map_err(|err| Error::run_error(RunKind::Trapped(*err), self.source, mod_pos))?;

        Ok(ret)
    }
}

struct Tester<'a> {
    sum: Summary,
    errs: Vec<Error<'a>>,
    source: &'a str,
    root: &'a wast::Root<'a>,
}

impl<'a> Tester<'a> {
    fn check<T>(&mut self, res: Result<'a, T>) -> Option<T> {
        match res {
            Ok(x) => {
                self.sum.pass();
                Some(x)
            }
            Err(err) => {
                self.sum.fail();
                self.errs.push(*err);
                None
            }
        }
    }

    fn parse_embedded_module(
        &self,
        m: &'a wast::EmbeddedModule,
    ) -> Result<'a, (ast::Module<'a>, usize)> {
        match &m.src {
            wast::EmbeddedSrc::Quote(text) => {
                let root = wat::parse(text).map_err(|err| {
                    Error::run_error(RunKind::ParseQuoteFailure(err), self.source, m.start)
                })?;

                validate(&root).map_err(|err| {
                    Error::run_error(RunKind::InvalidText(*err), self.source, m.start)
                })?;

                Ok((root.module, m.start))
            }
            wast::EmbeddedSrc::Binary(bin) => {
                let root = binary::parse(bin).map_err(|err| {
                    Error::run_error(RunKind::ParseBinaryFailure(*err), self.source, m.start)
                })?;

                validate(&root).map_err(|err| {
                    Error::run_error(RunKind::InvalidBinary(*err), self.source, m.start)
                })?;

                Ok((root.module, m.start))
            }
        }
    }

    fn parse_embedded_modules(&mut self) -> IndexToModule<'a> {
        let mut mods = HashMap::new();
        for (idx, directive) in self.root.directives.iter().enumerate() {
            if let wast::Directive::EmbeddedModule(e) = directive {
                if let Some(m) = self.check(self.parse_embedded_module(e)) {
                    mods.insert(idx, m);
                }
            }
        }
        mods
    }

    fn test(&mut self) {
        // Parse and validate modules at first
        let idx_to_mod = self.parse_embedded_modules();

        // Give up assertions check when some module cannot be parsed because some target modules
        // for assertion don't exist
        if self.sum.failed > 0 {
            let num_directives = self.root.directives.len() as u32;
            let num_modules = idx_to_mod.len() as u32;
            let skipped = num_directives - num_modules - self.sum.failed;
            self.sum.total += skipped;
            self.sum.skipped += skipped;
            return;
        }

        let mut instances = Instances::new(&idx_to_mod, self.source);
        for (idx, directive) in self.root.directives.iter().enumerate() {
            let result = self.test_directive(idx, directive, &mut instances);
            self.check(result);
        }
    }

    fn test_directive<'m>(
        &self,
        idx: usize,
        directive: &'a wast::Directive<'a>,
        instances: &mut Instances<'m, 'a>,
    ) -> Result<'a, ()> {
        use wast::Directive::*;
        match directive {
            InlineModule(root) => instances.push(&root.module, root.module.start),
            EmbeddedModule(_) => instances.push_with_idx(idx),
            AssertReturn(wast::AssertReturn::Invoke {
                start,
                invoke,
                expected,
            }) => {
                let ret = instances.invoke(invoke)?;
                if let (Some(expected), Some(actual)) = (*expected, ret) {
                    if !expected.matches(&actual) {
                        return Err(Error::run_error(
                            RunKind::InvokeUnexpectedReturn { actual, expected },
                            self.source,
                            *start,
                        ));
                    }
                }
                Ok(())
            }
            AssertReturn(wast::AssertReturn::Global {
                start,
                get,
                expected,
            }) => {
                let (machine, _) = instances.find(get.id, *start)?;
                if let Some(actual) = machine.get_global(&get.name) {
                    if expected.matches(&actual) {
                        Ok(())
                    } else {
                        Err(Error::run_error(
                            RunKind::InvokeUnexpectedReturn {
                                actual,
                                expected: *expected,
                            },
                            self.source,
                            *start,
                        ))
                    }
                } else {
                    Err(Error::run_error(
                        RunKind::GlobalNotFound(get.name.clone()),
                        self.source,
                        *start,
                    ))
                }
            }
            AssertExhaustion(wast::AssertExhaustion {
                start,
                expected,
                invoke,
            })
            | AssertTrap(wast::AssertTrap {
                start,
                expected,
                pred: wast::TrapPredicate::Invoke(invoke),
            }) => {
                match instances.invoke(invoke) {
                    Ok(ret) => Err(Error::run_error(
                        RunKind::InvokeTrapExpected {
                            ret,
                            expected: expected.clone(),
                        },
                        self.source,
                        *start,
                    )),
                    Err(err) if matches!(err.kind(), ErrorKind::Run(RunKind::Trapped(_trap))) => {
                        // Expected path. Execution was trapped
                        //
                        // TODO: Check trap reason is what we expected.
                        // `expected` is an expected error message as string but we don't conform
                        // the message. So we need to have logic for mapping from expected message
                        // to our error.
                        Ok(())
                    }
                    Err(err) => Err(err),
                }
            }
            AssertTrap(wast::AssertTrap {
                start,
                expected,
                pred: wast::TrapPredicate::Module(root),
            }) => {
                validate(root)?;
                let importer = DefaultImporter::with_stdio(Discard, Discard);
                let mut machine = Machine::instantiate(&root.module, importer).map_err(|err| {
                    Error::run_error(RunKind::Trapped(*err), self.source, root.module.start)
                })?;
                match machine.execute() {
                    Ok(_) => Err(Error::run_error(
                        RunKind::InvokeTrapExpected {
                            ret: None,
                            expected: expected.clone(),
                        },
                        self.source,
                        *start,
                    )),
                    Err(_trap) => {
                        // TODO: Check trap reason is what we expected.
                        // `expected` is an expected error message as string but we don't conform
                        // the message. So we need to have logic for mapping from expected message
                        // to our error.
                        Ok(())
                    }
                }
            }
            AssertInvalid(wast::AssertInvalid {
                start,
                wat,
                expected,
            }) => match validate(wat) {
                Ok(()) => Err(Error::run_error(
                    RunKind::UnexpectedValid {
                        expected: expected.clone(),
                    },
                    self.source,
                    *start,
                )),
                Err(_err) => {
                    // TODO: Check validation error is what we expected.
                    // `expected` is an expected error message as string but we don't conform the
                    // message. We need to have logic for mapping from the expected message to our error.
                    Ok(())
                }
            },
            AssertMalformed(wast::AssertMalformed {
                start,
                module,
                expected,
            }) => {
                let success = match &module.src {
                    wast::EmbeddedSrc::Quote(text) => match wat::parse(text) {
                        Ok(_) => true,
                        Err(_err) => {
                            // TODO: Check parse error is what we expected.
                            // `expected` is an expected error message as string but we don't conform
                            // the message. We need to have logic for mapping from the expected message
                            // to our error.
                            false
                        }
                    },
                    wast::EmbeddedSrc::Binary(bin) => match binary::parse(bin) {
                        Ok(_) => true,
                        Err(_err) => {
                            // TODO: Check parse error is what we expected.
                            // `expected` is an expected error message as string but we don't conform
                            // the message. We need to have logic for mapping from the expected message
                            // to our error.
                            false
                        }
                    },
                };

                if success {
                    Err(Error::run_error(
                        RunKind::ExpectedParseError {
                            expected: expected.clone(),
                        },
                        self.source,
                        *start,
                    ))
                } else {
                    Ok(())
                }
            }
            Register(wast::Register { start, .. })
            | AssertUnlinkable(wast::AssertUnlinkable { start, .. }) => Err(Error::run_error(
                RunKind::NotImplementedYet,
                self.source,
                *start,
            )),
            Invoke(invoke) => instances.invoke(invoke).map(|_| ()),
        }
    }
}
