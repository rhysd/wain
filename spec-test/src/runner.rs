use crate::error::{Error, Result, RunKind};
use crate::parser::Parser;
use crate::wast;
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

mod color {
    pub const RESET: &[u8] = b"\x1b[0m";
    pub const RED: &[u8] = b"\x1b[91m";
    pub const GREEN: &[u8] = b"\x1b[92m";
    pub const YELLOW: &[u8] = b"\x1b[93m";
    pub const BLUE: &[u8] = b"\x1b[94m";
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

fn const_value(c: &wast::Const) -> Option<Value> {
    use wast::Const::*;
    match c {
        I32(i) => Some(Value::I32(*i)),
        I64(i) => Some(Value::I64(*i)),
        F32(f) => Some(Value::F32(*f)),
        F64(f) => Some(Value::F64(*f)),
        _ => None,
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

        self.out.write_all(color::BLUE).unwrap();
        writeln!(&mut self.out, "\nTotal Results of {} files:", num_files).unwrap();
        total.println(&mut self.out);
        self.out.write_all(color::RESET).unwrap();
        Ok(total.failed == 0)
    }

    pub fn run_file(&mut self, path: &Path, file: &str) -> io::Result<Summary> {
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
                Ok(_) if SKIPPED.contains(&file) => Summary::new(1, 0, 1),
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

        let color = if sum.failed == 0 {
            color::GREEN
        } else {
            color::YELLOW
        };
        self.out.write_all(color).unwrap();
        writeln!(&mut self.out, "\nResults of {:?}:", path).unwrap();
        sum.println(&mut self.out);
        self.out.write_all(color::RESET).unwrap();

        Ok(sum)
    }
}

struct KnownModules<'s> {
    mods: Vec<(ast::Module<'s>, usize)>,
    source: &'s str,
}

impl<'s> KnownModules<'s> {
    fn new(source: &'s str) -> Self {
        KnownModules {
            mods: vec![],
            source,
        }
    }
    fn push(&mut self, m: ast::Module<'s>, pos: usize) {
        self.mods.push((m, pos));
    }

    fn find(&self, id: Option<&'s str>, pos: usize) -> Result<'s, (&ast::Module<'s>, usize)> {
        let searched = if let Some(id) = id {
            self.mods.iter().rev().find(|(m, _)| m.id == Some(id))
        } else {
            self.mods.last()
        };
        let (m, pos) = searched
            .ok_or_else(|| Error::run_error(RunKind::ModuleNotFound(id), self.source, pos))?;
        Ok((m, *pos))
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

    fn test(&mut self) {
        let mut modules = KnownModules::new(self.source);
        for directive in self.root.directives.iter() {
            let result = self.test_directive(directive, &mut modules);
            self.check(result);
        }
    }

    fn test_directive(
        &mut self,
        directive: &'a wast::Directive<'a>,
        known: &mut KnownModules<'a>,
    ) -> Result<'a, ()> {
        use wast::Directive::*;
        match directive {
            EmbeddedModule(wast::EmbeddedModule {
                embedded: wast::Embedded::Quote(text),
                start,
            }) => {
                // Use inner module's source and offset
                let root = wat::parse(text)?;
                validate(&root)?;
                known.push(root.module, *start);
                Ok(())
            }
            EmbeddedModule(wast::EmbeddedModule {
                embedded: wast::Embedded::Binary(bin),
                start,
            }) => {
                // Since binary format parse error contains &[u8] as source, it is not available
                // for crate::error::Error.
                let root = binary::parse(bin).map_err(|err| {
                    Error::run_error(RunKind::ParseBinaryFailure(*err), self.source, *start)
                })?;

                validate(&root).map_err(|err| {
                    Error::run_error(RunKind::InvalidBinary(*err), self.source, *start)
                })?;

                known.push(root.module, *start);

                Ok(())
            }
            AssertReturn(wast::AssertReturn::Invoke {
                start,
                invoke,
                expected,
            }) => {
                let (module, mod_pos) = known.find(invoke.id, *start)?;
                let importer = DefaultImporter::with_stdio(Discard, Discard);
                let mut machine = Machine::instantiate(module, importer).map_err(|err| {
                    Error::run_error(RunKind::Trapped(*err), self.source, mod_pos)
                })?;

                let args: Box<[Value]> = invoke
                    .args
                    .iter()
                    .map(const_value)
                    .collect::<Option<_>>()
                    .unwrap();
                let ret = machine.invoke(&invoke.name, &args).map_err(|err| {
                    Error::run_error(RunKind::Trapped(*err), self.source, mod_pos)
                })?;

                if let (Some(expected), Some(actual)) = (*expected, ret) {
                    use wast::Const::*;
                    let ok = match &expected {
                        I32(_) | I64(_) | F32(_) | F64(_) => {
                            const_value(&expected).unwrap() == actual
                        }
                        // TODO: Check payload for arithmetic NaN
                        CanonicalNan | ArithmeticNan => match actual {
                            Value::F32(f) => f.is_nan(),
                            Value::F64(f) => f.is_nan(),
                            _ => false,
                        },
                    };

                    if !ok {
                        return Err(Error::run_error(
                            RunKind::InvokeUnexpectedReturn { actual, expected },
                            self.source,
                            *start,
                        ));
                    }
                }
                Ok(())
            }
            d => Err(Error::run_error(
                RunKind::NotImplementedYet,
                self.source,
                d.start_pos(),
            )),
        }
    }
}
