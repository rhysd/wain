use crate::crash::CrashTester;
use crate::error::{Error, ErrorKind, Result, RunKind};
use crate::importer::SpecTestImporter;
use crate::parser::Parser;
use crate::wast;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::time;
use wain_ast as ast;
use wain_exec::{Runtime, Value};
use wain_syntax_binary as binary;
use wain_syntax_text as wat;
use wain_validate::validate;

#[cfg(not(target_os = "linux"))]
const SKIPPED: &[&str] = &["linking.wast"];

// Note: skip-stack-guard-page.wast is not runnable on Linux because there is a restriction in
// length of command line arguments on Linux.
// TODO: Instead of passing values via command line arguments, we should pass them via a text file
// to avoid this restriction.
#[cfg(target_os = "linux")]
const SKIPPED: &[&str] = &["linking.wast", "skip-stack-guard-page.wast"];

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

    pub fn failures(&self) -> u32 {
        self.failed
    }
}

// Test runner for one .wast file
pub struct Runner<W: Write> {
    out: W,
    summary_file: Option<PathBuf>,
    crasher: CrashTester,
    fast_fail: bool,
}

impl<W: Write> Runner<W> {
    pub fn new(mut out: W, fast_fail: bool, summary_file: Option<PathBuf>) -> Self {
        write!(&mut out, "Building crash-tester...").unwrap();
        let crasher = CrashTester::new();
        writeln!(&mut out, "done").unwrap();

        Runner {
            out,
            summary_file,
            crasher,
            fast_fail,
        }
    }

    fn report<D: fmt::Display>(&mut self, nth: usize, total: usize, err: D) {
        // Note: Path is included in error message
        self.out.write_all(color::RED).unwrap();
        write!(&mut self.out, "\n[{}/{}] ", nth, total).unwrap();
        self.out.write_all(color::RESET).unwrap();
        writeln!(&mut self.out, "{}", err).unwrap();
    }

    fn save_summaries(&mut self, summaries: &mut [(String, Summary)], total: &Summary) -> io::Result<()> {
        let mut file = if let Some(f) = &self.summary_file {
            fs::File::create(f)?
        } else {
            return Ok(());
        };

        summaries.sort_by(|(f1, _), (f2, _)| f1.cmp(f2));

        for (f, sum) in summaries.iter() {
            write!(&mut file, "{} -> ", f)?;
            sum.println(&mut file);
        }

        writeln!(&mut file)?;
        write!(&mut file, "TOTAL -> ")?;
        total.println(&mut file);

        Ok(())
    }

    pub fn run_dir(&mut self, dir: &Path) -> io::Result<Summary> {
        let mut total = Summary::default();
        let mut num_files = 0;
        let mut summaries = vec![];
        let start_time = time::SystemTime::now();

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

            let sum = self.run_file(&path, file)?;
            if self.fast_fail && !sum.success() {
                break;
            }
            total.merge(&sum);
            if self.summary_file.is_some() {
                summaries.push((file.to_string(), sum));
            }
            num_files += 1;
        }

        let msecs = start_time.elapsed().unwrap().as_millis();
        self.out.write_all(total.color()).unwrap();
        writeln!(&mut self.out, "\nResults of {} files ({} ms):", num_files, msecs,).unwrap();
        total.println(&mut self.out);
        self.out.write_all(color::RESET).unwrap();
        self.save_summaries(&mut summaries, &total)?;
        Ok(total)
    }

    pub fn run_file(&mut self, path: &Path, file: &str) -> io::Result<Summary> {
        self.out.write_all(color::BLUE).unwrap();
        writeln!(&mut self.out, "\nStart: {:?}", path).unwrap();
        self.out.write_all(color::RESET).unwrap();

        let source = fs::read_to_string(path)?;
        let start_time = time::SystemTime::now();

        let sum = if file == "inline-module.wast" {
            // special case
            if let Err(err) = wat::parse(&source) {
                self.report(1, 1, err);
                Summary::new(0, 1, 0)
            } else {
                Summary::new(1, 0, 0)
            }
        } else {
            match Parser::new(&source).parse::<wast::Script<'_>>() {
                Err(err) => {
                    self.report(1, 1, err);
                    Summary::new(0, 1, 0)
                }
                Ok(script) if SKIPPED.contains(&file) => {
                    // All commands are skipped for this file
                    Summary::new(1, 0, script.commands.len() as u32)
                }
                Ok(script) => {
                    let mut tester = Tester {
                        sum: Summary::new(1, 0, 0),
                        errs: vec![],
                        source: &source,
                        script: &script,
                    };
                    tester.test(&self.crasher);
                    let num_errs = tester.errs.len();
                    for (idx, err) in tester.errs.iter_mut().enumerate() {
                        let nth = idx + 1;
                        err.set_path(path);
                        self.report(nth, num_errs, err);
                    }
                    tester.sum
                }
            }
        };

        let msecs = start_time.elapsed().unwrap().as_millis();
        let color = sum.color();
        self.out.write_all(color).unwrap();
        writeln!(&mut self.out, "\nEnd {:?} ({} ms):", path, msecs).unwrap();
        sum.println(&mut self.out);
        self.out.write_all(color::RESET).unwrap();

        Ok(sum)
    }
}

type TestRuntime<'m, 's> = Runtime<'m, 's, SpecTestImporter>;
type IndexToModule<'s> = HashMap<usize, (ast::Module<'s>, usize)>;

struct Instances<'mods, 'src> {
    runtimes: Vec<(TestRuntime<'mods, 'src>, usize)>,
    idx_to_mod: &'mods IndexToModule<'src>,
    source: &'src str,
}

impl<'m, 's> Instances<'m, 's> {
    fn new(idx_to_mod: &'m IndexToModule<'s>, source: &'s str) -> Self {
        Instances {
            runtimes: vec![],
            idx_to_mod,
            source,
        }
    }

    fn new_runtime(&self, m: &'m ast::Module<'s>, pos: usize) -> Result<'s, TestRuntime<'m, 's>> {
        let runtime = Runtime::instantiate(m, SpecTestImporter)
            .map_err(|err| Error::run_error(RunKind::Trapped(*err), self.source, pos))?;
        Ok(runtime)
    }

    fn push_with_idx(&mut self, cmd_idx: usize) -> Result<'s, ()> {
        let (module, pos) = self.idx_to_mod.get(&cmd_idx).unwrap();
        self.push(module, *pos)
    }

    fn push(&mut self, module: &'m ast::Module<'s>, pos: usize) -> Result<'s, ()> {
        let runtime = self.new_runtime(module, pos)?;
        self.runtimes.push((runtime, pos));
        Ok(())
    }

    fn find(&mut self, id: Option<&'s str>, pos: usize) -> Result<'s, (&mut TestRuntime<'m, 's>, usize)> {
        let searched = if let Some(id) = id {
            self.runtimes.iter_mut().rev().find(|(m, _)| m.module().id == Some(id))
        } else {
            self.runtimes.last_mut()
        };

        // Note: ok_or_else is not available due to nested borrow of `self`
        if let Some((m, mod_pos)) = searched {
            Ok((m, *mod_pos))
        } else {
            Err(Error::run_error(RunKind::ModuleNotFound(id), self.source, pos))
        }
    }

    fn invoke(&mut self, invoke: &wast::Invoke<'s>) -> Result<'s, Option<Value>> {
        let (runtime, mod_pos) = self.find(invoke.id, invoke.start)?;

        let args: Box<[Value]> = invoke.args.iter().map(|c| c.to_value().unwrap()).collect();
        let ret = runtime
            .invoke(&invoke.name, &args)
            .map_err(|err| Error::run_error(RunKind::Trapped(*err), self.source, mod_pos))?;

        Ok(ret)
    }
}

struct Tester<'a> {
    sum: Summary,
    errs: Vec<Error<'a>>,
    source: &'a str,
    script: &'a wast::Script<'a>,
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

    fn parse_embedded_module(&self, m: &'a wast::EmbeddedModule) -> Result<'a, (ast::Module<'a>, usize)> {
        match &m.src {
            wast::EmbeddedSrc::Quote(text) => {
                let root = wat::parse(text)
                    .map_err(|err| Error::run_error(RunKind::ParseQuoteFailure(err), self.source, m.start))?;

                validate(&root).map_err(|err| Error::run_error(RunKind::InvalidText(*err), self.source, m.start))?;

                Ok((root.module, m.start))
            }
            wast::EmbeddedSrc::Binary(bin) => {
                let root = binary::parse(bin)
                    .map_err(|err| Error::run_error(RunKind::ParseBinaryFailure(*err), self.source, m.start))?;

                validate(&root).map_err(|err| Error::run_error(RunKind::InvalidBinary(*err), self.source, m.start))?;

                Ok((root.module, m.start))
            }
        }
    }

    fn parse_embedded_modules(&mut self) -> IndexToModule<'a> {
        let mut mods = HashMap::new();
        for (idx, cmd) in self.script.commands.iter().enumerate() {
            if let wast::Command::EmbeddedModule(e) = cmd {
                if let Some(m) = self.check(self.parse_embedded_module(e)) {
                    mods.insert(idx, m);
                }
            }
        }
        mods
    }

    fn test(&mut self, crasher: &CrashTester) {
        // Parse and validate modules at first
        let idx_to_mod = self.parse_embedded_modules();

        // Give up assertions check when some module cannot be parsed because some target modules
        // for assertion don't exist
        if self.sum.failed > 0 {
            let num_cmds = self.script.commands.len() as u32;
            let num_modules = idx_to_mod.len() as u32;
            let skipped = num_cmds - num_modules - self.sum.failed;
            self.sum.total += skipped;
            self.sum.skipped += skipped;
            return;
        }

        let mut instances = Instances::new(&idx_to_mod, self.source);
        for (idx, cmd) in self.script.commands.iter().enumerate() {
            let result = self.test_command(idx, cmd, &mut instances, crasher);
            self.check(result);
        }
    }

    fn test_command<'m>(
        &self,
        idx: usize,
        command: &'a wast::Command<'a>,
        instances: &mut Instances<'m, 'a>,
        crasher: &CrashTester,
    ) -> Result<'a, ()> {
        use wast::Command::*;
        match command {
            InlineModule(root) => {
                validate(root)?;
                instances.push(&root.module, root.module.start)
            }
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
            AssertReturn(wast::AssertReturn::Global { start, get, expected }) => {
                let (runtime, _) = instances.find(get.id, *start)?;
                if let Some(actual) = runtime.get_global(&get.name) {
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
            AssertTrap(wast::AssertTrap {
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
                match Runtime::instantiate(&root.module, SpecTestImporter) {
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
            AssertInvalid(wast::AssertInvalid { start, wat, expected }) => match validate(wat) {
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
            AssertExhaustion(wast::AssertExhaustion {
                start,
                expected,
                invoke,
            }) => {
                let (_, mod_pos) = instances.find(invoke.id, invoke.start)?;
                let stderr = crasher.test_crash(self.source, mod_pos, &invoke.name, &invoke.args)?;
                match expected.as_str() {
                    "call stack exhausted"
                        if stderr.contains("fatal runtime error: stack overflow")
                            || stderr.contains("has overflowed its stack") =>
                    {
                        Ok(())
                    }
                    _ => Err(Error::run_error(
                        RunKind::UnexpectedCrash {
                            stderr,
                            expected: expected.clone(),
                        },
                        self.source,
                        *start,
                    )),
                }
            }
            Register(wast::Register { start, .. }) | AssertUnlinkable(wast::AssertUnlinkable { start, .. }) => {
                Err(Error::run_error(RunKind::NotImplementedYet, self.source, *start))
            }
            Invoke(invoke) => instances.invoke(invoke).map(|_| ()),
        }
    }
}
