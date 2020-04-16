use crate::error::{Error, Result, RunKind};
use crate::parser::Parser;
use crate::wast;
use std::fmt;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use wain_ast as ast;
use wain_syntax_binary as binary;
use wain_syntax_text as wat;

const SKIPPED: &[&str] = &["linking.wast"];

mod color {
    pub const RESET: &[u8] = b"\x1b[0m";
    pub const RED: &[u8] = b"\x1b[91m";
    pub const GREEN: &[u8] = b"\x1b[92m";
    pub const YELLOW: &[u8] = b"\x1b[93m";
    pub const BLUE: &[u8] = b"\x1b[94m";
}

#[derive(Default)]
pub struct Summary {
    total: u32,
    passed: u32,
    failed: u32,
    skipped: u32,
}

impl Summary {
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

    fn skip(&mut self) {
        self.total += 1;
        self.skipped += 1;
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

    fn report<D: fmt::Display>(&mut self, nth: usize, total: usize, path: &Path, err: D) {
        self.out.write_all(color::RED).unwrap();
        writeln!(&mut self.out, "\n[{}/{}] {:?}", nth, total, path).unwrap();
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
            let mut sum = Summary::default();
            if let Err(err) = wat::parse(&source) {
                self.report(1, 1, &path, err);
                sum.fail();
            } else {
                sum.pass();
            }
            sum
        } else {
            let mut sum = Summary::default();
            match Parser::new(&source).parse::<wast::Root<'_>>() {
                Err(err) => {
                    self.report(1, 1, &path, err);
                    sum.fail();
                    sum
                }
                Ok(root) => {
                    sum.pass(); // parse success
                    if SKIPPED.contains(&file) {
                        sum.skip();
                        sum
                    } else {
                        let mut tester = Tester {
                            sum,
                            errs: vec![],
                            source: &source,
                            root: &root,
                        };
                        tester.test();
                        let num_errs = tester.errs.len();
                        for (idx, err) in tester.errs.iter_mut().enumerate() {
                            let nth = idx + 1;
                            err.set_path(&path);
                            self.report(nth, num_errs, &path, err);
                        }
                        tester.sum
                    }
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
        let mut modules = vec![];
        for directive in self.root.directives.iter() {
            let result = self.test_directive(directive, &mut modules);
            self.check(result);
        }
    }

    fn test_directive(
        &mut self,
        directive: &'a wast::Directive<'a>,
        known_modules: &mut Vec<(ast::Module<'a>, usize)>,
    ) -> Result<'a, ()> {
        use wast::Directive::*;
        match directive {
            EmbeddedModule(wast::EmbeddedModule {
                embedded: wast::Embedded::Quote(text),
                start,
            }) => {
                // Use inner module's source and offset
                let parsed = wat::parse(text).map_err(Into::into);
                if let Some(root) = self.check(parsed) {
                    known_modules.push((root.module, *start));
                }
            }
            EmbeddedModule(wast::EmbeddedModule {
                embedded: wast::Embedded::Binary(bin),
                start,
            }) => {
                // Since binary format parse error contains &[u8] as source, it is not available
                // for crate::error::Error.
                let parsed = binary::parse(bin).map_err(|err| {
                    Error::run_error(RunKind::ParseBinaryFailure(*err), self.source, *start)
                });
                if let Some(root) = self.check(parsed) {
                    known_modules.push((root.module, *start));
                }
            }
            d => {
                return Err(Error::run_error(
                    RunKind::NotImplementedYet,
                    self.source,
                    d.start_pos(),
                ))
            }
        }
        Ok(())
    }
}
