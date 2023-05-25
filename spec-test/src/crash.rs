use crate::error::{Error, Result, RunKind};
use crate::wast;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use wain_exec::Value;

pub struct CrashTester {
    bin_path: PathBuf,
}

impl CrashTester {
    pub fn new() -> Self {
        let out = Command::new("cargo")
            .arg("build")
            .arg("--bin")
            .arg("crash-tester")
            .output()
            .unwrap();
        if !out.status.success() {
            panic!(
                "cargo build --bin crash-tester failed with non-zero exit status: {}",
                String::from_utf8_lossy(&out.stdout),
            );
        }

        let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR")); // /path/to/wain/spec-test
        p.pop(); // /path/to/wain
        p.push("target"); // /path/to/wain/target
        p.push("debug"); // /path/to/wain/target/debug

        #[cfg(target_os = "windows")]
        p.push("crash-tester.exe"); // /path/to/wain/target/debug/crash-tester.exe

        #[cfg(not(target_os = "windows"))]
        p.push("crash-tester"); // /path/to/wain/target/debug/crash-tester

        assert!(p.is_file(), "crash tester path: {:?}", p);

        Self { bin_path: p }
    }

    pub fn test_crash<'s>(
        &self,
        source: &'s str,
        mod_offset: usize,
        name: &str,
        args: &[wast::Const],
    ) -> Result<'s, String> {
        let mut cmd = Command::new(&self.bin_path);
        cmd.arg(mod_offset.to_string()).arg(name);
        cmd.stdin(Stdio::piped()).stdout(Stdio::piped()).stderr(Stdio::piped());

        let mut child = cmd.spawn().unwrap();

        // Input arguments to stdin. One argument per line
        //
        //   i32 42
        //   f32 3.14
        //   ...
        let stdin = child.stdin.as_mut().unwrap();
        for arg in args {
            match arg.to_value().unwrap() {
                Value::I32(i) => writeln!(stdin, "i32 {}", i),
                Value::I64(i) => writeln!(stdin, "i64 {}", i),
                Value::F32(f) => writeln!(stdin, "f32 {}", f),
                Value::F64(f) => writeln!(stdin, "f64 {}", f),
            }
            .unwrap();
        }
        //  And ends with empty line.
        writeln!(stdin).unwrap();
        // Then input source code to stdin. Command line argument is not available since some test
        // code is too large for command line argument on Windows
        stdin.write_all(source.as_bytes()).unwrap();
        stdin.flush().unwrap();

        let out = child.wait_with_output().unwrap();
        if out.status.success() {
            return Err(Error::run_error(
                RunKind::DidNotCrash {
                    bin: self.bin_path.clone(),
                    args: args.iter().map(|a| format!("{:?}", a)).collect(),
                    stdout: String::from_utf8_lossy(&out.stdout).to_string(),
                },
                source,
                mod_offset,
            ));
        }

        Ok(String::from_utf8_lossy(&out.stderr).to_string())
    }
}
