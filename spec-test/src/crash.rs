use crate::error::{Error, Result, RunKind};
use crate::wast;
use std::path::PathBuf;
use std::process::Command;
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
        p.push("crash-tester"); // /path/to/wain/target/debug/crash-tester

        assert!(p.is_file());

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
        cmd.arg(source);

        let mut cmd_args = vec![];
        cmd_args.push(mod_offset.to_string());
        cmd_args.push(name.to_owned());

        for arg in args {
            let (ty, v) = match arg.to_value().unwrap() {
                Value::I32(i) => ("i32", i.to_string()),
                Value::I64(i) => ("i64", i.to_string()),
                Value::F32(f) => ("f32", f.to_string()),
                Value::F64(f) => ("f64", f.to_string()),
            };
            cmd_args.push(ty.to_owned());
            cmd_args.push(v);
        }

        let out = cmd.args(&cmd_args).output().unwrap();
        if out.status.success() {
            return Err(Error::run_error(
                RunKind::DidNotCrash {
                    bin: self.bin_path.clone(),
                    args: cmd_args.into_boxed_slice(),
                    stdout: String::from_utf8_lossy(&out.stdout).to_string(),
                },
                source,
                mod_offset,
            ));
        }

        Ok(String::from_utf8_lossy(&out.stderr).to_string())
    }
}
