#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let Ok(root) = wain_syntax_binary::parse(data) else { return; };
    let _ = wain_validate::validate(&root);
});
