use std::fmt;

pub(crate) fn describe_position(
    f: &mut fmt::Formatter<'_>,
    source: &str,
    start: usize,
) -> fmt::Result {
    if start == source.len() {
        write!(f, " caused at byte offset {} (end of input)", start)
    } else {
        let source = &source[start..];
        let end = source
            .find(['\n', '\r'].as_ref())
            .unwrap_or_else(|| source.len());
        write!(
            f,
            " caused at byte offset {}\n\n ... {}\n     ^\n     starts from here",
            start,
            &source[..end],
        )
    }
}
