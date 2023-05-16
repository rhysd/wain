use std::fmt;
use wain_ast::source::Source;

pub fn describe_position(f: &mut fmt::Formatter<'_>, source: &str, start: usize) -> fmt::Result {
    if start == source.len() {
        write!(f, " caused at byte offset {} (end of input)", start)
    } else {
        let source = &source[start..];
        let end = source.find(['\n', '\r'].as_ref()).unwrap_or(source.len());
        write!(
            f,
            " caused at byte offset {}\n\n ... {}\n     ^\n     starts from here",
            start,
            &source[..end],
        )
    }
}

#[derive(Clone)]
pub struct TextSource<'source>(pub(crate) &'source str);

impl<'s> Source for TextSource<'s> {
    type Raw = &'s str;

    fn describe(&self, f: &mut fmt::Formatter<'_>, offset: usize) -> fmt::Result {
        describe_position(f, self.0, offset)
    }

    fn raw(&self) -> Self::Raw {
        self.0
    }
}
