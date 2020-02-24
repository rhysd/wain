use std::fmt;
use std::iter;
use std::str;

#[derive(Debug)]
pub enum LexErrorKind {
    UnterminatedBlockComment,
}

#[derive(Debug)]
pub struct LexError {
    kind: LexErrorKind,
    offset: usize, // TODO: Use offset and source to show line and column
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexErrorKind::*;
        match self.kind {
            UnterminatedBlockComment => write!(f, "block comment is not terminated")?,
        }
        write!(f, " at byte offset {}", self.offset)
    }
}

pub type LexResult<T> = ::std::result::Result<T, Box<LexError>>;

// https://webassembly.github.io/spec/core/text/values.html#floating-point
#[derive(Debug)]
pub enum Float<'a> {
    Nan(Option<&'a str>), // payload
    Inf,
    Val(&'a str),
}

// https://webassembly.github.io/spec/core/text/lexical.html#tokens
#[derive(Debug)]
pub enum Token<'a> {
    LParen,
    RParen,
    Keyword(&'a str),
    Plus,
    Minus,
    Int(&'a str),
    Float(Float<'a>),
    String(&'a str),
    Ident(&'a str),
    // TODO: Reserved
    // token is any identifier which was not recognized. For now they should cause an error
}

type Lexed<'a> = Option<(Token<'a>, usize)>;

pub struct Lexer<'a> {
    chars: iter::Peekable<str::CharIndices<'a>>, // LL(1)
    source: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer<'_> {
        Lexer {
            source,
            chars: source.char_indices().peekable(),
        }
    }

    pub fn lex(&mut self) -> LexResult<Lexed<'a>> {
        while self.eat_whitespace()? {}
        Ok(None) // TODO: Check an extra token remains
    }

    fn eat_whitespace(&mut self) -> LexResult<bool> {
        // https://webassembly.github.io/spec/core/text/lexical.html#white-space
        match self.chars.peek() {
            None => Ok(false),
            Some((_, c)) => Ok(match c {
                ' ' | '\t' | '\n' | '\r' => {
                    self.chars.next(); // Eat one whitespace
                    true
                }
                _ => self.eat_line_comment() || self.eat_block_comment()?,
            }),
        }
    }

    fn eat_line_comment(&mut self) -> bool {
        // linecomment https://webassembly.github.io/spec/core/text/lexical.html#comments
        if !self.input().starts_with(";;") {
            return false;
        }
        self.chars.nth(1); // Eat ';' and ';'

        while let Some((_, c)) = self.chars.next() {
            if c == '\n' {
                break;
            }
        }

        true
    }

    fn eat_block_comment(&mut self) -> LexResult<bool> {
        // blockcomment https://webassembly.github.io/spec/core/text/lexical.html#comments
        if !self.input().starts_with("(;") {
            return Ok(false);
        }
        self.chars.nth(1); // Eat '(' and ';'

        // blockchar
        loop {
            if self.eat_block_comment()? {
                continue;
            }
            if self.input().starts_with(";)") {
                self.chars.nth(1); // Eat ';' and ')'
                return Ok(true);
            }
            if self.chars.next().is_none() {
                return self.fail(LexErrorKind::UnterminatedBlockComment);
            }
        }
    }

    fn offset(&mut self) -> usize {
        match self.chars.peek() {
            Some((offset, _)) => *offset,
            None => self.source.len(),
        }
    }

    fn input(&mut self) -> &'a str {
        &self.source[self.offset()..]
    }

    fn fail<T>(&mut self, kind: LexErrorKind) -> LexResult<T> {
        let offset = self.offset();
        Err(Box::new(LexError { kind, offset }))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<(Token<'a>, usize)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex().transpose()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all(s: &str) -> LexResult<()> {
        while Lexer::new(s).lex()?.is_some() {}
        Ok(())
    }

    #[test]
    fn spaces() {
        lex_all("").unwrap();
        lex_all(" ").unwrap();
        lex_all("	").unwrap();
        lex_all("\n").unwrap();
        lex_all("\r").unwrap();
        lex_all(" 	\r\n   	\n\n\n\n ").unwrap();
    }

    #[test]
    fn comments() {
        lex_all(";;").unwrap();
        lex_all(";;foo").unwrap();
        lex_all(";;foo\n;;bar\n  ;; piyo").unwrap();
        lex_all("(;;)").unwrap();
        lex_all("(; hi! ;)").unwrap();
        lex_all("(; hi!\n  how are you?\n  bye!\n ;)").unwrap();
        lex_all("(;(;;);)").unwrap();
        lex_all("(;\nhi!\n (;how are you?\n;) bye!\n;)").unwrap();
    }
}
