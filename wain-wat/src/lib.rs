use std::fmt;
use std::iter;
use std::str;

#[cfg_attr(test, derive(Debug))]
pub enum LexErrorKind {
    UnterminatedBlockComment,
    UnterminatedString { start: usize },
    EmptyIdentifier,
}

#[cfg_attr(test, derive(Debug))]
pub struct LexError {
    kind: LexErrorKind,
    offset: usize, // TODO: Use offset and source to show line and column
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexErrorKind::*;
        match self.kind {
            UnterminatedBlockComment => write!(f, "block comment is not terminated")?,
            UnterminatedString { start } => write!(
                f,
                "string literal started at byte offset {} is not terminated",
                start
            )?,
            EmptyIdentifier => write!(f, "Identifier must not be empty")?,
        }
        write!(f, " at byte offset {}", self.offset)
    }
}

pub type LexResult<T> = ::std::result::Result<T, Box<LexError>>;

#[cfg_attr(test, derive(Debug))]
pub enum Sign {
    Plus,
    Minus,
}

// https://webassembly.github.io/spec/core/text/values.html#floating-point
#[cfg_attr(test, derive(Debug))]
pub enum Float<'a> {
    Nan(Option<&'a str>), // Should parse the payload into u64?
    Inf,
    Val(&'a str),
}

// https://webassembly.github.io/spec/core/text/lexical.html#tokens
#[cfg_attr(test, derive(Debug))]
pub enum Token<'a> {
    LParen,
    RParen,
    Keyword(&'a str),
    Int(Sign, &'a str),
    Float(Sign, Float<'a>),
    String(&'a str), // Should parse the literal into Vec<u8>?
    Ident(&'a str),
    // TODO: Reserved
    // token is any identifier which was not recognized. For now they should cause an error
}

type Lexed<'a> = LexResult<Option<(Token<'a>, usize)>>;

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

    pub fn lex(&mut self) -> Lexed<'a> {
        while self.eat_whitespace()? {}

        macro_rules! try_lex {
            ($e:expr) => {
                if let Some(lexed) = $e? {
                    return Ok(Some(lexed));
                }
            };
        }

        // https://webassembly.github.io/spec/core/text/lexical.html#tokens
        try_lex!(self.lex_paren());
        try_lex!(self.lex_string());
        try_lex!(self.lex_ident());

        Ok(None) // TODO: Check an extra token remains
    }

    fn lex_paren(&mut self) -> Lexed<'a> {
        if let Some(offset) = self.eat_char('(') {
            Ok(Some((Token::LParen, offset)))
        } else if let Some(offset) = self.eat_char(')') {
            Ok(Some((Token::RParen, offset)))
        } else {
            Ok(None)
        }
    }

    fn lex_string(&mut self) -> Lexed<'a> {
        // https://webassembly.github.io/spec/core/text/values.html#strings
        let start = match self.eat_char('"') {
            Some(offset) => offset,
            None => return Ok(None),
        };

        // Note: Content of literal will be parsed in parser so we need to traverse the content
        // twice. It would be better to parse the content into Vec<u8> here.
        while let Some((offset, c)) = self.chars.next() {
            if c == '\\' {
                self.chars.next();
            } else if c == '"' {
                let token = Token::String(&self.source[start + 1..offset]); // + 1 consumes '"'
                return Ok(Some((token, start)));
            }
        }

        self.fail(LexErrorKind::UnterminatedString { start })
    }

    fn lex_ident(&mut self) -> Lexed<'a> {
        // https://webassembly.github.io/spec/core/text/values.html#text-id
        let start = match self.eat_char('$') {
            Some(offset) => offset,
            None => return Ok(None),
        };

        fn is_idchar(c: char) -> bool {
            // https://webassembly.github.io/spec/core/text/values.html#text-idchar
            match c {
                '0'..='9'
                | 'a'..='z'
                | 'A'..='Z'
                | '!'
                | '#'
                | '$'
                | '%'
                | '&'
                | '′'
                | '∗'
                | '+'
                | '−'
                | '.'
                | '/'
                | ':'
                | '<'
                | '='
                | '>'
                | '?'
                | '@'
                | '∖'
                | '^'
                | '_'
                | '`'
                | '|'
                | '~' => true,
                _ => false,
            }
        }

        let end = loop {
            match self.chars.peek() {
                Some((_, c)) if is_idchar(*c) => {
                    self.chars.next();
                }
                Some((offset, _)) => break *offset,
                None => break self.source.len(),
            }
        };

        if start == end {
            self.fail(LexErrorKind::EmptyIdentifier)
        } else {
            let token = Token::Ident(&self.source[start..end]);
            Ok(Some((token, start)))
        }
    }

    fn eat_whitespace(&mut self) -> LexResult<bool> {
        // https://webassembly.github.io/spec/core/text/lexical.html#white-space
        Ok(self.eat_one_of_chars(&[' ', '\t', '\n', '\r']).is_some()
            || self.eat_line_comment()
            || self.eat_block_comment()?)
    }

    fn eat_line_comment(&mut self) -> bool {
        // linecomment https://webassembly.github.io/spec/core/text/lexical.html#comments
        if !self.eat_str(";;") {
            return false;
        }

        while let Some((_, c)) = self.chars.next() {
            if c == '\n' {
                break;
            }
        }

        true
    }

    fn eat_block_comment(&mut self) -> LexResult<bool> {
        // blockcomment https://webassembly.github.io/spec/core/text/lexical.html#comments
        if !self.eat_str("(;") {
            return Ok(false);
        }

        // blockchar
        loop {
            if self.eat_block_comment()? {
                continue;
            }
            if self.eat_str(";)") {
                return Ok(true);
            }
            if self.chars.next().is_none() {
                return self.fail(LexErrorKind::UnterminatedBlockComment);
            }
        }
    }

    fn eat_char(&mut self, want: char) -> Option<usize> {
        match self.chars.peek() {
            Some((_, c)) if *c == want => self.chars.next().map(|(o, _)| o),
            _ => None,
        }
    }

    fn eat_one_of_chars(&mut self, chars: &[char]) -> Option<(usize, char)> {
        match self.chars.peek() {
            Some((_, c)) if chars.contains(c) => self.chars.next(),
            _ => None,
        }
    }

    fn eat_str(&mut self, s: &str) -> bool {
        assert!(s.len() > 0);
        if self.input().starts_with(s) {
            self.chars.nth(s.len() - 1);
            true
        } else {
            false
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
        lex_all("\t").unwrap();
        lex_all("\n").unwrap();
        lex_all("\r").unwrap();
        lex_all(" \t\r\n   \t\n\n\n\n ").unwrap();
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
