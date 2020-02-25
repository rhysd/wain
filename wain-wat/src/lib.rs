use std::fmt;
use std::iter;
use std::str;

#[cfg_attr(test, derive(Debug))]
pub enum LexErrorKind<'a> {
    UnterminatedBlockComment,
    UnterminatedString,
    ReservedName(&'a str),
    UnexpectedCharacter(char),
}

// TODO: Support std::error::Error

#[cfg_attr(test, derive(Debug))]
pub struct LexError<'a> {
    kind: LexErrorKind<'a>,
    offset: usize,
    source: &'a str,
}

impl<'a> LexError<'a> {
    pub fn kind(&self) -> &LexErrorKind<'a> {
        &self.kind
    }
}

impl<'a> fmt::Display for LexError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexErrorKind::*;
        match &self.kind {
            UnterminatedBlockComment => write!(f, "block comment is not terminated")?,
            UnterminatedString => write!(f, "string literal is not terminated",)?,
            ReservedName(name) => {
                write!(f, "Name '{}' is unavailable since it's reserved name", name)?
            }
            UnexpectedCharacter(c) => write!(f, "unexpected character '{}'", c)?,
        }

        let start = self.offset;
        let end = self.source[start..]
            .find(['\n', '\r'].as_ref())
            .unwrap_or(self.source.len());
        write!(
            f,
            " at byte offset {}\n\n ... {}\n     ^",
            self.offset,
            &self.source[start..end]
        )
    }
}

pub type LexResult<'a, T> = ::std::result::Result<T, Box<LexError<'a>>>;

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq)]
pub enum Sign {
    Plus,
    Minus,
}

// https://webassembly.github.io/spec/core/text/values.html#floating-point
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Float<'a> {
    Nan(Option<&'a str>), // Should parse the payload into u64?
    Inf,
    Val(&'a str),
}

// https://webassembly.github.io/spec/core/text/lexical.html#tokens
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Token<'a> {
    LParen,
    RParen,
    Keyword(&'a str), // Too many keywords so it's not pragmatic to define `Keyword` enum in terms of maintenance
    Int(Sign, &'a str),
    Float(Sign, Float<'a>),
    String(&'a str), // Should parse the literal into Vec<u8>?
    Ident(&'a str),
}

type Lexed<'a> = LexResult<'a, Option<(Token<'a>, usize)>>;

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
        // TODO: Lex numbers
        try_lex!(self.lex_ident_or_keyword());

        if let Some(peeked) = self.chars.peek() {
            let (offset, c) = peeked.clone(); // Borrow checker complains about *c and *offset in below statement
            self.fail(LexErrorKind::UnexpectedCharacter(c), offset)
        } else {
            Ok(None)
        }
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

        self.fail(LexErrorKind::UnterminatedString, start)
    }

    fn lex_ident_or_keyword(&mut self) -> Lexed<'a> {
        // https://webassembly.github.io/spec/core/text/lexical.html#tokens
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
                | '\''
                | '*'
                | '+'
                | '-'
                | '.'
                | '/'
                | ':'
                | '<'
                | '='
                | '>'
                | '?'
                | '@'
                | '\\'
                | '^'
                | '_'
                | '`'
                | '|'
                | '~' => true,
                _ => false,
            }
        }

        let start = self.offset();
        let end = loop {
            match self.chars.next() {
                Some((_, c)) if is_idchar(c) => continue,
                Some((offset, _)) => break offset,
                None => break self.source.len(),
            }
        };

        let word = &self.source[start..end];
        match word.chars().next() {
            Some('$') if word.len() > 1 => Ok(Some((Token::Ident(word), start))), // https://webassembly.github.io/spec/core/text/values.html#text-id
            Some('a'..='z') => Ok(Some((Token::Keyword(word), start))), // https://webassembly.github.io/spec/core/text/lexical.html#text-keyword
            Some(_) => self.fail(LexErrorKind::ReservedName(word), start), // https://webassembly.github.io/spec/core/text/lexical.html#text-reserved
            None => Ok(None),
        }
    }

    fn eat_whitespace(&mut self) -> LexResult<'a, bool> {
        // https://webassembly.github.io/spec/core/text/lexical.html#white-space
        Ok(self.eat_one_of_chars(&[' ', '\t', '\n', '\r']).is_some()
            || self.eat_line_comment()
            || self.eat_block_comment()?)
    }

    fn eat_line_comment(&mut self) -> bool {
        // linecomment https://webassembly.github.io/spec/core/text/lexical.html#comments
        if self.eat_str(";;").is_none() {
            return false;
        }

        while let Some((_, c)) = self.chars.next() {
            if c == '\n' {
                break;
            }
        }

        true
    }

    fn eat_block_comment(&mut self) -> LexResult<'a, bool> {
        // blockcomment https://webassembly.github.io/spec/core/text/lexical.html#comments
        let start = if let Some(offset) = self.eat_str("(;") {
            offset
        } else {
            return Ok(false);
        };

        // blockchar
        loop {
            if self.eat_block_comment()? {
                continue;
            }
            if self.eat_str(";)").is_some() {
                return Ok(true);
            }
            if self.chars.next().is_none() {
                return self.fail(LexErrorKind::UnterminatedBlockComment, start);
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

    fn eat_str(&mut self, s: &str) -> Option<usize> {
        assert!(s.len() > 0);
        let offset = self.offset();
        if self.source[offset..].starts_with(s) {
            self.chars.nth(s.len() - 1);
            Some(offset)
        } else {
            None
        }
    }

    fn offset(&mut self) -> usize {
        match self.chars.peek() {
            Some((offset, _)) => *offset,
            None => self.source.len(),
        }
    }

    fn fail<T>(&self, kind: LexErrorKind<'a>, offset: usize) -> LexResult<'a, T> {
        Err(Box::new(LexError {
            kind,
            offset,
            source: self.source,
        }))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a, (Token<'a>, usize)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex().transpose()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all<'a>(s: &'a str) -> LexResult<'a, Vec<(Token<'a>, usize)>> {
        Lexer::new(s).collect()
    }

    macro_rules! assert_matches {
        ($e:expr, $p:pat) => {
            match $e {
                $p => (),
                e => panic!(
                    "assertion failed: {:?} did not match to {}",
                    e,
                    stringify!($p)
                ),
            }
        };
        ($e:expr, $p:pat if $c:expr) => {
            match $e {
                $p if $c => (),
                e => panic!(
                    "assertion failed: {:?} did not match to {}",
                    e,
                    stringify!($p if $c),
                ),
            }
        };
    }

    #[test]
    fn spaces() {
        assert!(lex_all("").unwrap().is_empty());
        assert!(lex_all(" ").unwrap().is_empty());
        assert!(lex_all("\t").unwrap().is_empty());
        assert!(lex_all("\n").unwrap().is_empty());
        assert!(lex_all("\r").unwrap().is_empty());
        assert!(lex_all(" \t\r\n   \t\n\n\n\n ").unwrap().is_empty());
    }

    #[test]
    fn comments() {
        assert!(lex_all(";;").unwrap().is_empty());
        assert!(lex_all(";;foo").unwrap().is_empty());
        assert!(lex_all(";;foo\n;;bar\n  ;; piyo").unwrap().is_empty());
        assert!(lex_all("(;;)").unwrap().is_empty());
        assert!(lex_all("(; hi! ;)").unwrap().is_empty());
        assert!(lex_all("(; hi!\n  how are you?\n  bye!\n ;)")
            .unwrap()
            .is_empty());
        assert!(lex_all("(;(;;);)").unwrap().is_empty());
        assert!(lex_all("(;\nhi!\n (;how are you?\n;) bye!\n;)")
            .unwrap()
            .is_empty());
        // Errors
        assert_matches!(
            lex_all("(;").unwrap_err().kind(),
            LexErrorKind::UnterminatedBlockComment
        );
        assert_matches!(
            lex_all("(; hi! ").unwrap_err().kind(),
            LexErrorKind::UnterminatedBlockComment
        );
        assert_matches!(
            lex_all("(;(;;)").unwrap_err().kind(),
            LexErrorKind::UnterminatedBlockComment
        );
    }

    #[test]
    fn parens() {
        assert_eq!(lex_all("(").unwrap(), vec![(Token::LParen, 0)]);
        assert_eq!(lex_all(")").unwrap(), vec![(Token::RParen, 0)]);
    }

    #[test]
    fn strings() {
        let tokens = lex_all(r#""""#).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_matches!(&tokens[0].0, Token::String(""));
        let tokens = lex_all(r#""hello""#).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_matches!(&tokens[0].0, Token::String("hello"));
        let tokens = lex_all(r#""\t\n\r\"\'\\\u{1234}\00\a9""#).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_matches!(&tokens[0].0, Token::String(r#"\t\n\r\"\'\\\u{1234}\00\a9"#));
        let tokens = lex_all(r#""あいうえお""#).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_matches!(&tokens[0].0, Token::String("あいうえお"));
        // Errors
        assert_matches!(
            lex_all(r#"""#).unwrap_err().kind(),
            LexErrorKind::UnterminatedString
        );
        assert_matches!(
            lex_all(r#""foo\""#).unwrap_err().kind(),
            LexErrorKind::UnterminatedString
        );
    }

    #[test]
    fn idents() {
        let tokens = lex_all("$x").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_matches!(&tokens[0].0, Token::Ident("$x"));
        let tokens = lex_all("$foo0123FOO").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_matches!(&tokens[0].0, Token::Ident("$foo0123FOO"));
        let tokens = lex_all("$0aB!#$%&'*+-./:<=>?@\\^_`|~").unwrap();
        assert_eq!(tokens.len(), 1);
        assert_matches!(&tokens[0].0, Token::Ident("$0aB!#$%&'*+-./:<=>?@\\^_`|~"));
        // Errors
        assert_matches!(
            lex_all("$").unwrap_err().kind(),
            LexErrorKind::ReservedName(name) if *name == "$"
        );
        assert_matches!(
            lex_all("$ ;;").unwrap_err().kind(),
            LexErrorKind::ReservedName(name) if *name == "$"
        );
    }
}
