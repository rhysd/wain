use crate::source::describe_position;
use std::borrow::Cow;
use std::char;
use std::fmt;
use std::iter;
use std::ops;
use std::str;

#[cfg_attr(test, derive(Debug))]
#[derive(Clone)]
pub enum LexErrorKind<'source> {
    UnterminatedBlockComment,
    UnterminatedString,
    ReservedName(&'source str),
    UnexpectedCharacter(char),
    ControlCharInString,
    InvalidStringFormat,
}

// TODO: Support std::error::Error

#[cfg_attr(test, derive(Debug))]
#[derive(Clone)]
pub struct LexError<'source> {
    kind: LexErrorKind<'source>,
    offset: usize,
    source: &'source str,
}

impl<'s> LexError<'s> {
    pub fn kind(&self) -> &LexErrorKind<'s> {
        &self.kind
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn source(&self) -> &'s str {
        self.source
    }
}

impl<'s> fmt::Display for LexError<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexErrorKind::*;
        match &self.kind {
            UnterminatedBlockComment => write!(f, "block comment is not terminated")?,
            UnterminatedString => write!(f, "string literal is not terminated",)?,
            ReservedName(name) => write!(f, "name '{}' is unavailable since it's reserved name", name)?,
            UnexpectedCharacter(c) => write!(f, "unexpected character '{}'", c)?,
            ControlCharInString => write!(f, "control char in string")?,
            InvalidStringFormat => write!(
                f,
                r#"escape must be one of \t, \n, \r, \", \', \\, \u{{hexnum}}, \MN where M and N are hex number"#
            )?,
        }
        describe_position(f, self.source, self.offset)
    }
}

type Result<'s, T> = ::std::result::Result<T, Box<LexError<'s>>>;

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, Copy, PartialEq)]
pub enum Sign {
    Plus,
    Minus,
}

impl Sign {
    pub fn apply<N: ops::Neg<Output = N>>(self, n: N) -> N::Output {
        match self {
            Sign::Plus => n,
            Sign::Minus => -n,
        }
    }
}

impl fmt::Display for Sign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sign::Plus => f.write_str("+"),
            Sign::Minus => f.write_str("-"),
        }
    }
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Clone, Copy)]
pub enum NumBase {
    Hex,
    Dec,
}

impl NumBase {
    pub fn prefix(self) -> &'static str {
        match self {
            NumBase::Hex => "0x",
            NumBase::Dec => "",
        }
    }

    pub fn radix(self) -> u32 {
        match self {
            NumBase::Hex => 16,
            NumBase::Dec => 10,
        }
    }
}

// https://webassembly.github.io/spec/core/text/values.html#floating-point
#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone)]
pub enum Float<'source> {
    Nan(Option<&'source str>),
    Inf,
    Val {
        base: NumBase,
        frac: &'source str,
        exp: Option<(Sign, &'source str)>,
    },
}

// https://webassembly.github.io/spec/core/text/lexical.html#tokens
#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone)]
pub enum Token<'source> {
    LParen,
    RParen,
    Keyword(&'source str), // Too many keywords so it'source not pragmatic to define `Keyword` enum in terms of maintenance
    Int(Sign, NumBase, &'source str),
    Float(Sign, Float<'source>),
    String(Cow<'source, [u8]>, &'source str),
    Ident(&'source str),
}

impl<'s> fmt::Display for Token<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::LParen => f.write_str("paren '('"),
            Token::RParen => f.write_str("paren ')'"),
            Token::Keyword(kw) => write!(f, "keyword '{}'", kw),
            Token::Int(sign, base, s) => write!(f, "integer '{}{}{}'", sign, base.prefix(), s),
            Token::Float(sign, Float::Nan(Some(payload))) => {
                write!(f, "float number '{}nan:0x{}'", sign, payload)
            }
            Token::Float(sign, Float::Nan(None)) => write!(f, "float number '{}nan'", sign),
            Token::Float(sign, Float::Inf) => write!(f, "float number '{}inf'", sign),
            Token::Float(
                sign,
                Float::Val {
                    base,
                    frac,
                    exp: Some((exp_sign, exp)),
                },
            ) => {
                let exp_leader = if *base == NumBase::Hex { 'P' } else { 'E' };
                write!(
                    f,
                    "float number '{sign}{prefix}{frac}{exp_leader}{exp_sign}{exp}",
                    sign = sign,
                    prefix = base.prefix(),
                    frac = frac,
                    exp_leader = exp_leader,
                    exp_sign = exp_sign,
                    exp = exp
                )
            }
            Token::Float(sign, Float::Val { base, frac, exp: None }) => {
                write!(f, "float number '{}{}{}", sign, base.prefix(), frac,)
            }
            Token::String(_, s) => write!(f, "string literal {}", s),
            Token::Ident(ident) => write!(f, "identifier '{}'", ident),
        }
    }
}

type Lexed<'s> = Option<(Token<'s>, usize)>;
type LexResult<'s> = Result<'s, Lexed<'s>>;

#[derive(Clone)]
pub struct Lexer<'source> {
    chars: iter::Peekable<str::CharIndices<'source>>, // LL(1)
    source: &'source str,
}

impl<'s> Lexer<'s> {
    pub fn new(source: &str) -> Lexer<'_> {
        Lexer {
            source,
            chars: source.char_indices().peekable(),
        }
    }

    pub fn source(&self) -> &'s str {
        self.source
    }

    pub fn lex(&mut self) -> LexResult<'s> {
        while self.eat_whitespace()? {}

        // https://webassembly.github.io/spec/core/text/lexical.html#tokens
        if let Some(lexed) = self.lex_paren() {
            return Ok(Some(lexed));
        }
        if let Some(lexed) = self.lex_string()? {
            return Ok(Some(lexed));
        }
        // id, keyword, reserved, number
        if let Some(lexed) = self.lex_idchars()? {
            return Ok(Some(lexed));
        }

        if let Some(peeked) = self.chars.peek() {
            let (offset, c) = *peeked; // Borrow checker complains about *c and *offset in below statement
            self.fail(LexErrorKind::UnexpectedCharacter(c), offset)
        } else {
            Ok(None)
        }
    }

    fn lex_paren(&mut self) -> Lexed<'s> {
        if let Some(offset) = self.eat_char('(') {
            Some((Token::LParen, offset))
        } else {
            self.eat_char(')').map(|offset| (Token::RParen, offset))
        }
    }

    fn lex_string(&mut self) -> LexResult<'s> {
        // https://webassembly.github.io/spec/core/text/values.html#strings
        let start = match self.eat_char('"') {
            Some(offset) => offset,
            None => return Ok(None),
        };

        let mut buf = vec![];
        while let Some((i, c)) = self.chars.next() {
            match c {
                '"' => {
                    let content = if buf.is_empty() {
                        // When no escape is included in string literal, keep slice without copy
                        // omitting the first and last double quotes
                        Cow::Borrowed(self.source[start + 1..i].as_bytes())
                    } else {
                        Cow::Owned(buf)
                    };
                    let token = Token::String(content, &self.source[start..i + 1]);
                    return Ok(Some((token, start)));
                }
                '\\' => {
                    if buf.is_empty() {
                        // Encounter the first escaped character. It means the source of literal is
                        // different from its content. Need to allocate another buffer to keep its
                        // content. Note that `+ 1` omits the first '"'
                        buf.extend_from_slice(self.source[start + 1..i].as_bytes());
                    }

                    match self.chars.next() {
                        Some((_, 't')) => buf.push(b'\t'),
                        Some((_, 'n')) => buf.push(b'\n'),
                        Some((_, 'r')) => buf.push(b'\r'),
                        Some((_, '"')) => buf.push(b'"'),
                        Some((_, '\'')) => buf.push(b'\''),
                        Some((_, '\\')) => buf.push(b'\\'),
                        Some((_, 'u')) => {
                            match self.chars.next() {
                                Some((i, '{')) => {
                                    let brace_start = i + 1; // next to '{'
                                    let uend = loop {
                                        match self.chars.next() {
                                            Some((i, '}')) => break i,
                                            Some(_) => continue,
                                            None => return self.fail(LexErrorKind::UnterminatedString, start),
                                        }
                                    };
                                    if let Some(c) = u32::from_str_radix(&self.source[brace_start..uend], 16)
                                        .ok()
                                        .and_then(char::from_u32)
                                    {
                                        let mut b = [0; 4];
                                        buf.extend_from_slice(c.encode_utf8(&mut b).as_bytes());
                                    } else {
                                        return self.fail(LexErrorKind::InvalidStringFormat, start);
                                    }
                                }
                                Some(_) => return self.fail(LexErrorKind::InvalidStringFormat, start),
                                None => return self.fail(LexErrorKind::UnterminatedString, start),
                            }
                        }
                        Some((_, c)) => {
                            let hi = c.to_digit(16);
                            let lo = self.chars.next().and_then(|(_, c)| c.to_digit(16));
                            match (hi, lo) {
                                (Some(hi), Some(lo)) => buf.push((hi * 16 + lo) as u8),
                                _ => return self.fail(LexErrorKind::InvalidStringFormat, start),
                            }
                        }
                        None => return self.fail(LexErrorKind::UnterminatedString, start),
                    }
                }
                _ if c.is_ascii_control() => return self.fail(LexErrorKind::ControlCharInString, start),
                _ if !buf.is_empty() => {
                    let mut b = [0; 4];
                    buf.extend_from_slice(c.encode_utf8(&mut b).as_bytes());
                }
                _ => { /* Have not seen any escape chars yet */ }
            }
        }

        self.fail(LexErrorKind::UnterminatedString, start)
    }

    fn lex_idchars(&mut self) -> LexResult<'s> {
        fn is_idchar(c: char) -> bool {
            // https://webassembly.github.io/spec/core/text/values.html#text-idchar
            matches!(c,
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
                | '~'
            )
        }

        let start = self.offset();
        let end = loop {
            match self.chars.peek() {
                Some((_, c)) if is_idchar(*c) => {
                    self.chars.next();
                    continue;
                }
                Some((offset, _)) => break *offset,
                None => break self.source.len(),
            }
        };

        if start == end {
            return Ok(None);
        }

        // Note: Number must be lexed before keyword for 'inf' and 'nan'
        let idchars = &self.source[start..end];
        if let Some(lexed) = Self::lex_number_from_idchars(idchars, start) {
            return Ok(Some(lexed));
        }
        if let Some(lexed) = Self::lex_ident_or_keyword_from_idchars(idchars, start) {
            return Ok(Some(lexed));
        }

        // https://webassembly.github.io/spec/core/text/lexical.html#text-reserved
        self.fail(LexErrorKind::ReservedName(idchars), start)
    }

    fn is_num<F: Fn(&char) -> bool>(s: &str, pred: F) -> bool {
        if s.is_empty() {
            return false;
        }
        let mut prev_underscore = true; // true because number cannot start with '_'
        for c in s.chars() {
            match c {
                '_' if prev_underscore => return false,
                '_' => prev_underscore = true,
                _ if pred(&c) => prev_underscore = false,
                _ => return false,
            }
        }
        !prev_underscore
    }

    fn lex_unsigned_number(idchars: &'s str, sign: Sign, base: NumBase) -> Option<Token<'s>> {
        // https://webassembly.github.io/spec/core/text/values.html#integers
        // https://webassembly.github.io/spec/core/text/values.html#floating-point

        fn is_hex_exp(c: char) -> bool {
            c == 'p' || c == 'P'
        }
        fn is_dec_exp(c: char) -> bool {
            c == 'e' || c == 'E'
        }

        #[allow(clippy::type_complexity)]
        let (is_digit, is_exp): (fn(&char) -> bool, fn(char) -> bool) = match base {
            NumBase::Hex => (char::is_ascii_hexdigit, is_hex_exp),
            NumBase::Dec => (char::is_ascii_digit, is_dec_exp),
        };
        let mut chars = idchars.char_indices();
        if chars.next().map(|(_, c)| !is_digit(&c)).unwrap_or(true) {
            return None;
        }

        let mut exp_start = false;
        let mut saw_dot = false;
        {
            #[derive(PartialEq, Eq)]
            enum PrevChar {
                Dot,
                Underscore,
                Digit,
            }

            let mut prev_char = PrevChar::Digit;
            for (_, c) in &mut chars {
                prev_char = match c {
                    '.' if saw_dot || prev_char != PrevChar::Digit => return None,
                    '.' => {
                        saw_dot = true;
                        PrevChar::Dot
                    }
                    '_' if prev_char != PrevChar::Digit => return None,
                    '_' => PrevChar::Underscore,
                    c if is_exp(c) => {
                        exp_start = true;
                        break;
                    }
                    c if is_digit(&c) => PrevChar::Digit,
                    _ => return None,
                };
            }

            // Number cannot end with '_'
            if prev_char == PrevChar::Underscore {
                return None;
            }
        }

        match chars.next() {
            Some((i, c)) if exp_start => {
                let (exp_sign, start) = match c {
                    '+' => (Sign::Plus, i + 1),
                    '-' => (Sign::Minus, i + 1),
                    _ => (Sign::Plus, i),
                };
                let frac = &idchars[..i - 1]; // - 1 for 'e', 'E', 'p', 'P'
                let exp = &idchars[start..];
                if Self::is_num(exp, char::is_ascii_digit) {
                    let float = Float::Val {
                        base,
                        frac,
                        exp: Some((exp_sign, exp)),
                    };
                    Some(Token::Float(sign, float))
                } else {
                    None
                }
            }
            Some(_) => unreachable!(),
            None if exp_start => None, // e.g. '123e', '0x1fp'
            None if saw_dot => Some(Token::Float(
                sign,
                Float::Val {
                    base,
                    frac: idchars,
                    exp: None,
                },
            )),
            None => Some(Token::Int(sign, base, idchars)),
        }
    }

    fn lex_number_from_idchars(idchars: &'s str, start: usize) -> Lexed<'s> {
        let (sign, idchars) = match idchars.chars().next() {
            Some('+') => (Sign::Plus, &idchars[1..]),
            Some('-') => (Sign::Minus, &idchars[1..]),
            _ => (Sign::Plus, idchars),
        };

        // https://webassembly.github.io/spec/core/text/values.html#text-float
        let token = match idchars {
            "inf" => Some(Token::Float(sign, Float::Inf)),
            "nan" => Some(Token::Float(sign, Float::Nan(None))),
            idchars if idchars.starts_with("nan:0x") => {
                let payload = &idchars[6..];
                if Self::is_num(payload, char::is_ascii_hexdigit) {
                    Some(Token::Float(sign, Float::Nan(Some(payload))))
                } else {
                    None
                }
            }
            idchars if idchars.starts_with("0x") => Self::lex_unsigned_number(&idchars[2..], sign, NumBase::Hex),
            idchars => Self::lex_unsigned_number(idchars, sign, NumBase::Dec),
        };
        token.map(|t| (t, start))
    }

    fn lex_ident_or_keyword_from_idchars(idchars: &'s str, start: usize) -> Lexed<'s> {
        // https://webassembly.github.io/spec/core/text/lexical.html#tokens
        match idchars.chars().next() {
            Some('$') if idchars.len() > 1 => Some((Token::Ident(idchars), start)), // https://webassembly.github.io/spec/core/text/values.html#text-id
            Some('a'..='z') => Some((Token::Keyword(idchars), start)), // https://webassembly.github.io/spec/core/text/lexical.html#text-keyword
            _ => None,
        }
    }

    fn eat_whitespace(&mut self) -> Result<'s, bool> {
        // https://webassembly.github.io/spec/core/text/lexical.html#white-space
        fn is_ws_char(c: char) -> bool {
            matches!(c, ' ' | '\t' | '\n' | '\r')
        }
        Ok(self.eat_char_by(is_ws_char) || self.eat_line_comment() || self.eat_block_comment()?)
    }

    fn eat_line_comment(&mut self) -> bool {
        // linecomment https://webassembly.github.io/spec/core/text/lexical.html#comments
        if self.eat_str(";;").is_none() {
            return false;
        }

        for (_, c) in &mut self.chars {
            if c == '\n' {
                break;
            }
        }

        true
    }

    fn eat_block_comment(&mut self) -> Result<'s, bool> {
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
            Some((offset, c)) if *c == want => {
                let offset = *offset;
                self.chars.next();
                Some(offset)
            }
            _ => None,
        }
    }

    fn eat_char_by<F: Fn(char) -> bool>(&mut self, pred: F) -> bool {
        match self.chars.peek() {
            Some((_, c)) if pred(*c) => {
                self.chars.next();
                true
            }
            _ => false,
        }
    }

    fn eat_str(&mut self, s: &str) -> Option<usize> {
        assert!(!s.is_empty());
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

    fn fail<T>(&self, kind: LexErrorKind<'s>, offset: usize) -> Result<'s, T> {
        Err(Box::new(LexError {
            kind,
            offset,
            source: self.source,
        }))
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Result<'s, (Token<'s>, usize)>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex().transpose()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all(s: &str) -> Result<'_, Vec<(Token<'_>, usize)>> {
        Lexer::new(s).collect()
    }

    macro_rules! assert_lex_one {
        ($input:expr, $token:pat) => {
            let tokens = lex_all($input).unwrap();
            assert_eq!(tokens.len(), 1);
            match &tokens[0].0 {
                $token => {}
                e => panic!(
                    "assertion failed: {:?} did not match to token {}",
                    e,
                    stringify!($token)
                ),
            }
        };
    }

    macro_rules! assert_lex_error {
        ($input:expr, $errkind:pat) => {
            match lex_all($input).unwrap_err().kind() {
                $errkind => {}
                e => panic!(
                    "assertion failed: {:?} did not match to error kind {}",
                    e,
                    stringify!($token)
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
        assert!(lex_all("(; hi!\n  how are you?\n  bye!\n ;)").unwrap().is_empty());
        assert!(lex_all("(;(;;);)").unwrap().is_empty());
        assert!(lex_all("(;\nhi!\n (;how are you?\n;) bye!\n;)").unwrap().is_empty());
        // Errors
        assert_lex_error!("(;", LexErrorKind::UnterminatedBlockComment);
        assert_lex_error!("(; hi! ", LexErrorKind::UnterminatedBlockComment);
        assert_lex_error!("(;(;;)", LexErrorKind::UnterminatedBlockComment);
    }

    #[test]
    fn parens() {
        assert_lex_one!("(", Token::LParen);
        assert_lex_one!(")", Token::RParen);
    }

    #[test]
    fn strings() {
        macro_rules! assert_lex_string {
            ($input:expr, $bytes:expr) => {
                let tokens = lex_all($input).unwrap();
                assert_eq!(tokens.len(), 1);
                match &tokens[0].0 {
                    Token::String(v, src) if *v == $bytes.to_vec() && *src == $input => {}
                    e => panic!(
                        "assertion failed: {:?} did not match to token {}",
                        e,
                        stringify!(Token::String($bytes, $input))
                    ),
                }
            };
        }

        assert_lex_string!(r#""""#, b"");
        assert_lex_string!(r#""hello""#, b"hello");
        let mut v = "\t\n\r\"\'\\\u{1234}\x00".as_bytes().to_vec();
        v.push(b'\xa9');
        assert_lex_string!(r#""\t\n\r\"\'\\\u{1234}\00\a9""#, v);
        assert_lex_string!(r#""あいうえお""#, "あいうえお".as_bytes());
        assert_lex_error!(r#"""#, LexErrorKind::UnterminatedString);
        assert_lex_error!(r#""foo\""#, LexErrorKind::UnterminatedString);
        assert_lex_error!(r#""\u{41""#, LexErrorKind::UnterminatedString);
        assert_lex_error!(r#""\u"#, LexErrorKind::UnterminatedString);
        assert_lex_error!(r#""\u{""#, LexErrorKind::UnterminatedString);

        assert_lex_error!(r#""\x""#, LexErrorKind::InvalidStringFormat);
        assert_lex_error!(r#""\0""#, LexErrorKind::InvalidStringFormat);
        assert_lex_error!(r#""\0x""#, LexErrorKind::InvalidStringFormat);
        assert_lex_error!(r#""\u""#, LexErrorKind::InvalidStringFormat);
        assert_lex_error!(r#""\u{}""#, LexErrorKind::InvalidStringFormat);
        assert_lex_error!(r#""\u{hello!}""#, LexErrorKind::InvalidStringFormat);
        assert_lex_error!(r#""\u{d800}""#, LexErrorKind::InvalidStringFormat);
        assert_lex_error!(r#""\u{dfff}""#, LexErrorKind::InvalidStringFormat);
        assert_lex_error!(r#""\u{110000}""#, LexErrorKind::InvalidStringFormat);

        assert_lex_error!("\"\x00\"", LexErrorKind::ControlCharInString);
        assert_lex_error!("\"\x1f\"", LexErrorKind::ControlCharInString);
        assert_lex_error!("\"\x7f\"", LexErrorKind::ControlCharInString);
    }

    #[test]
    fn idents() {
        assert_lex_one!("$x", Token::Ident("$x"));
        assert_lex_one!("$foo0123FOO", Token::Ident("$foo0123FOO"));
        assert_lex_one!(
            "$0aB!#$%&'*+-./:<=>?@\\^_`|~",
            Token::Ident("$0aB!#$%&'*+-./:<=>?@\\^_`|~")
        );
    }

    #[test]
    fn keywords() {
        assert_lex_one!("module", Token::Keyword("module"));
        assert_lex_one!("i32.const", Token::Keyword("i32.const"));
        assert_lex_one!("nan:0x_1", Token::Keyword("nan:0x_1"));
        assert_lex_one!("nan:0x1_", Token::Keyword("nan:0x1_"));
        assert_lex_one!("nan:0x1__2", Token::Keyword("nan:0x1__2"));
    }

    #[test]
    fn reserved() {
        assert_lex_error!("0$foo", LexErrorKind::ReservedName("0$foo"));
        assert_lex_error!("$", LexErrorKind::ReservedName("$"));
        assert_lex_error!("$ ;;", LexErrorKind::ReservedName("$"));
        assert_lex_error!("123p3", LexErrorKind::ReservedName("123p3"));
        assert_lex_error!("0x123p1f", LexErrorKind::ReservedName("0x123p1f"));
        assert_lex_error!("123e", LexErrorKind::ReservedName("123e"));
        assert_lex_error!("123e+", LexErrorKind::ReservedName("123e+"));
        assert_lex_error!("0x", LexErrorKind::ReservedName("0x"));
        assert_lex_error!("1_", LexErrorKind::ReservedName("1_"));
        assert_lex_error!("1__2", LexErrorKind::ReservedName("1__2"));
        assert_lex_error!("1.2_", LexErrorKind::ReservedName("1.2_"));
        assert_lex_error!("1._2", LexErrorKind::ReservedName("1._2"));
        assert_lex_error!("1.2__3", LexErrorKind::ReservedName("1.2__3"));
        assert_lex_error!("1.E2_", LexErrorKind::ReservedName("1.E2_"));
        assert_lex_error!("1.E_2", LexErrorKind::ReservedName("1.E_2"));
        assert_lex_error!("1.E2__3", LexErrorKind::ReservedName("1.E2__3"));
    }

    #[test]
    fn integers() {
        assert_lex_one!("1", Token::Int(Sign::Plus, NumBase::Dec, "1"));
        assert_lex_one!("123", Token::Int(Sign::Plus, NumBase::Dec, "123"));
        assert_lex_one!("1_2_3", Token::Int(Sign::Plus, NumBase::Dec, "1_2_3"));
        assert_lex_one!("+1", Token::Int(Sign::Plus, NumBase::Dec, "1"));
        assert_lex_one!("+123", Token::Int(Sign::Plus, NumBase::Dec, "123"));
        assert_lex_one!("-1", Token::Int(Sign::Minus, NumBase::Dec, "1"));
        assert_lex_one!("-123", Token::Int(Sign::Minus, NumBase::Dec, "123"));
        assert_lex_one!("0xd", Token::Int(Sign::Plus, NumBase::Hex, "d"));
        assert_lex_one!("0xc0ffee", Token::Int(Sign::Plus, NumBase::Hex, "c0ffee"));
        assert_lex_one!("+0xd", Token::Int(Sign::Plus, NumBase::Hex, "d"));
        assert_lex_one!("+0xc0ffee", Token::Int(Sign::Plus, NumBase::Hex, "c0ffee"));
        assert_lex_one!("-0xd", Token::Int(Sign::Minus, NumBase::Hex, "d"));
        assert_lex_one!("-0xc0ffee", Token::Int(Sign::Minus, NumBase::Hex, "c0ffee"));
    }

    #[test]
    fn floats() {
        assert_lex_one!(
            "123.",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "123.",
                    exp: None,
                }
            )
        );
        assert_lex_one!(
            "123.456",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "123.456",
                    exp: None,
                }
            )
        );
        assert_lex_one!(
            "+123.",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "123.",
                    exp: None,
                }
            )
        );
        assert_lex_one!(
            "-123.",
            Token::Float(
                Sign::Minus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "123.",
                    exp: None,
                }
            )
        );
        assert_lex_one!(
            "123.e10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "123.",
                    exp: Some((Sign::Plus, "10")),
                }
            )
        );
        assert_lex_one!(
            "123.456e10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "123.456",
                    exp: Some((Sign::Plus, "10")),
                }
            )
        );
        assert_lex_one!(
            "1_2_3.4_5_6e1_0",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "1_2_3.4_5_6",
                    exp: Some((Sign::Plus, "1_0")),
                }
            )
        );
        assert_lex_one!(
            "123.E10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "123.",
                    exp: Some((Sign::Plus, "10")),
                }
            )
        );
        assert_lex_one!(
            "123.e+10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "123.",
                    exp: Some((Sign::Plus, "10")),
                }
            )
        );
        assert_lex_one!(
            "123.e-10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Dec,
                    frac: "123.",
                    exp: Some((Sign::Minus, "10")),
                }
            )
        );

        assert_lex_one!(
            "0xc0f.",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c0f.",
                    exp: None,
                }
            )
        );
        assert_lex_one!(
            "0xc0f.fee",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c0f.fee",
                    exp: None,
                }
            )
        );
        assert_lex_one!(
            "+0xc0f.",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c0f.",
                    exp: None,
                }
            )
        );
        assert_lex_one!(
            "-0xc0f.",
            Token::Float(
                Sign::Minus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c0f.",
                    exp: None,
                }
            )
        );
        assert_lex_one!(
            "0xc0f.p10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c0f.",
                    exp: Some((Sign::Plus, "10")),
                }
            )
        );
        assert_lex_one!(
            "0xc0f.feep10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c0f.fee",
                    exp: Some((Sign::Plus, "10")),
                }
            )
        );
        assert_lex_one!(
            "0xc_0_f.f_e_ep1_0",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c_0_f.f_e_e",
                    exp: Some((Sign::Plus, "1_0")),
                }
            )
        );
        assert_lex_one!(
            "0xc0f.feeP10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c0f.fee",
                    exp: Some((Sign::Plus, "10")),
                }
            )
        );
        assert_lex_one!(
            "0xc0f.p+10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c0f.",
                    exp: Some((Sign::Plus, "10")),
                }
            )
        );
        assert_lex_one!(
            "0xc0f.p-10",
            Token::Float(
                Sign::Plus,
                Float::Val {
                    base: NumBase::Hex,
                    frac: "c0f.",
                    exp: Some((Sign::Minus, "10")),
                }
            )
        );

        assert_lex_one!("inf", Token::Float(Sign::Plus, Float::Inf));
        assert_lex_one!("+inf", Token::Float(Sign::Plus, Float::Inf));
        assert_lex_one!("-inf", Token::Float(Sign::Minus, Float::Inf));
        assert_lex_one!("nan", Token::Float(Sign::Plus, Float::Nan(None)));
        assert_lex_one!("+nan", Token::Float(Sign::Plus, Float::Nan(None)));
        assert_lex_one!("-nan", Token::Float(Sign::Minus, Float::Nan(None)));
        assert_lex_one!("nan:0x1f", Token::Float(Sign::Plus, Float::Nan(Some("1f"))));
        assert_lex_one!("nan:0x1_f", Token::Float(Sign::Plus, Float::Nan(Some("1_f"))));
        assert_lex_one!("+nan:0x1f", Token::Float(Sign::Plus, Float::Nan(Some("1f"))));
        assert_lex_one!("-nan:0x1f", Token::Float(Sign::Minus, Float::Nan(Some("1f"))));
    }

    #[test]
    fn unexpected_characters() {
        // '[' is a reserved character and cannot appear in wat for now
        assert_lex_error!("[", LexErrorKind::UnexpectedCharacter('['));
        assert_lex_error!(" [", LexErrorKind::UnexpectedCharacter('['));
        assert_lex_error!("(;_;) [", LexErrorKind::UnexpectedCharacter('['));
        assert_lex_error!(";;\n[", LexErrorKind::UnexpectedCharacter('['));
    }

    #[test]
    fn hello_world() {
        let input = r#"
(module
 (type $i32_=>_none (func (param i32)))
 (type $none_=>_i32 (func (result i32)))
 (import "env" "print" (func $print (param i32)))
 (memory $0 2)
 (data (i32.const 1024) "Hello, world\n\00")
 (table $0 1 1 funcref)
 (global $global$0 (mut i32) (i32.const 66576))
 (export "memory" (memory $0))
 (export "_start" (func $_start))
 (func $_start (; 1 ;) (result i32)
  (call $print
   (i32.const 1024)
  )
  (i32.const 0)
 )
 ;; custom section "producers", size 27
)
        "#;
        let tokens = lex_all(input).unwrap();
        let tokens: Vec<_> = tokens.into_iter().map(|(t, _)| t).collect();
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Keyword("module"),
                Token::LParen,
                Token::Keyword("type"),
                Token::Ident("$i32_=>_none"),
                Token::LParen,
                Token::Keyword("func"),
                Token::LParen,
                Token::Keyword("param"),
                Token::Keyword("i32"),
                Token::RParen,
                Token::RParen,
                Token::RParen,
                Token::LParen,
                Token::Keyword("type"),
                Token::Ident("$none_=>_i32"),
                Token::LParen,
                Token::Keyword("func"),
                Token::LParen,
                Token::Keyword("result"),
                Token::Keyword("i32"),
                Token::RParen,
                Token::RParen,
                Token::RParen,
                Token::LParen,
                Token::Keyword("import"),
                Token::String(Cow::Borrowed(b"env"), r#""env""#),
                Token::String(Cow::Borrowed(b"print"), r#""print""#),
                Token::LParen,
                Token::Keyword("func"),
                Token::Ident("$print"),
                Token::LParen,
                Token::Keyword("param"),
                Token::Keyword("i32"),
                Token::RParen,
                Token::RParen,
                Token::RParen,
                Token::LParen,
                Token::Keyword("memory"),
                Token::Ident("$0"),
                Token::Int(Sign::Plus, NumBase::Dec, "2"),
                Token::RParen,
                Token::LParen,
                Token::Keyword("data"),
                Token::LParen,
                Token::Keyword("i32.const"),
                Token::Int(Sign::Plus, NumBase::Dec, "1024"),
                Token::RParen,
                Token::String(Cow::Borrowed(b"Hello, world\n\x00"), r#""Hello, world\n\00""#),
                Token::RParen,
                Token::LParen,
                Token::Keyword("table"),
                Token::Ident("$0"),
                Token::Int(Sign::Plus, NumBase::Dec, "1"),
                Token::Int(Sign::Plus, NumBase::Dec, "1"),
                Token::Keyword("funcref"),
                Token::RParen,
                Token::LParen,
                Token::Keyword("global"),
                Token::Ident("$global$0"),
                Token::LParen,
                Token::Keyword("mut"),
                Token::Keyword("i32"),
                Token::RParen,
                Token::LParen,
                Token::Keyword("i32.const"),
                Token::Int(Sign::Plus, NumBase::Dec, "66576"),
                Token::RParen,
                Token::RParen,
                Token::LParen,
                Token::Keyword("export"),
                Token::String(Cow::Borrowed(b"memory"), r#""memory""#),
                Token::LParen,
                Token::Keyword("memory"),
                Token::Ident("$0"),
                Token::RParen,
                Token::RParen,
                Token::LParen,
                Token::Keyword("export"),
                Token::String(Cow::Borrowed(b"_start"), r#""_start""#),
                Token::LParen,
                Token::Keyword("func"),
                Token::Ident("$_start"),
                Token::RParen,
                Token::RParen,
                Token::LParen,
                Token::Keyword("func"),
                Token::Ident("$_start"),
                Token::LParen,
                Token::Keyword("result"),
                Token::Keyword("i32"),
                Token::RParen,
                Token::LParen,
                Token::Keyword("call"),
                Token::Ident("$print"),
                Token::LParen,
                Token::Keyword("i32.const"),
                Token::Int(Sign::Plus, NumBase::Dec, "1024"),
                Token::RParen,
                Token::RParen,
                Token::LParen,
                Token::Keyword("i32.const"),
                Token::Int(Sign::Plus, NumBase::Dec, "0"),
                Token::RParen,
                Token::RParen,
                Token::RParen,
            ]
        );
    }

    #[test]
    fn apply_sign() {
        assert_eq!(Sign::Plus.apply(42), 42);
        assert_eq!(Sign::Plus.apply(-42), -42);
        assert_eq!(Sign::Plus.apply(1.0), 1.0);
        assert_eq!(Sign::Plus.apply(-1.0), -1.0);
        assert_eq!(Sign::Minus.apply(42), -42);
        assert_eq!(Sign::Minus.apply(-42), 42);
        assert_eq!(Sign::Minus.apply(1.0), -1.0);
        assert_eq!(Sign::Minus.apply(-1.0), 1.0);
    }
}
