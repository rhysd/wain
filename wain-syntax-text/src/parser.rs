use crate::ast::*;
use crate::lexer::{Float, LexError, Lexer, NumBase, Sign, Token};
use crate::source::describe_position;
use std::borrow::Cow;
use std::char;
use std::collections::HashMap;
use std::f32;
use std::f64;
use std::fmt;
use std::mem;
use std::ops;
use std::str;
use std::str::FromStr;

#[cfg_attr(test, derive(Debug))]
pub enum ParseErrorKind<'source> {
    LexError(LexError<'source>),
    UnexpectedToken {
        got: Token<'source>,
        expected: &'static str, // TODO: Make 'expected' better
    },
    UnexpectedEndOfFile {
        expected: &'static str, // TODO: Make 'expected' better
    },
    UnexpectedKeyword(&'source str),
    InvalidValType(&'source str),
    MalformedUtf8Encoding,
    MissingParen {
        paren: char,
        got: Option<Token<'source>>,
        what: &'source str,
    },
    InvalidOperand {
        insn: &'static str,
        msg: &'static str,
    },
    NumberMustBePositive(NumBase, &'source str),
    CannotParseNum {
        reason: String,
        ty: &'static str,
    },
    MultipleEntrypoints(Start<'source>, Start<'source>),
    IdAlreadyDefined {
        id: &'source str,
        prev_idx: u32,
        what: &'static str,
        scope: &'static str,
    },
    ExpectEndOfFile {
        token: Token<'source>,
        after: &'static str,
    },
    ImportMustPrecedeOtherDefs {
        what: &'static str,
    },
    InvalidAlignment(u32),
    IdBoundToParam(&'source str),
}

#[cfg_attr(test, derive(Debug))]
pub struct ParseError<'s> {
    kind: ParseErrorKind<'s>,
    offset: usize,
    source: &'s str,
}

impl<'s> ParseError<'s> {
    fn new(kind: ParseErrorKind<'s>, offset: usize, source: &'s str) -> Box<ParseError<'s>> {
        Box::new(ParseError { kind, offset, source })
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn source(&self) -> &'s str {
        self.source
    }
}

impl<'s> fmt::Display for ParseError<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParseErrorKind::*;
        match &self.kind {
            LexError(err) => return err.fmt(f),
            UnexpectedToken { got, expected } => write!(f, "expected {} but got {}", expected, got)?,
            UnexpectedEndOfFile { expected } => write!(f, "expected {} but reached EOF", expected)?,
            UnexpectedKeyword(kw) => write!(f, "unexpected keyword '{}'", kw)?,
            InvalidValType(ty) => write!(
                f,
                "value type must be one of 'i32', 'i64', 'f32', 'f64' but got '{}'",
                ty
            )?,
            MalformedUtf8Encoding => write!(f, "Name must be valid UTF-8 encoding characters")?,
            NumberMustBePositive(base, s) => write!(f, "number must be positive but got -{}{}", base.prefix(), s)?,
            MissingParen {
                paren,
                got: Some(tok),
                what,
            } => write!(f, "expected paren '{}' for {} but got {}", paren, what, tok)?,
            MissingParen { paren, got: None, what } => {
                write!(f, "expected paren '{}' for {} but reached EOF", paren, what)?
            }
            InvalidOperand { insn, msg } => write!(f, "invalid operand for '{}' instruction: {}", insn, msg)?,
            CannotParseNum { reason, ty } => write!(f, "cannot parse {} number: {}", ty, reason)?,
            MultipleEntrypoints(prev, cur) => write!(
                f,
                "module cannot contain multiple 'start' functions {}. previous start function was {} at offset {}",
                cur.idx, prev.idx, prev.start
            )?,
            IdAlreadyDefined {
                id,
                prev_idx,
                what,
                scope,
            } => write!(
                f,
                "identifier '{}' for {} is already defined for index {} in the {}",
                id, what, prev_idx, scope
            )?,
            ExpectEndOfFile { after, token } => write!(f, "expect EOF but got {} after parsing {}", token, after)?,
            ImportMustPrecedeOtherDefs { what } => write!(
                f,
                "import {} must be put before other function, memory, table and global definitions",
                what
            )?,
            InvalidAlignment(align) => write!(f, "alignment must be power of two but got {}", align)?,
            IdBoundToParam(id) => write!(f, "id '{}' must not be bound to parameter of call_indirect", id)?,
        };

        describe_position(f, self.source, self.offset)
    }
}

impl<'s> From<Box<LexError<'s>>> for Box<ParseError<'s>> {
    fn from(err: Box<LexError<'s>>) -> Box<ParseError<'s>> {
        Box::new(ParseError {
            source: err.source(),
            offset: err.offset(),
            kind: ParseErrorKind::LexError(*err),
        })
    }
}

type Result<'s, T> = ::std::result::Result<T, Box<ParseError<'s>>>;

// iter::Peekable is not sufficient to parse WAT tokens
// WAT requires LL(1) parser to see a token after '('
#[derive(Clone)]
pub struct LookAhead<I: Iterator> {
    it: I,
    current: Option<I::Item>,
    incoming: Option<I::Item>,
}

impl<I: Iterator> LookAhead<I> {
    pub fn new(mut it: I) -> Self {
        let current = it.next();
        let incoming = it.next();
        LookAhead { it, current, incoming }
    }
    pub fn peek(&self) -> Option<&I::Item> {
        self.current.as_ref()
    }
    pub fn lookahead(&self) -> Option<&I::Item> {
        self.incoming.as_ref()
    }
    pub fn inner(&self) -> &I {
        &self.it
    }
}

impl<I: Iterator> Iterator for LookAhead<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        // This implementation can be better. We can delay calling self.it.next() until it is really
        // necessary. In the case, peek() and lookahead() will be `&mut self` method.
        mem::replace(&mut self.current, mem::replace(&mut self.incoming, self.it.next()))
    }
}

struct Indices<'s> {
    source: &'s str,
    next_idx: u32,
    indices: HashMap<&'s str, u32>,
    what: &'static str,
    scope: &'static str,
}

impl<'s> Indices<'s> {
    fn new(source: &'s str, what: &'static str, scope: &'static str) -> Self {
        Self {
            source,
            next_idx: 0,
            indices: HashMap::new(),
            what,
            scope,
        }
    }

    fn error<T>(&self, kind: ParseErrorKind<'s>, offset: usize) -> Result<'s, T> {
        Err(ParseError::new(kind, offset, self.source))
    }

    fn new_idx(&mut self, id: Option<&'s str>, offset: usize) -> Result<'s, u32> {
        let idx = self.next_idx;
        if let Some(id) = id {
            if let Some(prev_idx) = self.indices.insert(id, idx) {
                return self.error(
                    ParseErrorKind::IdAlreadyDefined {
                        id,
                        prev_idx,
                        what: self.what,
                        scope: self.scope,
                    },
                    offset,
                );
            }
        }
        self.next_idx += 1;
        Ok(idx)
    }

    fn move_out(&mut self) -> HashMap<&'s str, u32> {
        self.next_idx = 0; // Clear next index for parsing next module
        mem::take(&mut self.indices)
    }
}

// TODO: Remember offset of place where the identifier is defined for better error
struct ParseContext<'s> {
    // types are put in Parser context due to abbreviation of typeuse
    // https://webassembly.github.io/spec/core/text/modules.html#abbreviations
    types: Vec<TypeDef<'s>>,
    exports: Vec<Export<'s>>,
    // Indices for each spaces.
    // https://webassembly.github.io/spec/core/syntax/modules.html#syntax-index
    type_indices: Indices<'s>,
    func_indices: Indices<'s>,
    table_indices: Indices<'s>,
    mem_indices: Indices<'s>,
    global_indices: Indices<'s>,
    implicit_type_uses: Vec<ImplicitTypeUse<'s>>,
}

impl<'s> ParseContext<'s> {
    fn new(source: &'s str) -> Self {
        ParseContext {
            types: vec![],
            exports: vec![],
            type_indices: Indices::new(source, "type", "module"),
            func_indices: Indices::new(source, "func", "module"),
            table_indices: Indices::new(source, "table", "module"),
            mem_indices: Indices::new(source, "memory", "module"),
            global_indices: Indices::new(source, "global", "module"),
            implicit_type_uses: vec![],
        }
    }
}

trait IsInfinite: Copy {
    fn is_infinite(self) -> bool;
}

impl IsInfinite for f32 {
    fn is_infinite(self) -> bool {
        self.is_infinite()
    }
}

impl IsInfinite for f64 {
    fn is_infinite(self) -> bool {
        self.is_infinite()
    }
}

// TODO: Add index-to-id tables for types, funcs, tables, mems, globals, locals and labels
// https://webassembly.github.io/spec/core/text/modules.html#indices
pub struct Parser<'s> {
    source: &'s str,
    tokens: LookAhead<Lexer<'s>>,
    ctx: ParseContext<'s>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        Parser {
            source,
            tokens: LookAhead::new(Lexer::new(source)),
            ctx: ParseContext::new(source),
        }
    }

    pub fn with_lexer(lexer: LookAhead<Lexer<'s>>) -> Self {
        Parser {
            source: lexer.inner().source(),
            ctx: ParseContext::new(lexer.inner().source()),
            tokens: lexer,
        }
    }

    pub fn source(&self) -> &'s str {
        self.source
    }

    pub fn into_lexer(self) -> LookAhead<Lexer<'s>> {
        self.tokens
    }

    pub fn is_done(&self) -> bool {
        self.tokens.peek().is_none()
    }

    pub fn parse_wat(mut self) -> Result<'s, Parsed<'s>> {
        self.parse()
    }

    pub fn parse<P: Parse<'s>>(&mut self) -> Result<'s, P> {
        Parse::<'s>::parse(self)
    }

    fn error<T>(&self, kind: ParseErrorKind<'s>, offset: usize) -> Result<'s, T> {
        Err(ParseError::new(kind, offset, self.source))
    }

    fn unexpected_token<T>(&self, got: Token<'s>, expected: &'static str, offset: usize) -> Result<'s, T> {
        self.error(ParseErrorKind::UnexpectedToken { got, expected }, offset)
    }

    fn unexpected_eof<T>(&self, expected: &'static str) -> Result<'s, T> {
        self.error(ParseErrorKind::UnexpectedEndOfFile { expected }, self.source().len())
    }

    fn cannot_parse_num<T, S: ToString>(&self, ty: &'static str, reason: S, offset: usize) -> Result<'s, T> {
        self.error(
            ParseErrorKind::CannotParseNum {
                reason: reason.to_string(),
                ty,
            },
            offset,
        )
    }

    fn maybe_eof<'p>(
        &'p self,
        lexed: Option<&'p <Lexer<'s> as Iterator>::Item>,
        expected: &'static str,
    ) -> Result<'s, (&Token<'s>, usize)> {
        match lexed {
            Some(Ok((tok, offset))) => Ok((tok, *offset)),
            Some(Err(err)) => Err(err.clone().into()),
            None => self.unexpected_eof(expected),
        }
    }

    fn lookahead(&self, expected: &'static str) -> Result<'s, (&Token<'s>, usize)> {
        self.maybe_eof(self.tokens.lookahead(), expected)
    }

    fn peek(&self, expected: &'static str) -> Result<'s, (&Token<'s>, usize)> {
        self.maybe_eof(self.tokens.peek(), expected)
    }

    pub fn current_pos(&self) -> Result<'s, Option<usize>> {
        match self.tokens.peek() {
            Some(Ok((_, pos))) => Ok(Some(*pos)),
            Some(Err(err)) => Err(err.clone().into()),
            None => Ok(None),
        }
    }

    fn next_token(&mut self, expected: &'static str) -> Result<'s, (Token<'s>, usize)> {
        match self.tokens.next() {
            Some(lexed) => Ok(lexed?),
            None => self.unexpected_eof(expected),
        }
    }

    fn eat_token(&mut self) -> bool {
        self.tokens.next().is_some()
    }

    fn peek_fold_start(&self, expected: &'static str) -> Result<'s, (Option<&'s str>, usize)> {
        match self.peek(expected)? {
            (Token::LParen, offset) => match self.lookahead(expected)? {
                (Token::Keyword(kw), _) => Ok((Some(*kw), offset)),
                (tok, offset) => self.unexpected_token(tok.clone(), expected, offset),
            },
            (_, offset) => Ok((None, offset)),
        }
    }

    fn maybe_ident(&mut self, expected: &'static str) -> Result<'s, Option<&'s str>> {
        Ok(match self.peek(expected)? {
            (Token::Ident(id), _) => {
                let id = *id;
                self.eat_token();
                Some(id)
            }
            _ => None,
        })
    }

    fn missing_paren<T>(
        &mut self,
        paren: char,
        got: Option<Token<'s>>,
        what: &'static str,
        offset: usize,
    ) -> Result<'s, T> {
        self.error(ParseErrorKind::MissingParen { paren, got, what }, offset)
    }

    fn opening_paren(&mut self, what: &'static str) -> Result<'s, usize> {
        if let Some(lexed) = self.tokens.next() {
            match lexed? {
                (Token::LParen, offset) => Ok(offset),
                (tok, offset) => self.missing_paren('(', Some(tok), what, offset),
            }
        } else {
            self.missing_paren('(', None, what, self.source.len())
        }
    }

    fn closing_paren(&mut self, what: &'static str) -> Result<'s, usize> {
        if let Some(lexed) = self.tokens.next() {
            match lexed? {
                (Token::RParen, offset) => Ok(offset),
                (tok, offset) => self.missing_paren(')', Some(tok), what, offset),
            }
        } else {
            self.missing_paren(')', None, what, self.source.len())
        }
    }

    fn parse_u32(&mut self, expected: &'static str) -> Result<'s, u32> {
        match self.next_token(expected)? {
            (Token::Int(Sign::Minus, base, s), offset) => {
                self.error(ParseErrorKind::NumberMustBePositive(base, s), offset)
            }
            (Token::Int(Sign::Plus, base, s), offset) => parse_u32_str(self, s, base, offset),
            (tok, offset) => self.unexpected_token(tok.clone(), expected, offset),
        }
    }

    fn parse_dec_float<F>(
        &self,
        sign: Sign,
        frac: &'s str,
        exp: Option<(Sign, &'s str)>,
        offset: usize,
    ) -> Result<'s, F>
    where
        F: FromStr + ops::Neg<Output = F> + IsInfinite,
        F::Err: fmt::Display,
    {
        // TODO: Implement parsing floating number literals without allocation
        let mut s: String = frac.chars().filter(|c| *c != '_').collect();
        if let Some((sign, exp)) = exp {
            s.push('e');
            if sign == Sign::Minus {
                s.push('-');
            }
            for c in exp.chars().filter(|c| *c != '_') {
                s.push(c);
            }
        }
        match s.parse::<F>() {
            Ok(f) if f.is_infinite() => self.cannot_parse_num("float", "float constant out of range", offset),
            Ok(f) => Ok(sign.apply(f)),
            Err(e) => self.cannot_parse_num("float", format!("{}", e), offset),
        }
    }

    fn parse_mem_arg(&mut self, default_align: u32) -> Result<'s, Mem> {
        fn base_and_digits(s: &str) -> (NumBase, &'_ str) {
            if let Some(hex) = s.strip_prefix("0x") {
                (NumBase::Hex, hex)
            } else {
                (NumBase::Dec, s)
            }
        }

        let offset = match self.peek("'offset' keyword for memory instruction")? {
            (Token::Keyword(kw), offset) if kw.starts_with("offset=") => {
                let (base, digits) = base_and_digits(&kw[7..]);
                let u = parse_u32_str(self, digits, base, offset)?;
                self.eat_token(); // Eat 'offset' keyword
                u
            }
            _ => 0,
        };

        let align = match self.peek("'align' keyword for memory instruction")? {
            (Token::Keyword(kw), offset) if kw.starts_with("align=") => {
                let (base, digits) = base_and_digits(&kw[6..]);
                let u = parse_u32_str(self, digits, base, offset)?;
                if u.count_ones() != 1 {
                    return self.error(ParseErrorKind::InvalidAlignment(u), offset);
                }
                self.eat_token(); // Eat 'align' keyword
                u.trailing_zeros()
            }
            _ => default_align,
        };

        Ok(Mem { align, offset })
    }

    fn create_inline_typeuse(&mut self, start: usize, params: &[Param<'s>], results: &[FuncResult]) -> u32 {
        let idx = self.ctx.implicit_type_uses.len();
        self.ctx.implicit_type_uses.push(ImplicitTypeUse {
            start,
            params: params.to_vec(),
            results: results.to_vec(),
        });
        idx as u32
    }

    fn resolve_implicit_type_uses(&mut self) -> Vec<u32> {
        // Handle abbreviation:
        //   https://webassembly.github.io/spec/core/text/modules.html#abbreviations
        //
        // A typeuse may also be replaced entirely by inline parameter and result declarations.
        // In that case, a type index is automatically inserted.
        //
        //   {param}* {result}* == (type {x}) {param}* {result}*
        //
        // where {x} is an existing function type which has the same signature.
        // If no function exists, insert new function type in module
        //
        //   (type (func {param}* {result}*))
        //

        let mut resolved = Vec::with_capacity(self.ctx.implicit_type_uses.len());
        for ImplicitTypeUse { start, params, results } in mem::take(&mut self.ctx.implicit_type_uses).into_iter() {
            let idx = if let Some(idx) = self.ctx.types.iter().enumerate().find_map(|(i, TypeDef { ty, .. })| {
                if ty.params.iter().map(|t| t.ty).eq(params.iter().map(|t| t.ty))
                    && ty.results.iter().map(|t| t.ty).eq(results.iter().map(|t| t.ty))
                {
                    Some(i)
                } else {
                    None
                }
            }) {
                idx
            } else {
                self.ctx.types.push(TypeDef {
                    start,
                    id: None,
                    ty: FuncType { start, params, results },
                });
                self.ctx.types.len() - 1
            };
            resolved.push(idx as u32);
        }
        resolved
    }
}

// TODO: Use trait rather than macros to avoid duplication of implementations
macro_rules! parse_uint_function {
    ($name:ident, $uint:ty) => {
        fn $name<'s>(parser: &Parser<'s>, input: &'s str, base: NumBase, offset: usize) -> Result<'s, $uint> {
            let radix = base.radix();
            let mut ret: $uint = 0;
            for c in input.chars() {
                if c == '_' {
                    // Skip delimiter
                    continue;
                }
                if let Some(d) = c.to_digit(radix) {
                    if let Some(added) = ret
                        .checked_mul(radix as $uint)
                        .and_then(|i| i.checked_add(d as $uint))
                    {
                        ret = added;
                    } else {
                        return parser.cannot_parse_num(stringify!($uint), "too big integer", offset);
                    }
                } else {
                    return parser.cannot_parse_num(stringify!($uint), format!("invalid digit '{}'", c), offset);
                }
            }
            Ok(ret)
        }
    };
}

parse_uint_function!(parse_u32_str, u32);
parse_uint_function!(parse_u64_str, u64);

macro_rules! parse_hex_float_fn {
    ($name:ident, $float:ty, $uint:ty) => {
        fn $name<'s>(parser: &Parser<'s>, sign: Sign, m_str: &'s str, exp: Option<(Sign, &'s str)>, offset: usize) -> Result<'s, $float> {
            // Parse logic is explained in Japanese at https://github.com/rhysd/wain/pull/9/files#r429552186

            // Parse exponent part
            let mut temp_exp = if let Some((exp_sign, exp_str)) = exp {
                match parse_u32_str(parser, exp_str, NumBase::Dec, offset) {
                    Ok(exp) if exp_sign == Sign::Plus && exp as i32 >= 0 => exp as i32,
                    Ok(exp) if exp_sign == Sign::Minus && exp.wrapping_neg() as i32 <= 0 => exp.wrapping_neg() as i32,
                    _ => {
                        // Ok(exp) means u32::MAX <= exp < i32::MIN or i32::MAX < exp <= u32::MAX.
                        // Err(_) means exp_str is larger than u32::MAX because the lexer guarantees
                        // that invalid digit cannot occur.
                        return parser.cannot_parse_num(
                            stringify!($float),
                            format!("exponent value out of range '{}{}'", exp_sign, exp_str),
                            offset,
                        );
                    }
                }
            } else {
                0
            };

            const SIGNIFICAND_BITS: i32 = <$float>::MANTISSA_DIGITS as i32;

            // Parse significand as unsigned integer. and adjust exponent
            // For example, 0x123.456 is parsed into 0x123456p-12 (temp_sig = 0x123456 and temp_exp -= 12)
            let mut temp_sig: $uint = 0;
            let mut saw_dot = false;
            for c in m_str.chars() {
                match c {
                    '.' => saw_dot = true,
                    '_' => continue,
                    // There are not enough significant digits. It means that SIGNIFICAND_BITS bits
                    // are not filled yet in temp_sig
                    _ if temp_sig < 1 << SIGNIFICAND_BITS + 1 => {
                        // Shift significand & append significant digit
                        // .unwrap() assumes `c` has hexadecimal character thanks to lexer
                        temp_sig = (temp_sig << 4) | c.to_digit(16).unwrap() as $uint;
                        if saw_dot {
                            // Adjust exponent if fractional part (16=2^4)
                            temp_exp = temp_exp.saturating_sub(4);
                        }
                    }
                    // There are enough significant digits. It means that all SIGNIFICAND_BITS bits
                    // were filled. All digits after SIGNIFICAND_BITS are dropped. But only LSB of temp_sig
                    // needs be updated for rounding to nearest
                    _ => {
                        // Set lsb of significand to 1 if non-zero digit (for "round to nearest even")
                        temp_sig |= (c != '0') as $uint;
                        if !saw_dot {
                            // Adjust exponent if integer part
                            temp_exp = temp_exp.saturating_add(4);
                        }
                    }
                }
            }

            // Encode float bits
            if temp_sig != 0 {
                const BITS: i32 = (mem::size_of::<$float>() * 8) as i32;
                // For rounding to nearest, at least two more bits than actual fraction is necessary.
                // Since digits are hexadecimal, it requires 4 bits to read one digit. And 1 bit is
                // necessary to remember all bits are 1 or not to determine round up or round down.
                //
                // Explained at 2. of https://github.com/rhysd/wain/pull/9/files#r429552186
                const TEMP_SIG_BITS: i32 = SIGNIFICAND_BITS + 4 + 1;
                // This bias considers to adjust significand to IEEE 754 format. We compute the significand
                // as unsigned integer. But integer part is 1 bit in IEEE 754 format. (e.g. 0b1011101 -> 0b1.011101).
                // Since the unsigned integer significand has TEMP_SIG_BITS bits, the value should be multiplid
                // with 1 / 2^(TEMP_SIG_BITS - 1). It means adding TEMP_SIG_BITS - 1 to exp value.
                //
                // https://github.com/rhysd/wain/pull/9/files#r429528984
                const TEMP_EXP_BIAS: i32 = (<$float>::MAX_EXP - 1) + (TEMP_SIG_BITS - 1);
                // The same reason as above comment for TEMP_SIG_BITS - 1.
                // https://github.com/rhysd/wain/pull/9/files#r429529571
                const TEMP_MAX_EXP: i32 = (<$float>::MAX_EXP - 1) - (TEMP_SIG_BITS - 1);
                const TEMP_MIN_EXP: i32 = (<$float>::MIN_EXP - 1) - (TEMP_SIG_BITS - 1);

                if temp_sig < 1 << TEMP_SIG_BITS - 1 {
                    // Normalize significand and adjust exponent. Adjust temp_sig to use all
                    // TEMP_SIG_BITS bits.
                    // Explained at 3. in https://github.com/rhysd/wain/pull/9#discussion_r429552186
                    let shift = temp_sig.leading_zeros() as i32 - (BITS - TEMP_SIG_BITS);
                    temp_sig <<= shift;
                    temp_exp = temp_exp.saturating_sub(shift);
                }

                temp_sig = if TEMP_MIN_EXP <= temp_exp && temp_exp <= TEMP_MAX_EXP {
                    // In range of normal numbers or infinity
                    // Explained at 4.i. in https://github.com/rhysd/wain/pull/9#discussion_r429552186

                    // Mask "implicit" bit. Extract TEMP_SIG_BITS bits from temp_sig
                    temp_sig &= (1 << TEMP_SIG_BITS - 1) - 1;

                    // Round to nearest even
                    // 0x2f is 0b101111 and 0x10 is 0b001_0000.
                    // Lower 7 bits before/after this process is as follows:
                    //
                    //   000_0000 -> 000_0000 (round down): as-is
                    //   000_0001 -> 001_0001 (round down): + 0b001_0000
                    //   000_1111 -> 001_1111 (round down): + 0b001_0000
                    //   001_0000 -> 001_0000 (round down): as-is
                    //   001_0001 -> 010_0001 (round up)  : + 0b001_0000
                    //   001_1111 -> 010_1111 (round up)  : + 0b001_0000
                    //   010_0000 -> 011_0000 (round down): + 0b001_0000
                    //   010_0001 -> 011_0001 (round down): + 0b001_0000
                    //   010_1111 -> 011_1111 (round down): + 0b001_0000
                    //   011_0000 -> 100_0000 (round up)  : + 0b001_0000
                    //   011_0001 -> 100_0001 (round up)  : + 0b001_0000
                    //   011_1111 -> 100_1111 (round up)  : + 0b001_0000
                    //
                    // Note: We don't need to consider a carry of temp_sig because carried '1' is
                    // added to exponent part later
                    if (temp_sig & 0x2f) != 0 {
                        temp_sig += 0x10;
                    }

                    // Encode significand & biased exponent (it may be infinity)
                    (temp_sig >> 5) + ((temp_exp + TEMP_EXP_BIAS) as $uint << SIGNIFICAND_BITS - 1)
                } else if TEMP_MIN_EXP - SIGNIFICAND_BITS <= temp_exp && temp_exp < TEMP_MIN_EXP {
                    // Subnormal or zero
                    // Explained at 4.ii. in https://github.com/rhysd/wain/pull/9#discussion_r429552186

                    // Adjust significand to set exponent part to MIN_EXP - 1. Here, set 1 to LSB
                    // for rounding to nearest when the shifted bits contain 1
                    let shift = TEMP_MIN_EXP - temp_exp;
                    let lsb = ((temp_sig & (1 << shift) - 1) != 0) as $uint;
                    temp_sig = (temp_sig >> shift) | lsb;

                    // Round to nearest even
                    if (temp_sig & 0x2f) != 0 {
                        temp_sig += 0x10;
                    }

                    // Encode significand (biased exponent = 0). Note that significand may be zero
                    temp_sig >> 5
                } else if TEMP_MAX_EXP < temp_exp {
                    // Infinity
                    (1 << BITS - SIGNIFICAND_BITS) - 1 << SIGNIFICAND_BITS - 1
                } else {
                    // Zero
                    0
                };
            }

            let f = <$float>::from_bits(temp_sig);
            if f.is_infinite() {
                return parser.cannot_parse_num(
                    stringify!($float),
                    "float constant out of range",
                    offset,
                );
            }

            // Set sign bit
            Ok(sign.apply(f))
        }
    };
}

parse_hex_float_fn!(parse_hex_float_f32, f32, u32);
parse_hex_float_fn!(parse_hex_float_f64, f64, u64);

macro_rules! match_token {
    ($parser:expr, $expected:expr, $pattern:pat => $ret:expr) => {
        match $parser.next_token($expected)? {
            ($pattern, offset) => ($ret, offset),
            (tok, offset) => {
                return $parser.unexpected_token(tok, $expected, offset);
            }
        }
    };
    ($parser:expr, $expected:expr, $pattern:pat) => {
        match_token!($parser, $expected, $pattern => ())
    };
}

pub trait Parse<'s>: Sized {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self>;
}

// https://webassembly.github.io/spec/core/text/modules.html
impl<'s> Parse<'s> for Parsed<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        Ok(Parsed {
            module: parser.parse()?,
            type_indices: parser.ctx.type_indices.move_out(),
            func_indices: parser.ctx.func_indices.move_out(),
            table_indices: parser.ctx.table_indices.move_out(),
            mem_indices: parser.ctx.mem_indices.move_out(),
            global_indices: parser.ctx.global_indices.move_out(),
        })
    }
}

// Helper enum to parse module field
//
// https://webassembly.github.io/spec/core/text/modules.html#text-modulefield
#[cfg_attr(test, derive(Debug))]
enum ModuleField<'s> {
    Type(TypeDef<'s>),
    Import(ImportItem<'s>),
    Export(Export<'s>),
    Func(Func<'s>),
    Elem(Elem<'s>),
    Table(TableAbbrev<'s>),
    Data(Data<'s>),
    Memory(MemoryAbbrev<'s>),
    Global(Global<'s>),
    Start(Start<'s>),
}

// https://webassembly.github.io/spec/core/text/modules.html#text-module
impl<'s> Parse<'s> for Module<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        // Note: Abbreviation
        // In a source file, the toplevel (module ...) surrounding the module body may be omitted.
        // {modulefield}* == (module {modulefield}*)

        // XXX: By this, at least 2 tokens are necessary to parse module but empty string should be
        // valid for module with abbreviation.
        let (keyword, start) = parser.peek_fold_start("module")?;
        let abbreviated = keyword != Some("module");
        if !abbreviated {
            parser.eat_token(); // eat '('
            parser.eat_token(); // eat 'module
        }

        let id = parser.maybe_ident("identifier for module")?;

        let mut funcs = vec![];
        let mut elems = vec![];
        let mut tables = vec![];
        let mut data = vec![];
        let mut memories = vec![];
        let mut globals = vec![];
        let mut entrypoint = None;

        // Any import must be put before other definitions because indices of imports must precede
        // indices of other definitions
        //
        // https://webassembly.github.io/spec/core/syntax/modules.html#indices
        //
        // > The index space for functions, tables, memories and globals includes respective imports
        // > declared in the same module. The indices of these imports precede the indices of other
        // > definitions in the same index space.
        let mut can_import = true;

        // Note: types are put in Parser struct field due to abbreviation of typeuse
        // https://webassembly.github.io/spec/core/text/modules.html#abbreviations
        parser.ctx.types.clear();
        parser.ctx.exports.clear();

        loop {
            match parser.tokens.peek() {
                Some(Ok((Token::LParen, _))) => {} // fallthrough
                Some(Ok(_)) => break,
                Some(Err(err)) => return Err(err.clone().into()),
                None if abbreviated => break, // with abbreviation, it may reach EOF
                None => return parser.unexpected_eof("opening paren for starting module field"),
            }

            match parser.parse()? {
                ModuleField::Type(ty) => parser.ctx.types.push(ty),
                ModuleField::Import(ImportItem::Func(func)) if !can_import => {
                    return parser.error(
                        ParseErrorKind::ImportMustPrecedeOtherDefs { what: "function" },
                        func.start,
                    );
                }
                ModuleField::Import(ImportItem::Func(func)) => funcs.push(func),
                ModuleField::Import(ImportItem::Table(table)) if !can_import => {
                    return parser.error(
                        ParseErrorKind::ImportMustPrecedeOtherDefs { what: "table" },
                        table.start,
                    );
                }
                ModuleField::Import(ImportItem::Table(table)) => tables.push(table),
                ModuleField::Import(ImportItem::Memory(memory)) if !can_import => {
                    return parser.error(
                        ParseErrorKind::ImportMustPrecedeOtherDefs { what: "memory" },
                        memory.start,
                    );
                }
                ModuleField::Import(ImportItem::Memory(memory)) => memories.push(memory),
                ModuleField::Import(ImportItem::Global(global)) if !can_import => {
                    return parser.error(
                        ParseErrorKind::ImportMustPrecedeOtherDefs { what: "global" },
                        global.start,
                    );
                }
                ModuleField::Import(ImportItem::Global(global)) => globals.push(global),
                ModuleField::Export(export) => parser.ctx.exports.push(export),
                ModuleField::Func(func) => {
                    if let FuncKind::Body { .. } = func.kind {
                        can_import = false;
                    } else if !can_import {
                        return parser.error(
                            ParseErrorKind::ImportMustPrecedeOtherDefs { what: "function" },
                            func.start,
                        );
                    }
                    funcs.push(func);
                }
                ModuleField::Elem(elem) => elems.push(elem),
                ModuleField::Table(TableAbbrev::Table(table)) => {
                    if table.import.is_none() {
                        can_import = false;
                    } else if !can_import {
                        return parser.error(
                            ParseErrorKind::ImportMustPrecedeOtherDefs { what: "table" },
                            table.start,
                        );
                    }
                    tables.push(table);
                }
                ModuleField::Table(TableAbbrev::Elem(table, elem)) => {
                    if table.import.is_none() {
                        can_import = false;
                    } else if !can_import {
                        return parser.error(
                            ParseErrorKind::ImportMustPrecedeOtherDefs { what: "table" },
                            table.start,
                        );
                    }
                    tables.push(table);
                    elems.push(elem);
                }
                ModuleField::Data(d) => data.push(d),
                ModuleField::Memory(MemoryAbbrev::Memory(m)) => {
                    if m.import.is_none() {
                        can_import = false;
                    } else if !can_import {
                        return parser.error(ParseErrorKind::ImportMustPrecedeOtherDefs { what: "memory" }, m.start);
                    }
                    memories.push(m);
                }
                ModuleField::Memory(MemoryAbbrev::Data(m, d)) => {
                    if m.import.is_none() {
                        can_import = false;
                    } else if !can_import {
                        return parser.error(ParseErrorKind::ImportMustPrecedeOtherDefs { what: "memory" }, m.start);
                    }
                    memories.push(m);
                    data.push(d);
                }
                ModuleField::Global(global) => {
                    if let GlobalKind::Init(_) = global.kind {
                        can_import = false;
                    } else if !can_import {
                        return parser.error(
                            ParseErrorKind::ImportMustPrecedeOtherDefs { what: "global" },
                            global.start,
                        );
                    }
                    globals.push(global);
                }
                ModuleField::Start(start) => {
                    if let Some(prev) = entrypoint {
                        let offset = start.start;
                        return parser.error(ParseErrorKind::MultipleEntrypoints(prev, start), offset);
                    } else {
                        entrypoint = Some(start);
                    }
                }
            }
        }

        if abbreviated {
            if let Some(Ok((token, offset))) = parser.tokens.peek() {
                return parser.error(
                    ParseErrorKind::ExpectEndOfFile {
                        token: token.clone(),
                        after: "abbreviated module",
                    },
                    *offset,
                );
            }
        } else {
            parser.closing_paren("module")?;
        }

        Ok(Module {
            start,
            id,
            implicit_type_uses: parser.resolve_implicit_type_uses(), // This consumes parser.ctx.implicit_type_uses
            types: mem::take(&mut parser.ctx.types),
            exports: mem::take(&mut parser.ctx.exports),
            funcs,
            elems,
            tables,
            data,
            memories,
            globals,
            entrypoint,
        })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-modulefield
impl<'s> Parse<'s> for ModuleField<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let expected = "one of 'type', 'import', 'export', 'func', 'elem', 'table', 'data', 'memory', 'global', 'start' sections in module";
        match parser.peek_fold_start(expected)? {
            (Some(kw), offset) => match kw {
                "type" => Ok(ModuleField::Type(parser.parse()?)),
                "import" => Ok(ModuleField::Import(parser.parse()?)),
                "export" => Ok(ModuleField::Export(parser.parse()?)),
                "func" => Ok(ModuleField::Func(parser.parse()?)),
                "elem" => Ok(ModuleField::Elem(parser.parse()?)),
                "table" => Ok(ModuleField::Table(parser.parse()?)),
                "data" => Ok(ModuleField::Data(parser.parse()?)),
                "memory" => Ok(ModuleField::Memory(parser.parse()?)),
                "global" => Ok(ModuleField::Global(parser.parse()?)),
                "start" => Ok(ModuleField::Start(parser.parse()?)),
                _ => parser.error(ParseErrorKind::UnexpectedKeyword(kw), offset),
            },
            (None, offset) => {
                let token = parser.peek(expected)?.0.clone();
                parser.unexpected_token(token, expected, offset)
            }
        }
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-typedef
impl<'s> Parse<'s> for TypeDef<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("type")?;
        match_token!(parser, "'type' keyword", Token::Keyword("type"));
        let id = parser.maybe_ident("identifier for type")?;
        let ty = parser.parse()?;
        parser.closing_paren("type")?;
        parser.ctx.type_indices.new_idx(id, start)?;
        Ok(TypeDef { start, id, ty })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-functype
impl<'s> Parse<'s> for FuncType<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("function type")?;
        match_token!(parser, "'func' keyword", Token::Keyword("func"));

        let params = parser.parse()?;
        let results = parser.parse()?;

        parser.closing_paren("function type")?;
        Ok(FuncType { start, params, results })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-param
// Not impl for Param<'s> considering abbreviation
impl<'s> Parse<'s> for Vec<Param<'s>> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let mut params = vec![];
        while let (Some("param"), start) = parser.peek_fold_start("parameter")? {
            parser.eat_token(); // eat '('
            parser.eat_token(); // eat 'param'

            let id = parser.maybe_ident("identifier for param")?;
            if id.is_some() {
                // ID is not available for abbreviation
                let ty = parser.parse()?;
                parser.closing_paren("parameter")?;
                params.push(Param { start, id, ty });
                continue;
            }

            // Abbreviation:
            //   (param {valtype}*) == (param {valtype})*
            loop {
                match parser.peek("value type for param or closing ')'")? {
                    (Token::RParen, _) => {
                        parser.eat_token(); // eat ')'
                        break;
                    }
                    (_, start) => {
                        let ty = parser.parse()?;
                        params.push(Param { start, id, ty });
                    }
                }
            }
        }
        Ok(params)
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-valtype
impl<'s> Parse<'s> for ValType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let expected = "keyword for value type";
        match parser.next_token(expected)? {
            (Token::Keyword("i32"), _) => Ok(ValType::I32),
            (Token::Keyword("i64"), _) => Ok(ValType::I64),
            (Token::Keyword("f32"), _) => Ok(ValType::F32),
            (Token::Keyword("f64"), _) => Ok(ValType::F64),
            (Token::Keyword(id), offset) => parser.error(ParseErrorKind::InvalidValType(id), offset),
            (tok, offset) => parser.unexpected_token(tok, expected, offset),
        }
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-result
// Not impl for FuncResult considering abbreviation
impl<'s> Parse<'s> for Vec<FuncResult> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let mut results = vec![];
        while let Some("result") = parser.peek_fold_start("result type")?.0 {
            parser.eat_token(); // eat '('
            parser.eat_token(); // eat 'result'

            // Abbreviation:
            //   (result {valtype}*) == (result {valtype})*
            loop {
                match parser.peek("value type for result or closing ')'")? {
                    (Token::RParen, _) => {
                        parser.eat_token(); // eat ')'
                        break;
                    }
                    (_, start) => {
                        let ty = parser.parse()?;
                        results.push(FuncResult { start, ty });
                    }
                }
            }
        }
        Ok(results)
    }
}

// https://webassembly.github.io/spec/core/text/values.html#text-name
impl<'s> Parse<'s> for Name<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        // A name string must form a valid UTF-8 encoding as defined by Unicode (Section 2.5)
        let (content, offset) = match_token!(parser, "string literal for name", Token::String(s, _) => s);
        let encoded = match content {
            Cow::Borrowed(slice) => str::from_utf8(slice).ok().map(Cow::Borrowed),
            Cow::Owned(vec) => String::from_utf8(vec).ok().map(Cow::Owned),
        };
        if let Some(encoded) = encoded {
            Ok(Name(encoded))
        } else {
            parser.error(ParseErrorKind::MalformedUtf8Encoding, offset)
        }
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-import
// Parse "mod name" "name" sequence in import section
impl<'s> Parse<'s> for Import<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let mod_name = parser.parse()?;
        let name = parser.parse()?;
        Ok(Import { mod_name, name })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-import
#[cfg_attr(test, derive(Debug))]
enum ImportItem<'s> {
    Func(Func<'s>),
    Table(Table<'s>),
    Memory(Memory<'s>),
    Global(Global<'s>),
}
impl<'s> Parse<'s> for ImportItem<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("import")?;
        match_token!(parser, "'import' keyword", Token::Keyword("import"));
        let import = parser.parse()?;

        parser.opening_paren("import item")?;
        let (keyword, offset) =
            match_token!(parser, "one of 'func', 'table', 'memory', 'global'", Token::Keyword(kw) => kw);

        let id = parser.maybe_ident("identifier for import item")?;

        // https://webassembly.github.io/spec/core/text/modules.html#text-importdesc
        let item = match keyword {
            "func" => {
                parser.ctx.func_indices.new_idx(id, start)?;
                ImportItem::Func(Func {
                    start,
                    id,
                    ty: parser.parse()?,
                    kind: FuncKind::Import(import),
                })
            }
            "table" => {
                parser.ctx.table_indices.new_idx(id, start)?;
                ImportItem::Table(Table {
                    start,
                    id,
                    ty: parser.parse()?,
                    import: Some(import),
                })
            }
            "memory" => {
                parser.ctx.mem_indices.new_idx(id, start)?;
                ImportItem::Memory(Memory {
                    start,
                    id,
                    ty: parser.parse()?,
                    import: Some(import),
                })
            }
            "global" => {
                parser.ctx.global_indices.new_idx(id, start)?;
                ImportItem::Global(Global {
                    start,
                    id,
                    ty: parser.parse()?,
                    kind: GlobalKind::Import(import),
                })
            }
            kw => return parser.error(ParseErrorKind::UnexpectedKeyword(kw), offset),
        };

        parser.closing_paren("import item")?;
        parser.closing_paren("import")?;
        Ok(item)
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#type-uses
impl<'s> Parse<'s> for TypeUse<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let (keyword, start) = parser.peek_fold_start("type or param or result in typeuse")?;
        let idx = if let Some("type") = keyword {
            parser.eat_token(); // Eat '('
            parser.eat_token(); // Eat 'type'
            let idx = parser.parse()?;
            parser.closing_paren("type")?;
            Some(idx)
        } else {
            None
        };

        let params: Vec<Param<'s>> = parser.parse()?;
        let results: Vec<FuncResult> = parser.parse()?;

        Ok(TypeUse {
            start,
            idx: match idx {
                Some(idx) => TypeIndex::Explicit(idx),
                None => TypeIndex::Implicit(parser.create_inline_typeuse(start, &params, &results)),
            },
            params,
            results,
        })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#indices
impl<'s> Parse<'s> for Index<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let expected = "number or identifier for index";
        match parser.next_token(expected)? {
            (Token::Int(Sign::Minus, base, s), offset) => {
                parser.error(ParseErrorKind::NumberMustBePositive(base, s), offset)
            }
            (Token::Int(Sign::Plus, base, s), offset) => parse_u32_str(parser, s, base, offset).map(Index::Num),
            (Token::Ident(id), _) => Ok(Index::Ident(id)),
            (tok, offset) => parser.unexpected_token(tok.clone(), expected, offset),
        }
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-tabletype
impl<'s> Parse<'s> for TableType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let limit = parser.parse()?;
        match_token!(parser, "'funcref' keyword for table type", Token::Keyword("funcref"));
        Ok(TableType { limit })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-limits
impl<'s> Parse<'s> for Limits {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let min = parser.parse_u32("u32 for min table limit")?;
        Ok(match parser.peek("u32 for max table limit")? {
            (Token::Int(..), _) => {
                let max = parser.parse_u32("u32 for min table limit")?;
                Limits::Range { min, max }
            }
            _ => Limits::From { min },
        })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-memtype
impl<'s> Parse<'s> for MemType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        parser.parse().map(|limit| MemType { limit })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-globaltype
impl<'s> Parse<'s> for GlobalType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        match parser.peek("'(' for mut or value type of global type")? {
            (Token::LParen, _) => {
                parser.eat_token(); // eat '('
                match_token!(parser, "'mut' keyword for global type", Token::Keyword("mut"));
                let ty = parser.parse()?;
                parser.closing_paren("mutable global type")?;
                Ok(GlobalType { mutable: true, ty })
            }
            _ => Ok(GlobalType {
                mutable: false,
                ty: parser.parse()?,
            }),
        }
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-export
impl<'s> Parse<'s> for Export<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("export")?;
        match_token!(parser, "'export' keyword for export", Token::Keyword("export"));
        let name = parser.parse()?;

        parser.opening_paren("export item")?;
        let (keyword, offset) = match_token!(parser, "keyword for export item", Token::Keyword(kw) => kw);
        let kind = match keyword {
            "func" => ExportKind::Func,
            "table" => ExportKind::Table,
            "memory" => ExportKind::Memory,
            "global" => ExportKind::Global,
            _ => return parser.error(ParseErrorKind::UnexpectedKeyword(keyword), offset),
        };
        let idx = parser.parse()?;
        parser.closing_paren("export item")?;

        parser.closing_paren("export")?;

        Ok(Export { start, name, kind, idx })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-func
impl<'s> Parse<'s> for Func<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("func")?;
        match_token!(parser, "'func' keyword for func field", Token::Keyword("func"));

        // Note: func has some abbreviations
        // https://webassembly.github.io/spec/core/text/modules.html#text-func-abbrev

        let id = parser.maybe_ident("identifier for func field")?;
        let idx = parser.ctx.func_indices.new_idx(id, start)?;
        loop {
            match parser.peek_fold_start("'import', 'export' or typeuse in func field")?.0 {
                Some("import") => {
                    // `(func $id (import "m" "n") {typeuse})` is a syntax sugar of
                    // `(import "m" "n" (func $id {typeuse}))`
                    parser.opening_paren("import in func")?;
                    parser.eat_token(); // Eat 'import' keyword
                    let import = parser.parse()?;
                    parser.closing_paren("import in func")?;
                    let ty = parser.parse()?;
                    parser.closing_paren("func")?;
                    return Ok(Func {
                        start,
                        id,
                        ty,
                        kind: FuncKind::Import(import),
                    });
                }
                Some("export") => {
                    // `(func $id (export "n") {typeuse})` is a syntax sugar of
                    // `(export "n" (func $id)) (func $id {typeuse})`
                    let export_start = parser.opening_paren("export in func")?;
                    parser.eat_token(); // Eat 'export' keyword
                    let name = parser.parse()?;
                    parser.closing_paren("export in func")?;
                    parser.ctx.exports.push(Export {
                        start: export_start,
                        name,
                        kind: ExportKind::Func,
                        idx: Index::Num(idx),
                    });
                }
                _ => {
                    let ty = parser.parse()?;
                    let locals = parser.parse()?;
                    let body = parser.parse()?;
                    parser.closing_paren("func")?;
                    return Ok(Func {
                        start,
                        id,
                        ty,
                        kind: FuncKind::Body { locals, body },
                    });
                }
            }
        }
    }
}

// Implement Parse for Vec due to abbreviation
// https://webassembly.github.io/spec/core/text/modules.html#text-func-abbrev
//
// https://webassembly.github.io/spec/core/text/modules.html#text-local
impl<'s> Parse<'s> for Vec<Local<'s>> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let mut locals = vec![];
        while let (Some("local"), start) = parser.peek_fold_start("local")? {
            parser.eat_token(); // Eat '(' keyword
            parser.eat_token(); // Eat 'local' keyword

            if let Some(id) = parser.maybe_ident("identifier for local")? {
                let ty = parser.parse()?;
                locals.push(Local {
                    start,
                    id: Some(id),
                    ty,
                });
            } else {
                // Only when ID is omitted, multiple types can be combined into one local statement
                // `(local ty*)` means `(local ty)*`. Here parse `i64 f32` of `(local i32 i64 f32)`
                loop {
                    match parser.peek("')' or typeuse for local")? {
                        (Token::RParen, _) => break,
                        _ => locals.push(Local {
                            start,
                            id: None,
                            ty: parser.parse()?,
                        }),
                    }
                }
            }

            parser.closing_paren("local")?;
        }
        Ok(locals)
    }
}

// Instructions has special abbreviation. It can be folded and nested. This struct parses sequence
// of instructions with the abbreviation.
//
//   1: ({plaininstr} {foldedinstr}*) == {foldedinstr}* {plaininstr}
//   2: (block {label} {resulttype} {instr}*) == block {label} {resulttype} {instr}* end
//   3: (loop {label} {resulttype} {instr}*) == 'loop' {label} {resulttype} {instr}* end
//   4: (if {label} {resulttype} {foldedinstr}* (then {instr}*) (else {instr}*)?) ==
//          {foldedinstr}* if {label} {resulttype} {instr}* else {instr}* end
//
// e.g.
//   (local.get $x) (i32.const 2) i32.add (i32.const 3) i32.mul ==
//       (i32.mul (i32.add (local.get $x) (i32.const 2)) (i32.const 3))
//
// https://webassembly.github.io/spec/core/text/instructions.html#folded-instructions
struct MaybeFoldedInsn<'s, 'p> {
    insns: Vec<Instruction<'s>>,
    parser: &'p mut Parser<'s>,
}

// Note: These are free functions not to modify `insns` field of MaybeFoldedInsn.

fn parse_maybe_result_type<'s>(parser: &mut Parser<'s>) -> Result<'s, Option<ValType>> {
    // Note: This requires that next token exists
    if let Some("result") = parser.peek_fold_start("result type")?.0 {
        parser.eat_token(); // Eat '('
        parser.eat_token(); // Eat 'result'
        let ty = parser.parse()?;
        parser.closing_paren("result type")?;
        Ok(Some(ty))
    } else {
        Ok(None)
    }
}

// https://webassembly.github.io/spec/core/text/instructions.html#folded-instructions
impl<'s, 'p> MaybeFoldedInsn<'s, 'p> {
    fn new(parser: &'p mut Parser<'s>) -> MaybeFoldedInsn<'s, 'p> {
        MaybeFoldedInsn { insns: vec![], parser }
    }

    fn parse_naked_insn(&mut self, end: bool) -> Result<'s, Instruction<'s>> {
        let (kw, start) = match_token!(self.parser, "keyword for instruction", Token::Keyword(k) => k);
        let kind = match kw {
            // Control instructions
            // https://webassembly.github.io/spec/core/text/instructions.html#control-instructions
            "block" | "loop" => {
                let label = self.parser.maybe_ident("label for block or loop")?;
                let ty = parse_maybe_result_type(self.parser)?;
                let body = self.parser.parse()?;
                let id = if end {
                    match_token!(self.parser, "'end' keyword for block or loop", Token::Keyword("end"));
                    self.parser.maybe_ident("ID for block or loop")?
                } else {
                    None
                };
                if kw == "block" {
                    InsnKind::Block { label, ty, body, id }
                } else {
                    InsnKind::Loop { label, ty, body, id }
                }
            }
            "if" => {
                // Note: 'else' can be omitted when else clause is empty
                // https://webassembly.github.io/spec/core/text/instructions.html#abbreviations
                let is_folded = !end;
                let label = self.parser.maybe_ident("label for block or loop")?;
                let ty = parse_maybe_result_type(self.parser)?;

                // Folded 'if' instruction abbreviation:
                //   (if {label} {resulttype} {foldedinstr}* (then {instr}*) (else {instr}*))
                //      == {foldedinstr}* if {label} {resulttype} {instr}* else {instr}* end

                // foldedinstr* for condition
                if is_folded {
                    loop {
                        let kw = self
                            .parser
                            .peek_fold_start("folded instructions for condition in folded 'if' instruction")?
                            .0;
                        match kw {
                            Some("then") => break,
                            _ => self.parse_folded()?,
                        }
                    }
                }

                // (then {instr}*)
                if is_folded {
                    self.parser.opening_paren("'then' clause in folded 'if'")?;
                    match_token!(self.parser, "'then' keyword for folded 'if'", Token::Keyword("then"));
                }
                let then_body = self.parser.parse()?;
                if is_folded {
                    self.parser.closing_paren("then clause in folded 'if'")?;
                }

                // (else {instr}*))
                let (else_body, else_id) = match self.parser.peek("'else', 'end', '(' or ')' in 'if'")? {
                    (Token::Keyword("end"), _) if end && !is_folded => (vec![], None),
                    (Token::RParen, _) if !end => (vec![], None),
                    (Token::LParen, _) if is_folded => {
                        self.parser.eat_token(); // Eat '('
                        match_token!(
                            self.parser,
                            "'else' keyword in else clause of folded 'if'",
                            Token::Keyword("else")
                        );
                        let body = self.parser.parse()?;
                        self.parser.closing_paren("else clause in folded 'if'")?;
                        (body, None)
                    }
                    (Token::Keyword("else"), _) if !is_folded => {
                        self.parser.eat_token(); // Eat 'else'
                        let id = self.parser.maybe_ident("ID for 'else' in 'if'")?;
                        let body = self.parser.parse()?;
                        (body, id)
                    }
                    (tok, offset) => {
                        return self
                            .parser
                            .unexpected_token(tok.clone(), "'else' or 'end' in 'if'", offset);
                    }
                };
                let end_id = if end {
                    match_token!(self.parser, "'end' keyword for 'if'", Token::Keyword("end"));
                    self.parser.maybe_ident("ID for end of 'if'")?
                } else {
                    None
                };
                InsnKind::If {
                    label,
                    ty,
                    then_body,
                    else_id,
                    else_body,
                    end_id,
                }
            }
            "unreachable" => InsnKind::Unreachable,
            "nop" => InsnKind::Nop,
            "br" => InsnKind::Br(self.parser.parse()?),
            "br_if" => InsnKind::BrIf(self.parser.parse()?),
            "br_table" => {
                let mut labels = vec![];
                while let (Token::Int(..), _) | (Token::Ident(_), _) = self.parser.peek("labels for 'br_table'")? {
                    labels.push(self.parser.parse()?);
                }
                if let Some(default_label) = labels.pop() {
                    InsnKind::BrTable { labels, default_label }
                } else {
                    return self.parser.error(
                        ParseErrorKind::InvalidOperand {
                            insn: "br_table",
                            msg: "at least one label is necessary",
                        },
                        start,
                    );
                }
            }
            "return" => InsnKind::Return,
            "call" => InsnKind::Call(self.parser.parse()?),
            "call_indirect" => {
                let ty: TypeUse = self.parser.parse()?;
                // No identifier can be bound in any param declaration appearing in the type annotation
                if let Some(id) = ty.params.iter().find_map(|p| p.id) {
                    return self.parser.error(ParseErrorKind::IdBoundToParam(id), start);
                }
                InsnKind::CallIndirect(ty)
            }
            // Parametric instructions
            // https://webassembly.github.io/spec/core/text/instructions.html#parametric-instructions
            "drop" => InsnKind::Drop,
            "select" => InsnKind::Select,
            // Variable instructions
            // https://webassembly.github.io/spec/core/text/instructions.html#variable-instructions
            "local.get" => InsnKind::LocalGet(self.parser.parse()?),
            "local.set" => InsnKind::LocalSet(self.parser.parse()?),
            "local.tee" => InsnKind::LocalTee(self.parser.parse()?),
            "global.get" => InsnKind::GlobalGet(self.parser.parse()?),
            "global.set" => InsnKind::GlobalSet(self.parser.parse()?),
            // Memory instructions
            // https://webassembly.github.io/spec/core/text/instructions.html#memory-instructions
            "i32.load" => InsnKind::I32Load(self.parser.parse_mem_arg(2)?),
            "i64.load" => InsnKind::I64Load(self.parser.parse_mem_arg(3)?),
            "f32.load" => InsnKind::F32Load(self.parser.parse_mem_arg(2)?),
            "f64.load" => InsnKind::F64Load(self.parser.parse_mem_arg(3)?),
            "i32.load8_s" => InsnKind::I32Load8S(self.parser.parse_mem_arg(0)?),
            "i32.load8_u" => InsnKind::I32Load8U(self.parser.parse_mem_arg(0)?),
            "i32.load16_s" => InsnKind::I32Load16S(self.parser.parse_mem_arg(1)?),
            "i32.load16_u" => InsnKind::I32Load16U(self.parser.parse_mem_arg(1)?),
            "i64.load8_s" => InsnKind::I64Load8S(self.parser.parse_mem_arg(0)?),
            "i64.load8_u" => InsnKind::I64Load8U(self.parser.parse_mem_arg(0)?),
            "i64.load16_s" => InsnKind::I64Load16S(self.parser.parse_mem_arg(1)?),
            "i64.load16_u" => InsnKind::I64Load16U(self.parser.parse_mem_arg(1)?),
            "i64.load32_s" => InsnKind::I64Load32S(self.parser.parse_mem_arg(2)?),
            "i64.load32_u" => InsnKind::I64Load32U(self.parser.parse_mem_arg(2)?),
            "i32.store" => InsnKind::I32Store(self.parser.parse_mem_arg(2)?),
            "i64.store" => InsnKind::I64Store(self.parser.parse_mem_arg(3)?),
            "f32.store" => InsnKind::F32Store(self.parser.parse_mem_arg(2)?),
            "f64.store" => InsnKind::F64Store(self.parser.parse_mem_arg(3)?),
            "i32.store8" => InsnKind::I32Store8(self.parser.parse_mem_arg(0)?),
            "i32.store16" => InsnKind::I32Store16(self.parser.parse_mem_arg(1)?),
            "i64.store8" => InsnKind::I64Store8(self.parser.parse_mem_arg(0)?),
            "i64.store16" => InsnKind::I64Store16(self.parser.parse_mem_arg(1)?),
            "i64.store32" => InsnKind::I64Store32(self.parser.parse_mem_arg(2)?),
            "memory.size" => InsnKind::MemorySize,
            "memory.grow" => InsnKind::MemoryGrow,
            // Numeric instructions
            // https://webassembly.github.io/spec/core/text/instructions.html#numeric-instructions
            // Constants
            "i32.const" => {
                // Note: i32.const operand takes range of i32::MIN <= i <= u32::MAX
                // When the value is over i32::MAX, it is treated as u32 value and bitcasted to i32
                let ((sign, base, digits), offset) =
                    match_token!(self.parser, "integer for i32.const operand", Token::Int(s, b, d) => (s, b, d));
                let u = parse_u32_str(self.parser, digits, base, offset)?;
                if sign == Sign::Plus {
                    InsnKind::I32Const(u as i32)
                } else if u <= i32::MAX as u32 + 1 {
                    InsnKind::I32Const(u.wrapping_neg() as i32)
                } else {
                    return self.parser.cannot_parse_num("i32", "too small integer", offset);
                }
            }
            "i64.const" => {
                // Note: i64.const operand takes range of i64::MIN <= i <= u64::MAX
                // When the value is over i64::MAX, it is treated as u64 value and bitcasted to i64
                let ((sign, base, digits), offset) =
                    match_token!(self.parser, "integer for i64.const operand", Token::Int(s, b, d) => (s, b, d));
                let u = parse_u64_str(self.parser, digits, base, offset)?;
                if sign == Sign::Plus {
                    InsnKind::I64Const(u as i64)
                } else if u <= i64::MAX as u64 + 1 {
                    InsnKind::I64Const(u.wrapping_neg() as i64)
                } else {
                    return self.parser.cannot_parse_num("i64", "too small integer", offset);
                }
            }
            "f32.const" => {
                let val = match self.parser.next_token("float number or integer for f32.const")? {
                    (Token::Float(sign, float), offset) => match float {
                        Float::Inf => match sign {
                            Sign::Plus => f32::INFINITY,
                            Sign::Minus => f32::NEG_INFINITY,
                        },
                        Float::Nan(None) => sign.apply(f32::NAN),
                        Float::Nan(Some(payload)) => {
                            // Encode  f32 NaN value via u32 assuming IEEE-754 format for NaN boxing.
                            // Palyload must be
                            //   - within 23bits (fraction of f32 is 23bits)
                            //   - >= 2^(23-1) meant that most significant bit must be 1 (since frac cannot be zero for NaN value)
                            // https://webassembly.github.io/spec/core/syntax/values.html#floating-point
                            let payload_u = parse_u32_str(self.parser, payload, NumBase::Hex, offset)?;
                            if payload_u == 0 || 0x80_0000 <= payload_u {
                                return self.parser.cannot_parse_num(
                                    "f32",
                                    "payload of NaN must be in range of 1 <= payload < 2^23",
                                    offset,
                                );
                            }
                            // NaN boxing. 1 <= payload_u < 2^23 and floating point number is in IEEE754 format.
                            // This will encode the payload into fraction of NaN.
                            //   0x{sign}11111111{payload}
                            let sign = match sign {
                                Sign::Plus => 0,
                                Sign::Minus => 1u32 << 31, // most significant bit is 1 for negative number
                            };
                            let exp = 0b1111_1111u32 << (31 - 8);
                            f32::from_bits(sign | exp | payload_u)
                        }
                        Float::Val {
                            base: NumBase::Dec,
                            frac,
                            exp,
                        } => self.parser.parse_dec_float(sign, frac, exp, offset)?,
                        Float::Val {
                            base: NumBase::Hex,
                            frac,
                            exp,
                        } => {
                            // Note: Better algorithm should be considered
                            // https://github.com/rust-lang/rust/blob/3982d3514efbb65b3efac6bb006b3fa496d16663/src/libcore/num/dec2flt/algorithm.rs
                            parse_hex_float_f32(self.parser, sign, frac, exp, offset)?
                        }
                    },
                    (Token::Int(sign, NumBase::Dec, digits), offset) => {
                        self.parser.parse_dec_float(sign, digits, None, offset)?
                    }
                    (Token::Int(sign, NumBase::Hex, digits), offset) => {
                        parse_hex_float_f32(self.parser, sign, digits, None, offset)?
                    }
                    (tok, offset) => {
                        return self
                            .parser
                            .unexpected_token(tok, "float number or integer for f32.const", offset)
                    }
                };
                InsnKind::F32Const(val)
            }
            "f64.const" => {
                let val = match self.parser.next_token("float number or integer for f64.const")? {
                    (Token::Float(sign, float), offset) => match float {
                        Float::Inf => match sign {
                            Sign::Plus => f64::INFINITY,
                            Sign::Minus => f64::NEG_INFINITY,
                        },
                        Float::Nan(None) => sign.apply(f64::NAN),
                        Float::Nan(Some(payload)) => {
                            // Encode f64 NaN value via u64 assuming IEEE-754 format for NaN boxing.
                            // Palyload must be
                            //   - within 52bits (since fraction of f64 is 52bits)
                            //   - >= 2^(52-1) meant that most significant bit must be 1 (since frac cannot be zero for NaN value)
                            // https://webassembly.github.io/spec/core/syntax/values.html#floating-point
                            let payload_u = parse_u64_str(self.parser, payload, NumBase::Hex, offset)?;
                            if payload_u == 0 || 0x10_0000_0000_0000 <= payload_u {
                                return self.parser.cannot_parse_num(
                                    "f64",
                                    "payload of NaN must be in range of 1 <= payload < 2^52",
                                    offset,
                                );
                            }
                            // NaN boxing. 1 <= payload_u < 2^52 and floating point number is in IEEE754 format.
                            // This will encode the payload into fraction of NaN.
                            //   0x{sign}11111111111{payload}
                            let sign = match sign {
                                Sign::Plus => 0,
                                Sign::Minus => 1u64 << 63, // most significant bit is 1 for negative number
                            };
                            let exp = 0b111_1111_1111u64 << (63 - 11);
                            f64::from_bits(sign | exp | payload_u)
                        }
                        Float::Val {
                            base: NumBase::Dec,
                            frac,
                            exp,
                        } => self.parser.parse_dec_float(sign, frac, exp, offset)?,
                        Float::Val {
                            base: NumBase::Hex,
                            frac,
                            exp,
                        } => parse_hex_float_f64(self.parser, sign, frac, exp, offset)?,
                    },
                    (Token::Int(sign, NumBase::Dec, digits), offset) => {
                        self.parser.parse_dec_float(sign, digits, None, offset)?
                    }
                    (Token::Int(sign, NumBase::Hex, digits), offset) => {
                        parse_hex_float_f64(self.parser, sign, digits, None, offset)?
                    }
                    (tok, offset) => {
                        return self
                            .parser
                            .unexpected_token(tok, "float number or integer for f64.const", offset)
                    }
                };
                InsnKind::F64Const(val)
            }
            "i32.clz" => InsnKind::I32Clz,
            "i32.ctz" => InsnKind::I32Ctz,
            "i32.popcnt" => InsnKind::I32Popcnt,
            "i32.add" => InsnKind::I32Add,
            "i32.sub" => InsnKind::I32Sub,
            "i32.mul" => InsnKind::I32Mul,
            "i32.div_s" => InsnKind::I32DivS,
            "i32.div_u" => InsnKind::I32DivU,
            "i32.rem_s" => InsnKind::I32RemS,
            "i32.rem_u" => InsnKind::I32RemU,
            "i32.and" => InsnKind::I32And,
            "i32.or" => InsnKind::I32Or,
            "i32.xor" => InsnKind::I32Xor,
            "i32.shl" => InsnKind::I32Shl,
            "i32.shr_s" => InsnKind::I32ShrS,
            "i32.shr_u" => InsnKind::I32ShrU,
            "i32.rotl" => InsnKind::I32Rotl,
            "i32.rotr" => InsnKind::I32Rotr,
            "i64.clz" => InsnKind::I64Clz,
            "i64.ctz" => InsnKind::I64Ctz,
            "i64.popcnt" => InsnKind::I64Popcnt,
            "i64.add" => InsnKind::I64Add,
            "i64.sub" => InsnKind::I64Sub,
            "i64.mul" => InsnKind::I64Mul,
            "i64.div_s" => InsnKind::I64DivS,
            "i64.div_u" => InsnKind::I64DivU,
            "i64.rem_s" => InsnKind::I64RemS,
            "i64.rem_u" => InsnKind::I64RemU,
            "i64.and" => InsnKind::I64And,
            "i64.or" => InsnKind::I64Or,
            "i64.xor" => InsnKind::I64Xor,
            "i64.shl" => InsnKind::I64Shl,
            "i64.shr_s" => InsnKind::I64ShrS,
            "i64.shr_u" => InsnKind::I64ShrU,
            "i64.rotl" => InsnKind::I64Rotl,
            "i64.rotr" => InsnKind::I64Rotr,
            "f32.abs" => InsnKind::F32Abs,
            "f32.neg" => InsnKind::F32Neg,
            "f32.ceil" => InsnKind::F32Ceil,
            "f32.floor" => InsnKind::F32Floor,
            "f32.trunc" => InsnKind::F32Trunc,
            "f32.nearest" => InsnKind::F32Nearest,
            "f32.sqrt" => InsnKind::F32Sqrt,
            "f32.add" => InsnKind::F32Add,
            "f32.sub" => InsnKind::F32Sub,
            "f32.mul" => InsnKind::F32Mul,
            "f32.div" => InsnKind::F32Div,
            "f32.min" => InsnKind::F32Min,
            "f32.max" => InsnKind::F32Max,
            "f32.copysign" => InsnKind::F32Copysign,
            "f64.abs" => InsnKind::F64Abs,
            "f64.neg" => InsnKind::F64Neg,
            "f64.ceil" => InsnKind::F64Ceil,
            "f64.floor" => InsnKind::F64Floor,
            "f64.trunc" => InsnKind::F64Trunc,
            "f64.nearest" => InsnKind::F64Nearest,
            "f64.sqrt" => InsnKind::F64Sqrt,
            "f64.add" => InsnKind::F64Add,
            "f64.sub" => InsnKind::F64Sub,
            "f64.mul" => InsnKind::F64Mul,
            "f64.div" => InsnKind::F64Div,
            "f64.min" => InsnKind::F64Min,
            "f64.max" => InsnKind::F64Max,
            "f64.copysign" => InsnKind::F64Copysign,
            "i32.eqz" => InsnKind::I32Eqz,
            "i32.eq" => InsnKind::I32Eq,
            "i32.ne" => InsnKind::I32Ne,
            "i32.lt_s" => InsnKind::I32LtS,
            "i32.lt_u" => InsnKind::I32LtU,
            "i32.gt_s" => InsnKind::I32GtS,
            "i32.gt_u" => InsnKind::I32GtU,
            "i32.le_s" => InsnKind::I32LeS,
            "i32.le_u" => InsnKind::I32LeU,
            "i32.ge_s" => InsnKind::I32GeS,
            "i32.ge_u" => InsnKind::I32GeU,
            "i64.eqz" => InsnKind::I64Eqz,
            "i64.eq" => InsnKind::I64Eq,
            "i64.ne" => InsnKind::I64Ne,
            "i64.lt_s" => InsnKind::I64LtS,
            "i64.lt_u" => InsnKind::I64LtU,
            "i64.gt_s" => InsnKind::I64GtS,
            "i64.gt_u" => InsnKind::I64GtU,
            "i64.le_s" => InsnKind::I64LeS,
            "i64.le_u" => InsnKind::I64LeU,
            "i64.ge_s" => InsnKind::I64GeS,
            "i64.ge_u" => InsnKind::I64GeU,
            "f32.eq" => InsnKind::F32Eq,
            "f32.ne" => InsnKind::F32Ne,
            "f32.lt" => InsnKind::F32Lt,
            "f32.gt" => InsnKind::F32Gt,
            "f32.le" => InsnKind::F32Le,
            "f32.ge" => InsnKind::F32Ge,
            "f64.eq" => InsnKind::F64Eq,
            "f64.ne" => InsnKind::F64Ne,
            "f64.lt" => InsnKind::F64Lt,
            "f64.gt" => InsnKind::F64Gt,
            "f64.le" => InsnKind::F64Le,
            "f64.ge" => InsnKind::F64Ge,
            "i32.wrap_i64" => InsnKind::I32WrapI64,
            "i32.trunc_f32_s" => InsnKind::I32TruncF32S,
            "i32.trunc_f32_u" => InsnKind::I32TruncF32U,
            "i32.trunc_f64_s" => InsnKind::I32TruncF64S,
            "i32.trunc_f64_u" => InsnKind::I32TruncF64U,
            "i64.extend_i32_s" => InsnKind::I64ExtendI32S,
            "i64.extend_i32_u" => InsnKind::I64ExtendI32U,
            "i64.trunc_f32_s" => InsnKind::I64TruncF32S,
            "i64.trunc_f32_u" => InsnKind::I64TruncF32U,
            "i64.trunc_f64_s" => InsnKind::I64TruncF64S,
            "i64.trunc_f64_u" => InsnKind::I64TruncF64U,
            "f32.convert_i32_s" => InsnKind::F32ConvertI32S,
            "f32.convert_i32_u" => InsnKind::F32ConvertI32U,
            "f32.convert_i64_s" => InsnKind::F32ConvertI64S,
            "f32.convert_i64_u" => InsnKind::F32ConvertI64U,
            "f32.demote_f64" => InsnKind::F32DemoteF64,
            "f64.convert_i32_s" => InsnKind::F64ConvertI32S,
            "f64.convert_i32_u" => InsnKind::F64ConvertI32U,
            "f64.convert_i64_s" => InsnKind::F64ConvertI64S,
            "f64.convert_i64_u" => InsnKind::F64ConvertI64U,
            "f64.promote_f32" => InsnKind::F64PromoteF32,
            "i32.reinterpret_f32" => InsnKind::I32ReinterpretF32,
            "i64.reinterpret_f64" => InsnKind::I64ReinterpretF64,
            "f32.reinterpret_i32" => InsnKind::F32ReinterpretI32,
            "f64.reinterpret_i64" => InsnKind::F64ReinterpretI64,
            "i32.extend8_s" => InsnKind::I32Extend8S,
            "i32.extend16_s" => InsnKind::I32Extend16S,
            "i64.extend8_s" => InsnKind::I64Extend8S,
            "i64.extend16_s" => InsnKind::I64Extend16S,
            "i64.extend32_s" => InsnKind::I64Extend32S,
            _ => return self.parser.error(ParseErrorKind::UnexpectedKeyword(kw), start),
        };
        Ok(Instruction { start, kind })
    }

    fn parse_folded(&mut self) -> Result<'s, ()> {
        let start = self.parser.opening_paren("folded instruction")?;
        let mut insn = self.parse_naked_insn(false)?;
        insn.start = start;

        if !insn.kind.is_block() {
            while let (Token::LParen, _) = self
                .parser
                .peek("')' for ending folded instruction or '(' for nested instruction")?
            {
                self.parse_folded()?;
            }
        }

        self.parser.closing_paren("folded instruction")?;
        self.insns.push(insn);
        Ok(())
    }

    fn parse_one(&mut self) -> Result<'s, ()> {
        // Parse {instr}. Even if one {instr}, result is sequence of instructions because of
        // folded form
        if let Token::LParen = self.parser.peek("instruction keyword or '('")?.0 {
            self.parse_folded()?;
        } else {
            let insn = self.parse_naked_insn(true)?;
            self.insns.push(insn);
        }
        Ok(())
    }

    fn parse(&mut self) -> Result<'s, ()> {
        // Parse {instr}*
        loop {
            // This assumes that instr* is always ending with
            //   - ')' and 'end' for end of instruction
            //   - 'else' for 'then' clause of 'if' instruction
            //   - other than keyword except for above
            match self.parser.peek("instruction keyword or '(' for folded instruction")?.0 {
                Token::LParen => self.parse_folded()?,
                Token::RParen | Token::Keyword("end") | Token::Keyword("else") => return Ok(()),
                Token::Keyword(_) => {
                    let insn = self.parse_naked_insn(true)?;
                    self.insns.push(insn)
                }
                _ => return Ok(()),
            };
        }
    }
}

impl<'s> Parse<'s> for Vec<Instruction<'s>> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let mut parser = MaybeFoldedInsn::new(parser);
        parser.parse()?;
        Ok(parser.insns)
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#element-segments
impl<'s> Parse<'s> for Elem<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        // Need to parse several abbreviations
        // https://webassembly.github.io/spec/core/text/modules.html#id7
        let start = parser.opening_paren("elem")?;
        match_token!(parser, "'elem' keyword", Token::Keyword("elem"));

        // tableidx can be omitted
        let idx = match parser.peek("table index of elem segment")?.0 {
            Token::Int(..) | Token::Ident(_) => parser.parse()?,
            _ => Index::Num(0),
        };

        let offset = if let Some("offset") = parser.peek_fold_start("offset in elem segment")?.0 {
            parser.eat_token(); // Eat '('
            parser.eat_token(); // Eat 'offset'
            let expr = parser.parse()?;
            parser.closing_paren("offset parameter of elem segment")?;
            expr
        } else {
            // Abbreviation: {instr} == (offset {instr})
            let mut parser = MaybeFoldedInsn::new(parser);
            parser.parse_one()?;
            parser.insns
        };

        // This token is not defined in webassembly.github.io/spec but wasm2wat emits it. It seems that this is not
        // included in Wasm MVP, but it is necessary to parse .wat files emitted by wasm2wat.
        // https://github.com/WebAssembly/wabt/blob/142c52678acf7decf6a48190c395ed73bb91170a/src/wat-writer.cc#L1271
        if let (Token::Keyword("func"), _) = parser.peek("")? {
            parser.eat_token(); // eat 'func'
        }

        let mut init = vec![];
        loop {
            if let (Token::RParen, _) = parser.peek("')' for elem segment")? {
                break;
            }
            init.push(parser.parse()?);
        }

        parser.closing_paren("elem")?;
        Ok(Elem {
            start,
            idx,
            offset,
            init,
        })
    }
}

// Helper struct to resolve import/export/elem abbreviation in 'table' section
// https://webassembly.github.io/spec/core/text/modules.html#text-table-abbrev
#[cfg_attr(test, derive(Debug))]
enum TableAbbrev<'s> {
    Elem(Table<'s>, Elem<'s>),
    Table(Table<'s>),
}

impl<'s> Parse<'s> for TableAbbrev<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("table")?;
        match_token!(parser, "'table' keyword", Token::Keyword("table"));

        let id = parser.maybe_ident("identifier for table section")?;
        let idx = parser.ctx.table_indices.new_idx(id, start)?;

        loop {
            match parser.peek("argument of table section")?.0 {
                Token::LParen => {
                    parser.eat_token(); // eat '('
                    let (keyword, offset) =
                        match_token!(parser, "'import' or 'export' for table section", Token::Keyword(k) => k);
                    match keyword {
                        "import" => {
                            // (table {id}? (import {name} {name} ) {tabletype}) ==
                            //    (import {name} {name} (table {id}? {tabletype}))
                            let import = parser.parse()?;
                            parser.closing_paren("import argument of table section")?;
                            let ty = parser.parse()?;
                            parser.closing_paren("table")?;
                            return Ok(TableAbbrev::Table(Table {
                                start,
                                id,
                                ty,
                                import: Some(import),
                            }));
                        }
                        "export" => {
                            // (table {id}? (export {name}) ...) ==
                            //   (export {name} (table {id}')) (table {id}' ...)
                            //   note that this occurs repeatedly
                            let name = parser.parse()?;
                            parser.closing_paren("export argument in table section")?;
                            parser.ctx.exports.push(Export {
                                start,
                                name,
                                kind: ExportKind::Table,
                                idx: Index::Num(idx),
                            });
                            // 'export' can be chained by'import', 'export', 'elem' and tabletype
                        }
                        kw => return parser.error(ParseErrorKind::UnexpectedKeyword(kw), offset),
                    }
                }
                Token::Keyword("funcref") => {
                    // (table {id}? funcref (elem  {funcidx}*)) ==
                    //   (table {id}' n n funcref) (elem {id}' (i32.const 0) {funcidx}*)
                    //   where n is length of {funcidx}*
                    parser.eat_token(); // eat 'funcref' (elemtype)
                    let elem_start = parser.opening_paren("elem argument in table section")?;
                    match_token!(parser, "'elem' keyword for table section", Token::Keyword("elem"));

                    let mut init = vec![];
                    while let Token::Int(..) | Token::Ident(_) =
                        parser.peek("function indices in elem in table section")?.0
                    {
                        init.push(parser.parse()?);
                    }

                    parser.closing_paren("elem argument in table section")?;
                    parser.closing_paren("table")?;
                    let n = init.len() as u32; // TODO: Check length <= 2^32
                    let table = Table {
                        start,
                        id,
                        ty: TableType {
                            limit: Limits::Range { min: n, max: n },
                        },
                        import: None,
                    };
                    let elem = Elem {
                        start: elem_start,
                        idx: Index::Num(idx),
                        offset: vec![Instruction {
                            start: elem_start,
                            kind: InsnKind::I32Const(0),
                        }],
                        init,
                    };
                    return Ok(TableAbbrev::Elem(table, elem));
                }
                _ => {
                    // tabletype
                    let ty = parser.parse()?;
                    parser.closing_paren("table")?;
                    return Ok(TableAbbrev::Table(Table {
                        start,
                        id,
                        ty,
                        import: None,
                    }));
                }
            }
        }
    }
}

impl<'s> Parse<'s> for Data<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("data")?;
        match_token!(parser, "'data' keyword", Token::Keyword("data"));

        // the memory index can be omitted, defaulting to 𝟶.
        let idx = match parser.peek("memory index for data")?.0 {
            Token::Int(..) | Token::Ident(_) => parser.parse()?,
            _ => Index::Num(0),
        };

        let offset = if let Some("offset") = parser.peek_fold_start("offset in data segment")?.0 {
            parser.eat_token(); // Eat '('
            parser.eat_token(); // Eat 'offset'
            let offset = parser.parse()?;
            parser.closing_paren("offset of data segment")?;
            offset
        } else {
            // Abbreviation: {instr} == (offset {instr})
            let mut parser = MaybeFoldedInsn::new(parser);
            parser.parse_one()?;
            parser.insns
        };

        let mut data = vec![];
        loop {
            match parser.next_token("')' or string literal for data segment")? {
                (Token::RParen, _) => {
                    return Ok(Data {
                        start,
                        idx,
                        offset,
                        data: Cow::Owned(data),
                    });
                }
                (Token::String(content, _), _) => data.extend_from_slice(content.as_ref()),
                (tok, offset) => {
                    return parser.unexpected_token(tok.clone(), "')' or string literal for data segment", offset)
                }
            }
        }
    }
}

// Helper struct to resolve import/export/data abbreviation in memory section
// https://webassembly.github.io/spec/core/text/modules.html#memories
#[cfg_attr(test, derive(Debug))]
enum MemoryAbbrev<'s> {
    Memory(Memory<'s>),
    Data(Memory<'s>, Data<'s>),
}
impl<'s> Parse<'s> for MemoryAbbrev<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("memory")?;
        match_token!(parser, "'memory' keyword", Token::Keyword("memory"));

        let id = parser.maybe_ident("identifier for memory section")?;
        let idx = parser.ctx.mem_indices.new_idx(id, start)?;

        loop {
            match parser.peek("argument of memory section")?.0 {
                Token::LParen => {
                    parser.eat_token(); // eat '('
                    let (keyword, offset) = match_token!(parser, "'import' or 'export' or 'data' for memory section", Token::Keyword(k) => k);
                    match keyword {
                        "import" => {
                            // (memory {id}? (import {name} {name} ) {memtype}) ==
                            //    (import {name} {name} (memory {id}? {memtype}))
                            let import = parser.parse()?;
                            parser.closing_paren("import argument of memory section")?;
                            let ty = parser.parse()?;
                            parser.closing_paren("memory")?;
                            return Ok(MemoryAbbrev::Memory(Memory {
                                start,
                                id,
                                ty,
                                import: Some(import),
                            }));
                        }
                        "export" => {
                            // (memory {id}? (export {name}) ...) ==
                            //   (export {name} (memory {id}')) (memory {id}' ...)
                            //   note that this occurs repeatedly
                            let name = parser.parse()?;
                            parser.closing_paren("export argument in memory section")?;
                            parser.ctx.exports.push(Export {
                                start,
                                name,
                                kind: ExportKind::Memory,
                                idx: Index::Num(idx),
                            });
                            // 'export' can be chained by'import', 'export', 'data' and tabletype
                        }
                        "data" => {
                            // (memory {id}? (data  {datastring})) ==
                            //   (memory {id}' m m) (data {id}' (i32.const 0) {datastring})
                            //   where n = data bytes, m = ceil(n / 64Ki), Note: 64Ki means page size

                            let mut data = vec![];
                            loop {
                                match parser.next_token("')' or string literal for data of memory section")? {
                                    (Token::RParen, _) => break,
                                    (Token::String(content, _), _) => data.extend_from_slice(content.as_ref()),
                                    (tok, offset) => {
                                        return parser.unexpected_token(
                                            tok.clone(),
                                            "')' or string literal for data of memory section",
                                            offset,
                                        )
                                    }
                                }
                            }
                            parser.closing_paren("memory")?;

                            // Infer memory limits from page size (64 * 1024 = 65536)
                            let n = (data.len() as f64 / 65536.0).ceil() as u32;

                            return Ok(MemoryAbbrev::Data(
                                Memory {
                                    start,
                                    id,
                                    ty: MemType {
                                        limit: Limits::Range { min: n, max: n },
                                    },
                                    import: None,
                                },
                                Data {
                                    start,
                                    idx: Index::Num(idx),
                                    offset: vec![Instruction {
                                        start,
                                        kind: InsnKind::I32Const(0),
                                    }],
                                    data: Cow::Owned(data),
                                },
                            ));
                        }
                        kw => return parser.error(ParseErrorKind::UnexpectedKeyword(kw), offset),
                    }
                }
                _ => {
                    // memtype
                    let ty = parser.parse()?;
                    parser.closing_paren("memory")?;
                    return Ok(MemoryAbbrev::Memory(Memory {
                        start,
                        id,
                        ty,
                        import: None,
                    }));
                }
            }
        }
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#globals
impl<'s> Parse<'s> for Global<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("global")?;
        match_token!(parser, "'global' keyword", Token::Keyword("global"));

        let id = parser.maybe_ident("identifier for global section")?;
        let idx = parser.ctx.global_indices.new_idx(id, start)?;

        // Note: Global section has import/export abbreviation
        // https://webassembly.github.io/spec/core/text/modules.html#globals
        loop {
            match parser.peek("argument of global section")?.0 {
                Token::LParen => {
                    parser.eat_token(); // Eat '('
                    let (keyword, offset) =
                        match_token!(parser, "'import' or 'export' for global section", Token::Keyword(k) => k);
                    match keyword {
                        "import" => {
                            // (global {id}? (import {name} {name} ) {globaltype}) ==
                            //    (import {name} {name} (global {id}? {globaltype}))
                            let import = parser.parse()?;
                            parser.closing_paren("import argument of global section")?;
                            let ty = parser.parse()?;
                            parser.closing_paren("global")?;
                            return Ok(Global {
                                start,
                                id,
                                ty,
                                kind: GlobalKind::Import(import),
                            });
                        }
                        "export" => {
                            // (global {id}? (export {name}) ...) ==
                            //   (export {name} (global {id}')) (global {id}' ...)
                            //   note that this occurs repeatedly
                            let name = parser.parse()?;
                            parser.closing_paren("export argument in global section")?;
                            parser.ctx.exports.push(Export {
                                start,
                                name,
                                kind: ExportKind::Global,
                                idx: Index::Num(idx),
                            });
                        }
                        "mut" => {
                            // globaltype with mut
                            let ty = parser.parse()?;
                            parser.closing_paren("global mutable type")?;
                            let init = parser.parse()?;
                            parser.closing_paren("global")?;
                            let ty = GlobalType { mutable: true, ty };
                            return Ok(Global {
                                start,
                                id,
                                ty,
                                kind: GlobalKind::Init(init),
                            });
                        }
                        kw => return parser.error(ParseErrorKind::UnexpectedKeyword(kw), offset),
                    }
                }
                _ => {
                    // globaltype without mut
                    let ty = parser.parse()?;
                    let init = parser.parse()?;
                    parser.closing_paren("global")?;
                    let ty = GlobalType { mutable: false, ty };
                    return Ok(Global {
                        start,
                        id,
                        ty,
                        kind: GlobalKind::Init(init),
                    });
                }
            }
        }
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-start
impl<'s> Parse<'s> for Start<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.opening_paren("start")?;
        match_token!(parser, "'start' keyword", Token::Keyword("start"));
        let idx = parser.parse()?;
        parser.closing_paren("start")?;
        Ok(Start { start, idx })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookahead() {
        let v = vec![1, 2, 3, 4];
        let mut i = LookAhead::new(v.into_iter());
        assert_eq!(i.peek(), Some(&1));
        assert_eq!(i.lookahead(), Some(&2));
        assert_eq!(i.next(), Some(1));

        assert_eq!(i.peek(), Some(&2));
        assert_eq!(i.lookahead(), Some(&3));
        assert_eq!(i.next(), Some(2));

        assert_eq!(i.peek(), Some(&3));
        assert_eq!(i.lookahead(), Some(&4));
        assert_eq!(i.next(), Some(3));

        assert_eq!(i.peek(), Some(&4));
        assert_eq!(i.lookahead(), None);
        assert_eq!(i.next(), Some(4));

        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);
        assert_eq!(i.next(), None);

        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);

        let v: Vec<i32> = vec![];
        let mut i = LookAhead::new(v.into_iter());
        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);
        assert_eq!(i.next(), None);
        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);
    }

    #[test]
    fn indices() {
        let mut i = Indices::new("source", "what", "scope");
        i.new_idx(None, 0).unwrap();
        i.new_idx(Some("hi"), 0).unwrap();
        i.new_idx(None, 0).unwrap();
        i.new_idx(Some("bye"), 0).unwrap();
        let m = i.move_out();
        assert_eq!(m.get("hi"), Some(&1));
        assert_eq!(m.get("bye"), Some(&3));
        assert_eq!(m.get("hey"), None);

        // Index is reset after move_out
        i.new_idx(Some("hi"), 0).unwrap();
        let m = i.move_out();
        assert_eq!(m.get("hi"), Some(&0));

        let mut i = Indices::new("source", "what", "scope");
        i.new_idx(Some("hi"), 0).unwrap();
        i.new_idx(Some("hi"), 0).unwrap_err();
    }

    macro_rules! assert_parse {
        ($input:expr, $node:ty, $expect:pat if $cond:expr) => {{
            let input = $input;
            let mut parser = Parser::new(input);
            let node: $node = match parser.parse() {
                Ok(n) => n,
                Err(e) => panic!("parse failed!: {}\n{:?}", e, e),
            };
            match node {
                $expect if $cond => { /* OK */ }
                _ => panic!(
                    "assertion failed: {:?} did not match to {}",
                    node,
                    stringify!($expect if $cond)
                ),
            }
            assert!(parser.is_done(), "token is remaining: {:?}", parser.tokens.collect::<Vec<_>>());
            parser
        }};
        ($input:expr, $node:ty, $expect:pat) => {
            assert_parse!($input, $node, $expect if true)
        };
    }

    macro_rules! assert_error {
        ($input:expr, $node:ty, $expect:pat if $cond:expr) => {{
            use ParseErrorKind::*;
            let input = $input;
            match Parser::new(input).parse::<$node>().unwrap_err().kind {
                $expect if $cond => { /* OK */ }
                err => panic!("assertion failed: {:?} did not match to pattern {}", err, stringify!($expect if $cond)),
            }
        }};
        ($input:expr, $node:ty, $expect:pat) => {
            assert_error!($input, $node, $expect if true);
        };
    }

    #[test]
    fn root() {
        assert_parse!(
            r#"
            (module $m1
                (type $t1 (func))
                (type $t2 (func))
                (import "m" "n" (func $f2 (type 0)))
                (import "m" "n" (table $tb2 0 funcref))
                (import "m" "n" (memory $m2 3))
                (import "m" "n" (global $g2 i32))
                (func $f1)
                (table $tb1 0 funcref)
                (memory $m1 3)
                (global $g1 i32 i32.const 0)
                (start $f1)
            )
            "#,
            Parsed<'_>,
            Parsed {
                module: Module {
                    id: Some("$m1"),
                    types,
                    exports,
                    elems,
                    tables,
                    data,
                    memories,
                    globals,
                    funcs,
                    entrypoint,
                    ..
                },
                type_indices,
                func_indices,
                table_indices,
                mem_indices,
                global_indices,
            }
            if types.len() == 2
               && elems.is_empty()
               && tables.len() == 2
               && data.is_empty()
               && memories.len() == 2
               && globals.len() == 2
               && funcs.len() == 2
               && exports.is_empty()
               && entrypoint.is_some()
               && type_indices.contains_key("$t1")
               && type_indices.contains_key("$t2")
               && type_indices.len() == 2
               && func_indices.contains_key("$f1")
               && func_indices.contains_key("$f2")
               && func_indices.len() == 2
               && table_indices.contains_key("$tb1")
               && table_indices.contains_key("$tb2")
               && table_indices.len() == 2
               && mem_indices.contains_key("$m1")
               && mem_indices.contains_key("$m2")
               && mem_indices.len() == 2
               && global_indices.contains_key("$g1")
               && global_indices.contains_key("$g2")
               && global_indices.len() == 2
        );
    }

    #[test]
    fn module() {
        assert_parse!(r#"(module)"#, Module<'_>, Module { id: None, types, .. } if types.is_empty());
        assert_parse!(r#"(module $foo)"#, Module<'_>, Module { id: Some("$foo"), types, .. } if types.is_empty());
        assert_parse!(r#"(module $foo (type $f1 (func)))"#, Module<'_>, Module { id: Some("$foo"), types, .. } if types.len() == 1);
        assert_parse!(
            r#"
            (module
              (type $f1 (func))
              (type $f2 (func)))
            "#,
            Module<'_>,
            Module { id: None, types, .. } if types.len() == 2
        );
        assert_parse!(
            r#"(module (start $f))"#,
            Module<'_>,
            Module {
                id: None,
                entrypoint: Some(Start {
                    idx: Index::Ident("$f"),
                    ..
                }),
                ..
            }
        );
        assert_parse!(r#"(module (func))"#, Module<'_>, Module { funcs, .. } if funcs.len() == 1);
        assert_parse!(
            r#"(module (start $f))"#,
            Module<'_>,
            Module {
                id: None,
                entrypoint: Some(Start {
                    idx: Index::Ident("$f"),
                    ..
                }),
                ..
            }
        );
        // abbreviation
        assert_parse!(
            r#"(type $f1 (func)) (type $f2 (func))"#,
            Module<'_>,
            Module { id: None, types, .. } if types.len() == 2
        );

        assert_error!(r#"(module"#, Module<'_>, UnexpectedEndOfFile { .. });
        assert_error!(
            r#"(module $foo (type $f (func))"#,
            Module<'_>,
            UnexpectedEndOfFile { .. }
        );
        assert_error!(
            r#"(module $foo (start $f) (start 3))"#,
            Module<'_>,
            MultipleEntrypoints(
                Start {
                    idx: Index::Ident("$f"),
                    ..
                },
                Start { idx: Index::Num(3), .. },
            )
        );
        // abbreviation
        assert_error!(
            r#"(type $f1 (func)) )"#,
            Module<'_>,
            ExpectEndOfFile {
                token: Token::RParen,
                ..
            }
        );
        // imports must be put before other definitions
        assert_error!(
            r#"
            (module
              (type $t1 (func))
              (func $f1)
              (import "m" "n" (func $f2 (type 0)))
            )
            "#,
            Module<'_>,
            ImportMustPrecedeOtherDefs { .. }
        );
        assert_error!(
            r#"
            (module
              (table $tb1 0 funcref)
              (import "m" "n" (table $tb2 0 funcref))
            )
            "#,
            Module<'_>,
            ImportMustPrecedeOtherDefs { .. }
        );
        assert_error!(
            r#"
            (module
              (memory $m1 3)
              (import "m" "n" (memory $m2 3))
            )
            "#,
            Module<'_>,
            ImportMustPrecedeOtherDefs { .. }
        );
        assert_error!(
            r#"
            (module
              (global $g1 i32 i32.const 0)
              (import "m" "n" (global $g2 i32))
            )
            "#,
            Module<'_>,
            ImportMustPrecedeOtherDefs { .. }
        );
    }

    #[test]
    fn module_field() {
        assert_parse!(r#"(type $f1 (func))"#, ModuleField<'_>, ModuleField::Type(..));
        assert_parse!(r#"(import "m" "n" (func))"#, ModuleField<'_>, ModuleField::Import(..));
        assert_parse!(r#"(export "n" (func $foo))"#, ModuleField<'_>, ModuleField::Export(..));
        assert_parse!(
            r#"(elem (offset i32.const 10) $func)"#,
            ModuleField<'_>,
            ModuleField::Elem(..)
        );
        assert_parse!(r#"(table 0 funcref)"#, ModuleField<'_>, ModuleField::Table(..));
        assert_parse!(
            r#"(data 0 i32.const 0 "hello")"#,
            ModuleField<'_>,
            ModuleField::Data(..)
        );
        assert_parse!(r#"(memory 3)"#, ModuleField<'_>, ModuleField::Memory(..));
        assert_parse!(
            r#"(global (mut i32) i32.const 0)"#,
            ModuleField<'_>,
            ModuleField::Global(..)
        );
        assert_parse!(r#"(start 3)"#, ModuleField<'_>, ModuleField::Start(..));
        assert_parse!(r#"(func)"#, ModuleField<'_>, ModuleField::Func(..));

        assert_error!(r#"(hello!)"#, ModuleField<'_>, UnexpectedKeyword("hello!"));
    }

    #[test]
    fn type_def() {
        assert_parse!(r#"(type $f1 (func))"#, TypeDef<'_>, TypeDef { id: Some("$f1"), .. });
        assert_parse!(r#"(type (func))"#, TypeDef<'_>, TypeDef { id: None, .. });

        assert_error!(r#"(type (func) (func))"#, TypeDef<'_>, MissingParen { paren: ')', .. });
        assert_error!(r#"(type)"#, TypeDef<'_>, MissingParen { paren: '(', .. });
        assert_error!(r#"(type"#, TypeDef<'_>, UnexpectedEndOfFile { .. });
    }

    #[test]
    fn func_type() {
        assert_parse!(r#"(func)"#, FuncType<'_>, FuncType{ params, results, .. } if params.is_empty() && results.is_empty());
        assert_parse!(r#"(func (param $a i32) (result i32))"#, FuncType<'_>, FuncType{ params, results, .. } if params.len() == 1 && results.len() == 1);
        assert_parse!(r#"(func (param $a i32))"#, FuncType<'_>, FuncType{ params, results, .. } if params.len() == 1 && results.is_empty());
        assert_parse!(r#"(func (result i32))"#, FuncType<'_>, FuncType{ params, results, .. } if params.is_empty() && results.len() == 1);
        assert_parse!(r#"
            (func
                (param $a i32)
                (param $a i32)
                (result i32)
                (result i32))
        "#, FuncType<'_>, FuncType{ params, results, .. } if params.len() == 2 && results.len() == 2);
        // Abbreviation

        assert_error!(r#"func"#, FuncType<'_>, MissingParen { paren: '(', .. });
        assert_error!(
            r#"(type"#,
            FuncType<'_>,
            UnexpectedToken {
                expected: "'func' keyword",
                ..
            }
        );
        assert_error!(
            r#"(func "#,
            FuncType<'_>,
            UnexpectedEndOfFile {
                expected: "parameter",
                ..
            }
        );
        assert_error!(
            r#"(func (result i32) foo"#,
            FuncType<'_>,
            MissingParen { paren: ')', .. }
        );
    }

    #[test]
    fn params() {
        macro_rules! assert_params {
            ($input:expr, $expect:pat) => {
                let input = concat!($input, ')'); // ) is necessary to avoid unexpected EOF
                let mut parser = Parser::new(input);
                let params: Vec<Param<'_>> = parser.parse().unwrap();
                match params.as_slice() {
                    $expect => { /* OK */ }
                    params => panic!(
                        "assertion failed: {:?} did not match to {}",
                        params,
                        stringify!($expect)
                    ),
                }
                assert!(matches!(parser.tokens.next(), Some(Ok((Token::RParen, _)))));
            };
        }

        assert_params!(
            "(param $a i32)",
            [Param {
                id: Some("$a"),
                ty: ValType::I32,
                ..
            }]
        );
        assert_params!(
            "(param i32)",
            [Param {
                id: None,
                ty: ValType::I32,
                ..
            }]
        );
        assert_params!(
            "(param $a i32) (param i64) (param f32) (param $b f64)",
            [
                Param {
                    id: Some("$a"),
                    ty: ValType::I32,
                    ..
                },
                Param {
                    id: None,
                    ty: ValType::I64,
                    ..
                },
                Param {
                    id: None,
                    ty: ValType::F32,
                    ..
                },
                Param {
                    id: Some("$b"),
                    ty: ValType::F64,
                    ..
                }
            ]
        );

        assert_params!("", []);
        assert_params!("(param)", []);
        assert_params!(
            "(param i32 i64 f32 f64)",
            [
                Param {
                    id: None,
                    ty: ValType::I32,
                    ..
                },
                Param {
                    id: None,
                    ty: ValType::I64,
                    ..
                },
                Param {
                    id: None,
                    ty: ValType::F32,
                    ..
                },
                Param {
                    id: None,
                    ty: ValType::F64,
                    ..
                }
            ]
        );
        assert_params!(
            "(param i32 i64) (param $a i32) (param) (param f32 f64) (param $b i64)",
            [
                Param {
                    id: None,
                    ty: ValType::I32,
                    ..
                },
                Param {
                    id: None,
                    ty: ValType::I64,
                    ..
                },
                Param {
                    id: Some("$a"),
                    ty: ValType::I32,
                    ..
                },
                Param {
                    id: None,
                    ty: ValType::F32,
                    ..
                },
                Param {
                    id: None,
                    ty: ValType::F64,
                    ..
                },
                Param {
                    id: Some("$b"),
                    ty: ValType::I64,
                    ..
                }
            ]
        );
    }

    #[test]
    fn value_type() {
        assert_parse!(r#"i32"#, ValType, ValType::I32);
        assert_parse!(r#"i64"#, ValType, ValType::I64);
        assert_parse!(r#"f32"#, ValType, ValType::F32);
        assert_parse!(r#"f64"#, ValType, ValType::F64);

        assert_error!(r#"string"#, ValType, InvalidValType("string"));
        assert_error!(
            r#"$hello"#,
            ValType,
            UnexpectedToken {
                expected: "keyword for value type",
                ..
            }
        );
    }

    #[test]
    fn func_results() {
        macro_rules! assert_results {
            ($input:expr, $expect:pat) => {
                let input = concat!($input, ')'); // ) is necessary to avoid unexpected EOF
                let mut parser = Parser::new(input);
                let results: Vec<FuncResult> = parser.parse().unwrap();
                match results.as_slice() {
                    $expect => { /* OK */ }
                    results => panic!(
                        "assertion failed: {:?} did not match to {}",
                        results,
                        stringify!($expect)
                    ),
                }
                assert!(matches!(parser.tokens.next(), Some(Ok((Token::RParen, _)))));
            };
        }

        assert_results!("", []);
        assert_results!("(result)", []);
        assert_results!("(result i32)", [FuncResult { ty: ValType::I32, .. }]);
        assert_results!(
            "(result i32) (result i64) (result f32) (result f64) ",
            [
                FuncResult { ty: ValType::I32, .. },
                FuncResult { ty: ValType::I64, .. },
                FuncResult { ty: ValType::F32, .. },
                FuncResult { ty: ValType::F64, .. }
            ]
        );
        assert_results!(
            "(result i32 i64 f32 f64) ",
            [
                FuncResult { ty: ValType::I32, .. },
                FuncResult { ty: ValType::I64, .. },
                FuncResult { ty: ValType::F32, .. },
                FuncResult { ty: ValType::F64, .. }
            ]
        );
        assert_results!(
            "(result i32 i64) (result f32) (result) (result f64 i32) (result i64)",
            [
                FuncResult { ty: ValType::I32, .. },
                FuncResult { ty: ValType::I64, .. },
                FuncResult { ty: ValType::F32, .. },
                FuncResult { ty: ValType::F64, .. },
                FuncResult { ty: ValType::I32, .. },
                FuncResult { ty: ValType::I64, .. }
            ]
        );
    }

    #[test]
    fn name() {
        assert_parse!(r#""n""#, Name, Name(n) if n == "n");
        assert_parse!(r#""name""#, Name, Name(n) if n == "name");
        assert_parse!(r#""a\tb\nc""#, Name, Name(n) if n == "a\tb\nc");
        assert_parse!(r#""""#, Name, Name(n) if n.is_empty());
        assert_parse!(r#""\t\n\r\"\'\\\u{3042}\41""#, Name, Name(n) if n == "\t\n\r\"'\\あA");

        assert_error!(r#""\80""#, Name, MalformedUtf8Encoding);
    }

    #[test]
    fn import() {
        assert_parse!(
            r#"(import "mod" "name" (func (type 0)))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                kind: FuncKind::Import(Import {
                    mod_name: Name(mn),
                    name: Name(n),
                }),
                ..
            }) if mn == "mod" && n == "name"
        );
        assert_parse!(
            r#"(import "env" "print" (func $print (param i32)))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                kind: FuncKind::Import(Import { .. }),
                ..
            })
        );

        assert_error!(r#"import"#, ImportItem<'_>, MissingParen { paren: '(', .. });
        assert_error!(
            r#"(hello"#,
            ImportItem<'_>,
            UnexpectedToken {
                expected: "'import' keyword",
                ..
            }
        );
        assert_error!(
            r#"(import "mod" "name" (func)"#,
            ImportItem<'_>,
            MissingParen { paren: ')', .. }
        );
        assert_error!(r#"(import "mod" (func)"#, ImportItem<'_>, UnexpectedToken { .. });
        assert_error!(r#"(import (func)"#, ImportItem<'_>, UnexpectedToken { .. });
    }

    #[test]
    fn import_item() {
        assert_parse!(
            r#"(import "m" "n" (func))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                id: None,
                kind: FuncKind::Import(_),
                ..
            })
        );
        assert_parse!(
            r#"(import "m" "n" (func $foo))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                id: Some("$foo"),
                kind: FuncKind::Import(_),
                ..
            })
        );
        assert_parse!(
            r#"(import "m" "n" (table 0 funcref))"#,
            ImportItem<'_>,
            ImportItem::Table(Table {
                id: None,
                import: Some(_),
                ..
            })
        );
        assert_parse!(
            r#"(import "m" "n" (table $foo 0 funcref))"#,
            ImportItem<'_>,
            ImportItem::Table(Table {
                id: Some("$foo"),
                import: Some(_),
                ..
            })
        );
        assert_parse!(
            r#"(import "m" "n" (memory 0))"#,
            ImportItem<'_>,
            ImportItem::Memory(Memory {
                id: None,
                import: Some(_),
                ..
            })
        );
        assert_parse!(
            r#"(import "m" "n" (memory $foo 0))"#,
            ImportItem<'_>,
            ImportItem::Memory(Memory {
                id: Some("$foo"),
                import: Some(_),
                ..
            })
        );
        assert_parse!(
            r#"(import "m" "n" (global i32))"#,
            ImportItem<'_>,
            ImportItem::Global(Global {
                id: None,
                kind: GlobalKind::Import(_),
                ..
            })
        );
        assert_parse!(
            r#"(import "m" "n" (global $foo i32))"#,
            ImportItem<'_>,
            ImportItem::Global(Global {
                id: Some("$foo"),
                kind: GlobalKind::Import(_),
                ..
            })
        );
        let parser = assert_parse!(
            r#"(module
              (import "m" "f" (func $x))
              (import "m" "t" (table $x 0 funcref))
              (import "m" "m" (memory $x 1))
              (import "m" "g" (global $x i32))
            )"#,
            Module<'_>,
            Module{ funcs, tables, memories, globals, .. }
            if funcs.len() == 1 && tables.len() == 1 && memories.len() == 1 && globals.len() == 1
        );
        assert_eq!(parser.ctx.func_indices.indices.len(), 1);
        assert_eq!(parser.ctx.table_indices.indices.len(), 1);
        assert_eq!(parser.ctx.mem_indices.indices.len(), 1);
        assert_eq!(parser.ctx.global_indices.indices.len(), 1);

        assert_error!(r#"func"#, ImportItem<'_>, MissingParen { paren: '(', .. });
        assert_error!(r#"(import "m" "n" (func"#, ImportItem<'_>, UnexpectedEndOfFile { .. });
    }

    #[test]
    fn type_use() {
        // XXX: Parsing TypeUse<'s> directly does not work since parser tries to parse (param)* and
        // (result)* and fails with unexpected EOF when they are missing. This is not a real-world
        // problem because typeuse is always used within other statement.
        assert_parse!(
            r#"(import "m" "n" (func (type 0)))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                ty: TypeUse { params, results, idx: TypeIndex::Explicit(Index::Num(0)), .. },
                ..
            }) if params.is_empty() && results.is_empty()
        );
        assert_parse!(
            r#"(import "m" "n" (func (type $f)))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                ty: TypeUse { params, results, idx: TypeIndex::Explicit(Index::Ident("$f")), .. },
                ..
            }) if params.is_empty() && results.is_empty()
        );
        assert_parse!(
            r#"(import "m" "n" (func (type 0) (param i32)))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                ty: TypeUse { params, results, idx: TypeIndex::Explicit(Index::Num(0)), .. },
                ..
            }) if params.len() == 1 && results.is_empty()
        );
        assert_parse!(
            r#"(import "m" "n" (func (type 0) (result i32)))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                ty: TypeUse { params, results, idx: TypeIndex::Explicit(Index::Num(0)), .. },
                ..
            }) if params.is_empty() && results.len() == 1
        );
        assert_parse!(
            r#"(import "m" "n" (func (type 0) (param i32) (result i32)))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                ty: TypeUse { params, results, idx: TypeIndex::Explicit(Index::Num(0)), .. },
                ..
            }) if params.len() == 1 && results.len() == 1
        );
        // Abbreviation
        assert_parse!(
            r#"(import "m" "n" (func (param i32) (result i32)))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                ty: TypeUse { params, results, idx: TypeIndex::Implicit(0), .. },
                ..
            }) if params.len() == 1 && results.len() == 1
        );
        assert_parse!(
            r#"(import "m" "n" (func (result i32)))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                ty: TypeUse { params, results, idx: TypeIndex::Implicit(0), .. },
                ..
            }) if params.is_empty() && results.len() == 1
        );
        assert_parse!(
            r#"(import "m" "n" (func))"#,
            ImportItem<'_>,
            ImportItem::Func(Func {
                ty: TypeUse { params, results, idx: TypeIndex::Implicit(0), .. },
                ..
            }) if params.is_empty() && results.is_empty()
        );

        // typeuse has special abbreviation and it affects entire module. It means that assert_parse!
        // is not available
        {
            let input = r#"
                (module
                 (type $f (func (param i32) (result i32)))
                 (type $g (func (param i32) (result i32)))
                 (func (param i32) (result i32))
                )
            "#;
            let mut parser = Parser::new(input);
            let m: Module<'_> = parser.parse().unwrap();
            // First function type which has the same signature is chosen
            assert_eq!(m.funcs[0].ty.idx, TypeIndex::Implicit(0));
        }
        {
            let input = r#"
                (module
                 (type $f (func (param i32) (result i32)))
                 (type $g (func (param i32) (result i64)))
                 (func (param i32) (result i64))
                )
            "#;
            let mut parser = Parser::new(input);
            let m: Module<'_> = parser.parse().unwrap();
            // First function type which has the same signature is chosen
            assert_eq!(m.funcs[0].ty.idx, TypeIndex::Implicit(0));
        }
        {
            let input = r#"
                (module
                 (func (param f32) (result i32))
                )
            "#;
            let mut parser = Parser::new(input);
            let m: Module<'_> = parser.parse().unwrap();
            // If there is no function type matching to the function's signature, new function type
            // is inserted to the module
            assert_eq!(m.types.len(), 1);
            match &m.types[0] {
                TypeDef {
                    id: None,
                    ty: FuncType { params, results, .. },
                    ..
                } => {
                    assert_eq!(params.len(), 1);
                    assert_eq!(params[0].id, None);
                    assert_eq!(params[0].ty, ValType::F32);
                    assert_eq!(results.len(), 1);
                    assert_eq!(results[0].ty, ValType::I32);
                }
                t => panic!("Type section entry is not set correctly: {:?}", t),
            }
        }

        // Note: {typeuse} accepts empty string due to abbreviation. Empty string is accepted as
        // TypeUse { idx: None, params: [], results: [], .. } with (type (func)) inserted to module.
        // https://webassembly.github.io/spec/core/text/modules.html#abbreviations
    }

    #[test]
    fn index() {
        assert_parse!(r#"0"#, Index<'_>, Index::Num(0));
        assert_parse!(r#"0x1f"#, Index<'_>, Index::Num(0x1f));
        assert_parse!(r#"$foo"#, Index<'_>, Index::Ident("$foo"));

        assert_error!(
            r#"hi"#,
            Index<'_>,
            UnexpectedToken {
                expected: "number or identifier for index",
                ..
            }
        );
        assert_error!(
            r#""#,
            Index<'_>,
            UnexpectedEndOfFile {
                expected: "number or identifier for index",
                ..
            }
        );
    }

    #[test]
    fn table_type_and_limits() {
        assert_parse!(
            r#"0 funcref"#,
            TableType,
            TableType {
                limit: Limits::From { min: 0 }
            }
        );
        assert_parse!(
            r#"0 1 funcref"#,
            TableType,
            TableType {
                limit: Limits::Range { min: 0, max: 1 }
            }
        );

        assert_error!(
            r#"0 1 hi"#,
            TableType,
            UnexpectedToken {
                expected: "'funcref' keyword for table type",
                ..
            }
        );
        assert_error!(
            r#"hi"#,
            TableType,
            UnexpectedToken {
                expected: "u32 for min table limit",
                ..
            }
        );
    }

    #[test]
    fn memtype() {
        assert_parse!(
            r#"0 10"#,
            MemType,
            MemType {
                limit: Limits::Range { min: 0, max: 10 }
            }
        );
    }

    #[test]
    fn global_type() {
        assert_parse!(
            r#"i32"#,
            GlobalType,
            GlobalType {
                mutable: false,
                ty: ValType::I32
            }
        );
        assert_parse!(
            r#"(mut i32)"#,
            GlobalType,
            GlobalType {
                mutable: true,
                ty: ValType::I32
            }
        );

        assert_error!(
            r#""#,
            GlobalType,
            UnexpectedEndOfFile {
                expected: "'(' for mut or value type of global type",
                ..
            }
        );
        assert_error!(
            r#"(hello"#,
            GlobalType,
            UnexpectedToken {
                expected: "'mut' keyword for global type",
                ..
            }
        );
        assert_error!(r#"(mut i32"#, GlobalType, MissingParen { paren: ')', .. });
    }

    #[test]
    fn export() {
        assert_parse!(
            r#"(export "hi" (func 0))"#,
            Export<'_>,
            Export{ name: Name(n), kind: ExportKind::Func, idx: Index::Num(0), .. } if n == "hi"
        );
        assert_parse!(
            r#"(export "hi" (table $foo))"#,
            Export<'_>,
            Export{ name: Name(n), kind: ExportKind::Table, idx: Index::Ident("$foo"), .. } if n == "hi"
        );
        assert_parse!(
            r#"(export "hi" (memory 0))"#,
            Export<'_>,
            Export{ name: Name(n), kind: ExportKind::Memory, idx: Index::Num(0), .. } if n == "hi"
        );
        assert_parse!(
            r#"(export "hi" (global 0))"#,
            Export<'_>,
            Export{ name: Name(n), kind: ExportKind::Global, idx: Index::Num(0), .. } if n == "hi"
        );

        assert_error!(r#"export"#, Export<'_>, MissingParen { paren: '(', .. });
        assert_error!(r#"(export "hi" (func 0"#, Export<'_>, MissingParen { paren: ')', .. });
        assert_error!(r#"(export "hi" (func 0)"#, Export<'_>, MissingParen { paren: ')', .. });
        assert_error!(
            r#"(hello"#,
            Export<'_>,
            UnexpectedToken {
                expected: "'export' keyword for export",
                ..
            }
        );
        assert_error!(r#"(export "hi" (hello 0))"#, Export<'_>, UnexpectedKeyword("hello"));
    }

    #[test]
    fn func_field() {
        assert_parse!(
            r#"(func $f (import "m" "n") (type 0))"#,
            Func<'_>,
            Func {
                kind: FuncKind::Import(Import {
                    mod_name: Name(m),
                    name: Name(n),
                }),
                ..
            } if m == "m" && n == "n"
        );
        let parser = assert_parse!(
            r#"(func $f (export "n") (type 0))"#,
            Func<'_>,
            Func {
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    body,
                },
                ..
            }
            if locals.is_empty() && body.is_empty()
        );
        assert_eq!(parser.ctx.exports.len(), 1);
        match &parser.ctx.exports[0] {
            Export {
                name: Name(n),
                kind: ExportKind::Func,
                idx: Index::Num(0),
                ..
            } if n == "n" => { /* OK */ }
            e => panic!("did not match: {:?}", e),
        }
        assert_parse!(
            r#"(func (type 0))"#,
            Func<'_>,
            Func {
                id: None,
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    body,
                },
                ..
            } if locals.is_empty() && body.is_empty()
        );
        assert_parse!(
            r#"(func $f (type 0))"#,
            Func<'_>,
            Func {
                id: Some("$f"),
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                ..
            }
        );
        assert_parse!(
            r#"(func $f)"#,
            Func<'_>,
            Func {
                id: Some("$f"),
                ty: TypeUse {
                    idx: TypeIndex::Implicit(0),
                    ..
                },
                ..
            }
        );
        assert_parse!(
            r#"(func $f (type 0) (local))"#,
            Func<'_>,
            Func {
                id: Some("$f"),
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    ..
                },
                ..
            } if locals.is_empty()
        );
        assert_parse!(
            r#"(func $f (type 0) (local i32))"#,
            Func<'_>,
            Func {
                id: Some("$f"),
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    ..
                },
                ..
            } if locals[0].ty == ValType::I32 && locals[0].id.is_none()
        );
        assert_parse!(
            r#"(func $f (type 0) (local $l i32))"#,
            Func<'_>,
            Func {
                id: Some("$f"),
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    ..
                },
                ..
            } if locals[0].ty == ValType::I32 && locals[0].id == Some("$l")
        );
        assert_parse!(
            r#"(func $f (type 0) (local i32 f64))"#,
            Func<'_>,
            Func {
                id: Some("$f"),
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    ..
                },
                ..
            } if locals.iter().map(|l| l.ty).collect::<Vec<_>>() == vec![ValType::I32, ValType::F64] &&
                locals.iter().map(|l| l.id).collect::<Vec<_>>() == vec![None, None]
        );
        assert_parse!(
            r#"(func $f (type 0) (local i32) (local f64))"#,
            Func<'_>,
            Func {
                id: Some("$f"),
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    ..
                },
                ..
            } if locals.iter().map(|l| l.ty).collect::<Vec<_>>() == vec![ValType::I32, ValType::F64] &&
                locals.iter().map(|l| l.id).collect::<Vec<_>>() == vec![None, None]
        );
        assert_parse!(
            r#"(func $f (type 0) (local $l1 i32) (local $l2 f64))"#,
            Func<'_>,
            Func {
                id: Some("$f"),
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    ..
                },
                ..
            } if locals.iter().map(|l| l.ty).collect::<Vec<_>>() == vec![ValType::I32, ValType::F64] &&
                locals.iter().map(|l| l.id).collect::<Vec<_>>() == vec![Some("$l1"), Some("$l2")]
        );
        assert_parse!(
            r#"(func $_start (result i32))"#,
            Func<'_>,
            Func {
                id: Some("$_start"),
                ty: TypeUse {
                    idx: TypeIndex::Implicit(0),
                    params,
                    results,
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    ..
                },
                ..
            } if params.is_empty() && results[0].ty == ValType::I32 && locals.is_empty()
        );
        // Multiple exports
        let parser = assert_parse!(
            r#"(func (export "a") (export "b") (export "c"))"#,
            Func<'_>,
            Func { .. }
        );
        assert!(matches!(parser.ctx.exports.as_slice(), [
            Export {
                name: Name(n1),
                kind: ExportKind::Func,
                idx: Index::Num(0),
                ..
            },
            Export {
                name: Name(n2),
                kind: ExportKind::Func,
                idx: Index::Num(0),
                ..
            },
            Export {
                name: Name(n3),
                kind: ExportKind::Func,
                idx: Index::Num(0),
                ..
            },
        ] if n1 == "a" && n2 == "b" && n3 == "c"));
        let parser = assert_parse!(
            r#"(func $f (export "a") (export "b") (type 0) (local i32 f64) (i32.const 10))"#,
            Func<'_>,
            Func {
                id: Some("$f"),
                ty: TypeUse {
                    idx: TypeIndex::Explicit(Index::Num(0)),
                    ..
                },
                kind: FuncKind::Body {
                    locals,
                    body,
                },
                ..
            }
            if locals.len() == 2 && body.len() == 1
        );
        assert!(matches!(parser.ctx.exports.as_slice(), [
            Export {
                name: Name(n1),
                kind: ExportKind::Func,
                idx: Index::Num(0),
                ..
            },
            Export {
                name: Name(n2),
                kind: ExportKind::Func,
                idx: Index::Num(0),
                ..
            },
        ] if n1 == "a" && n2 == "b"));
    }

    macro_rules! assert_insn {
        ($p:ident => $init:expr, $input:expr, $expect:pat if $cond:expr) => {
            let input = concat!($input, ')');
            let mut $p = Parser::new(input);
            { $init; }
            let insns: Vec<Instruction> = $p.parse().unwrap();
            let kinds = insns.into_iter().map(|i| i.kind).collect::<Vec<_>>();
            match kinds.as_slice() {
                $expect if $cond => { /* OK */ }
                i => panic!(
                    "assertion failed: {:?} did not match to {}",
                    i,
                    stringify!($expect if $cond)
                ),
            }
            match $p.tokens.next() {
                Some(Ok((Token::RParen, _))) => (),
                tok => assert!(false, "Tokens still remain: {:?} and then {:?}", tok, $p.tokens.collect::<Vec<_>>()),
            }
        };
        ($p:ident => $init:expr, $input:expr, $expect:pat) => {
            assert_insn!($p => $init, $input, $expect if true);
        };
        ($input:expr, $expect:pat if $cond:expr) => {
            assert_insn!(p => (), $input, $expect if $cond);
        };
        ($input:expr, $expect:pat) => {
            assert_insn!($input, $expect if true);
        };
    }

    #[test]
    fn insn_abbrev() {
        use InsnKind::*;
        assert_insn!(r#"nop"#, [Nop]);
        assert_insn!(r#"(nop)"#, [Nop]);
        assert_insn!(r#"(unreachable (nop))"#, [Nop, Unreachable]);
        assert_insn!(r#"(return (nop) (unreachable))"#, [Nop, Unreachable, Return]);
        assert_insn!(r#"(return (unreachable (nop)))"#, [Nop, Unreachable, Return]);
        assert_insn!(r#"(nop (return (nop) (unreachable)))"#, [Nop, Unreachable, Return, Nop]);
        assert_insn!(r#"nop unreachable return"#, [Nop, Unreachable, Return]);
        assert_insn!(r#"nop (nop) (nop)"#, [Nop, Nop, Nop]);
        assert_insn!(r#"(nop) nop (nop)"#, [Nop, Nop, Nop]);
        assert_insn!(r#"(nop) (nop) nop"#, [Nop, Nop, Nop]);
        assert_insn!(r#"nop (nop (nop))"#, [Nop, Nop, Nop]);
        assert_insn!(r#"(nop (nop)) nop"#, [Nop, Nop, Nop]);
        assert_insn!(r#"block block block end end end"#, [Block { body, .. }]
            if match &body[0].kind {
                Block { body, ..} => match &body[0].kind {
                    Block { body, .. } => body.is_empty(),
                    n => panic!("nest 2: {:?}", n),
                }
                n => panic!("nest 1: {:?}", n),
            }
        );
        assert_insn!(r#"(block (block (block )))"#, [Block { body, .. }]
            if match &body[0].kind {
                Block { body, ..} => match &body[0].kind {
                    Block { body, .. } => body.is_empty(),
                    n => panic!("nest 2: {:?}", n),
                }
                n => panic!("nest 1: {:?}", n),
            }
        );
        assert_insn!(r#"block (block block end) end"#, [Block { body, .. }]
            if match &body[0].kind {
                Block { body, ..} => match &body[0].kind {
                    Block { body, .. } => body.is_empty(),
                    n => panic!("nest 2: {:?}", n),
                }
                n => panic!("nest 1: {:?}", n),
            }
        );

        assert_error!(
            r#"(nop nop)"#,
            Vec<Instruction>,
            MissingParen {
                paren: ')',
                what: "folded instruction",
                ..
            }
        );
        assert_error!(r#"not-exist"#, Vec<Instruction>, UnexpectedKeyword("not-exist"));
        assert_error!(r#"(not-exist)"#, Vec<Instruction>, UnexpectedKeyword("not-exist"));
    }

    #[test]
    fn control_instructions() {
        use InsnKind::*;
        assert_insn!(
            r#"block end"#,
            [
                Block{ label: None, ty: None, body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"block $blk end $id"#,
            [
                Block{ label: Some("$blk"), ty: None, body, id: Some("$id") }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"block $blk (result i32) end"#,
            [
                Block{ label: Some("$blk"), ty: Some(ValType::I32), body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"block (result i32) end"#,
            [
                Block{ label: None, ty: Some(ValType::I32), body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"block nop end"#,
            [
                Block{ label: None, ty: None, body, id: None }
            ] if matches!(body[0].kind, Nop)
        );
        assert_insn!(
            r#"block $blk nop end"#,
            [
                Block{ label: Some("$blk"), ty: None, body, id: None }
            ] if matches!(body[0].kind, Nop)
        );
        assert_insn!(
            r#"block (result i32) nop end"#,
            [
                Block{ label: None, ty: Some(ValType::I32), body, id: None }
            ] if matches!(body[0].kind, Nop)
        );
        assert_insn!(
            r#"(block)"#,
            [
                Block{ label: None, ty: None, body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"(block $blk)"#,
            [
                Block{ label: Some("$blk"), ty: None, body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"(block (result i32))"#,
            [
                Block{ label: None, ty: Some(ValType::I32), body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"(block nop)"#,
            [
                Block{ label: None, ty: None, body, id: None }
            ] if matches!(body[0].kind, Nop)
        );
        // Note: 'loop' instruction is parsed with the same logic as 'block' instruction. Only one test case is sufficient
        assert_insn!(
            r#"loop end"#,
            [
                Loop{ label: None, ty: None, body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"if end"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if else end"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l (result i32) else $a end $b"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: Some("$a"), else_body, end_id: Some("$b") }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l (result i32) else $a end $b"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: Some("$a"), else_body, end_id: Some("$b") }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l (result i32) end $b"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: Some("$b") }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l end"#,
            [
                If{ label: Some("$l"), ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if (result i32) end"#,
            [
                If{ label: None, ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if nop end"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if matches!(then_body[0].kind, Nop) && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l nop end"#,
            [
                If{ label: Some("$l"), ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if matches!(then_body[0].kind, Nop) && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l (result i32) nop end"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: None }
            ] if matches!(then_body[0].kind, Nop) && else_body.is_empty()
        );
        assert_insn!(
            r#"if nop else unreachable end"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if matches!(then_body[0].kind, Nop) && matches!(else_body[0].kind, Unreachable)
        );
        assert_insn!(
            r#"(if (then))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if $l (then))"#,
            [
                If{ label: Some("$l"), ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if $l (result i32) (then))"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if (then) (else))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if $l (then) (else))"#,
            [
                If{ label: Some("$l"), ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if $l (result i32) (then) (else))"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if (then nop))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if matches!(then_body[0].kind, Nop) && else_body.is_empty()
        );
        assert_insn!(
            r#"(if (then nop) (else nop))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if matches!(then_body[0].kind, Nop) && matches!(else_body[0].kind, Nop)
        );
        assert_insn!(
            r#"(if (then (nop)) (else (nop)))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if matches!(then_body[0].kind, Nop) && matches!(else_body[0].kind, Nop)
        );
        assert_insn!(
            r#"(if (then nop nop) (else nop nop))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.len() == 2 && else_body.len() == 2
        );
        assert_insn!(
            r#"(if (then (nop (nop))) (else (nop (nop))))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.len() == 2 && else_body.len() == 2
        );
        assert_insn!(
            r#"(if (nop) (then) (else))"#,
            [
                Nop,
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if (nop) (nop) (nop) (then))"#,
            [
                Nop, Nop, Nop,
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(r#"unreachable"#, [Unreachable]);
        assert_insn!(r#"br 0"#, [Br(Index::Num(0))]);
        assert_insn!(r#"br $blk"#, [Br(Index::Ident("$blk"))]);
        assert_insn!(r#"br_if 0"#, [BrIf(Index::Num(0))]);
        assert_insn!(r#"br_if $blk"#, [BrIf(Index::Ident("$blk"))]);
        assert_insn!(
            r#"br_table 0"#,
            [BrTable {
                labels,
                default_label: Index::Num(0)
            }] if labels.is_empty()
        );
        assert_insn!(
            r#"br_table 1 2 3 0"#,
            [
                BrTable {
                    labels,
                    default_label: Index::Num(0)
                }
            ] if labels
                    .iter()
                    .map(|i| match i {
                        Index::Num(n) => *n,
                        Index::Ident(i) => panic!("unexpected index: {}", i),
                    })
                    .collect::<Vec<_>>() == vec![1, 2, 3]
        );
        assert_insn!(
            r#"br_table $a $b $c $x"#,
            [
                BrTable {
                    labels,
                    default_label: Index::Ident("$x")
                }
            ] if labels
                    .iter()
                    .map(|i| match i {
                        Index::Num(n) => panic!("unexpected index: {}", n),
                        Index::Ident(i) => *i,
                    })
                    .collect::<Vec<_>>() == vec!["$a", "$b", "$c"]
        );
        assert_insn!(r#"call 0"#, [Call(Index::Num(0))]);
        assert_insn!(r#"call $f"#, [Call(Index::Ident("$f"))]);
        assert_insn!(
            r#"call_indirect (type 0)"#,
            [CallIndirect(TypeUse {
                idx: TypeIndex::Explicit(Index::Num(0)),
                ..
            })]
        );

        assert_error!(r#"br_table)"#, Vec<Instruction<'_>>, InvalidOperand { .. });
        assert_error!(
            r#"(if (then nop) else nop)"#,
            Vec<Instruction<'_>>,
            UnexpectedToken {
                got: Token::Keyword("else"),
                ..
            }
        );
        assert_error!(
            r#"call_indirect (type 0) (param $p i32))"#,
            Vec<Instruction<'_>>,
            IdBoundToParam("$p")
        );
    }

    #[test]
    fn parametric_instructions() {
        use InsnKind::*;
        assert_insn!(r#"drop"#, [Drop]);
        assert_insn!(r#"select"#, [Select]);
    }

    #[test]
    fn variable_instructions() {
        use InsnKind::*;
        assert_insn!(r#"local.get 0"#, [LocalGet(Index::Num(0))]);
        assert_insn!(r#"local.get $x"#, [LocalGet(Index::Ident("$x"))]);
        assert_insn!(r#"local.set 0"#, [LocalSet(Index::Num(0))]);
        assert_insn!(r#"local.tee 0"#, [LocalTee(Index::Num(0))]);
        assert_insn!(r#"global.get 0"#, [GlobalGet(Index::Num(0))]);
        assert_insn!(r#"global.set 0"#, [GlobalSet(Index::Num(0))]);

        assert_error!(r#"local.get foo"#, Vec<Instruction<'_>>, UnexpectedToken { .. });
    }

    #[test]
    fn memory_instructions() {
        use InsnKind::*;
        assert_insn!(r#"i32.load"#, [I32Load(Mem { align: 2, offset: 0 })]);
        assert_insn!(r#"i32.load align=32"#, [I32Load(Mem { align: 5, offset: 0 })]);
        assert_insn!(r#"i32.load offset=10"#, [I32Load(Mem { align: 2, offset: 10 })]);
        assert_insn!(
            r#"i32.load offset=10 align=32"#,
            [I32Load(Mem { align: 5, offset: 10 })]
        );
        assert_insn!(
            r#"i32.load offset=0x1f align=0x80"#,
            [I32Load(Mem { align: 7, offset: 0x1f })]
        );
        assert_insn!(r#"i64.load"#, [I64Load(..)]);
        assert_insn!(r#"f32.load"#, [F32Load(..)]);
        assert_insn!(r#"f64.load"#, [F64Load(..)]);
        assert_insn!(r#"i32.load8_s"#, [I32Load8S(..)]);
        assert_insn!(r#"i32.load8_u"#, [I32Load8U(..)]);
        assert_insn!(r#"i32.load16_s"#, [I32Load16S(..)]);
        assert_insn!(r#"i32.load16_u"#, [I32Load16U(..)]);
        assert_insn!(r#"i64.load8_s"#, [I64Load8S(..)]);
        assert_insn!(r#"i64.load8_u"#, [I64Load8U(..)]);
        assert_insn!(r#"i64.load16_s"#, [I64Load16S(..)]);
        assert_insn!(r#"i64.load16_u"#, [I64Load16U(..)]);
        assert_insn!(r#"i64.load32_s"#, [I64Load32S(..)]);
        assert_insn!(r#"i64.load32_u"#, [I64Load32U(..)]);
        assert_insn!(r#"i32.store"#, [I32Store(..)]);
        assert_insn!(r#"i64.store"#, [I64Store(..)]);
        assert_insn!(r#"f32.store"#, [F32Store(..)]);
        assert_insn!(r#"f64.store"#, [F64Store(..)]);
        assert_insn!(r#"i32.store8"#, [I32Store8(..)]);
        assert_insn!(r#"i32.store16"#, [I32Store16(..)]);
        assert_insn!(r#"i64.store8"#, [I64Store8(..)]);
        assert_insn!(r#"i64.store16"#, [I64Store16(..)]);
        assert_insn!(r#"i64.store32"#, [I64Store32(..)]);
        assert_insn!(r#"memory.size"#, [MemorySize]);
        assert_insn!(r#"memory.grow"#, [MemoryGrow]);

        assert_error!(
            r#"i32.load align=32 offset=10"#,
            Vec<Instruction<'_>>,
            UnexpectedKeyword("offset=10")
        );
        assert_error!(r#"i32.load align=pqr"#, Vec<Instruction<'_>>, CannotParseNum { .. });
        assert_error!(r#"i32.load align=0"#, Vec<Instruction<'_>>, InvalidAlignment(0));
        assert_error!(r#"i32.load align=7"#, Vec<Instruction<'_>>, InvalidAlignment(7));
    }

    #[test]
    fn const_instructions() {
        use InsnKind::*;
        assert_insn!(r#"i32.const 0"#, [I32Const(0)]);
        assert_insn!(r#"i32.const 42"#, [I32Const(42)]);
        assert_insn!(r#"i32.const 4_2"#, [I32Const(42)]);
        assert_insn!(r#"i32.const 0x1f"#, [I32Const(0x1f)]);
        assert_insn!(r#"i32.const -0"#, [I32Const(-0)]);
        assert_insn!(r#"i32.const -42"#, [I32Const(-42)]);
        assert_insn!(r#"i32.const -0x1f"#, [I32Const(-0x1f)]);
        assert_insn!(r#"i32.const 2147483647"#, [I32Const(2147483647)]); // INT32_MAX
        assert_insn!(r#"i32.const 0x7fffffff"#, [I32Const(0x7fffffff)]); // INT32_MAX
        assert_insn!(r#"i32.const -2147483648"#, [I32Const(-2147483648)]); // INT32_MIN
        assert_insn!(r#"i32.const -0x80000000"#, [I32Const(-0x80000000)]); // INT32_MAX
        assert_insn!(r#"i32.const 0xfedc6543"#, [I32Const(-19110589)]); // INT32_MAX < i < UINT32_MAX
        assert_insn!(r#"i32.const 4294967295"#, [I32Const(-1)]); // UINT32_MAX
        assert_insn!(r#"i32.const 0xffffffff"#, [I32Const(-1)]); // UINT32_MAX

        assert_error!(
            r#"i32.const 0.123"#,
            Vec<Instruction<'_>>,
            UnexpectedToken {
                expected: "integer for i32.const operand",
                ..
            }
        );
        // uint32_max + 1
        assert_error!(
            r#"i32.const 4294967296"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. } if reason == "too big integer"
        );
        // uint32_max + 1
        assert_error!(
            r#"i32.const 0x100000000"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. } if reason == "too big integer"
        );
        assert_error!(
            r#"i32.const -0x80000001"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. } if reason == "too small integer"
        );
        assert_error!(
            r#"i32.const -0x99999999"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. } if reason == "too small integer"
        );

        assert_insn!(r#"i64.const 0"#, [I64Const(0)]);
        assert_insn!(r#"i64.const 42"#, [I64Const(42)]);
        assert_insn!(r#"i64.const 4_2"#, [I64Const(42)]);
        assert_insn!(r#"i64.const 0x1f"#, [I64Const(0x1f)]);
        assert_insn!(r#"i64.const 0x1_f"#, [I64Const(0x1f)]);
        assert_insn!(r#"i64.const -42"#, [I64Const(-42)]);
        assert_insn!(r#"i64.const -0x1f"#, [I64Const(-0x1f)]);
        assert_insn!(
            r#"i64.const 9223372036854775807"#, // INT64_MAX
            [I64Const(9223372036854775807)]
        );
        assert_insn!(
            r#"i64.const -9223372036854775808"#, // INT64_MIN
            [I64Const(-9223372036854775808)]
        );
        assert_insn!(
            r#"i64.const 0x7fffffffffffffff"#, // INT64_MAX
            [I64Const(0x7fffffffffffffff)]
        );
        assert_insn!(
            r#"i64.const -0x8000000000000000"#, // INT64_MIN
            [I64Const(-0x8000000000000000)]
        );
        assert_insn!(
            r#"i64.const 0x8000000000000000"#, // INT64_MAX + 1
            [I64Const(-9223372036854775808)]
        );
        assert_insn!(
            r#"i64.const 0xffffffffffffffff"#, // UINT64_MAX
            [I64Const(-1)]
        );
        assert_insn!(
            r#"i64.const 18446744073709551615"#, // UINT64_MAX
            [I64Const(-1)]
        );

        assert_error!(
            r#"i64.const 0.123"#,
            Vec<Instruction<'_>>,
            UnexpectedToken {
                expected: "integer for i64.const operand",
                ..
            }
        );
        assert_error!(
            r#"i64.const 0x10000000000000000"#, // UINT64_MAX + 1
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. }
            if reason == "too big integer"
        );
        assert_error!(
            r#"i64.const 18446744073709551616"#, // UINT64_MAX + 1
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. }
            if reason == "too big integer"
        );
        assert_error!(
            r#"i64.const -0x8000000000000001"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. }
            if reason == "too small integer"
        );
        assert_error!(
            r#"i64.const -0x9999999999999999"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. }
            if reason == "too small integer"
        );

        assert_insn!(r#"f32.const 42"#, [F32Const(f)] if *f == 42.0);
        assert_insn!(r#"f32.const 0x42"#, [F32Const(f)] if *f == 66.0);
        assert_insn!(r#"f32.const 8589934592"#, [F32Const(f)] if *f > 85899e5 && *f < 85900e5);
        assert_insn!(r#"f32.const 42."#, [F32Const(f)] if *f == 42.0);
        assert_insn!(r#"f32.const 42.0"#, [F32Const(f)] if *f == 42.0);
        assert_insn!(r#"f32.const 4_2."#, [F32Const(f)] if *f == 42.0);
        assert_insn!(r#"f32.const 0x1f."#, [F32Const(f)] if *f == 31.0);
        assert_insn!(r#"f32.const -42."#, [F32Const(f)] if *f == -42.0);
        assert_insn!(r#"f32.const -4_2."#, [F32Const(f)] if *f == -42.0);
        assert_insn!(r#"f32.const -0x1f."#, [F32Const(f)] if *f == -31.0);
        assert_insn!(r#"f32.const 1.2_3"#, [F32Const(f)] if *f == 1.23);
        assert_insn!(r#"f32.const 1.2_3E3"#, [F32Const(f)] if *f == 1.23e3);
        assert_insn!(r#"f32.const 120.4E-3"#, [F32Const(f)] if *f == 120.4e-3);
        assert_insn!(r#"f32.const 99E+1_3"#, [F32Const(f)] if *f == 99e13);
        assert_insn!(r#"f32.const 0xe."#, [F32Const(f)] if *f == 0xe as f32);
        assert_insn!(r#"f32.const 0xe_f."#, [F32Const(f)] if *f == 0xef as f32);
        assert_insn!(r#"f32.const 0x1_f.2_e"#, [F32Const(f)] if *f == 0x1f as f32 + 2.0 / 16.0 + 14.0 / (16.0 * 16.0));
        assert_insn!(r#"f32.const 0xe.f"#, [F32Const(f)] if *f == 14.0 + 15.0 / 16.0);
        assert_insn!(r#"f32.const 0xe.fP3"#, [F32Const(f)] if *f == (14.0 + 15.0 / 16.0) * 2.0 * 2.0 * 2.0);
        assert_insn!(r#"f32.const 0xe.fP-1"#, [F32Const(f)] if *f == (14.0 + 15.0 / 16.0) / 2.0);
        assert_insn!(r#"f32.const 3.4E38"#, [F32Const(f)] if *f == 3.4E38); // ≈ (2 - 2^(-23)) * 2^127
        assert_insn!(r#"f32.const 2.E-149"#, [F32Const(f)] if *f == 2.0e-149);
        assert_insn!(r#"f32.const 1.4E-45"#, [F32Const(f)] if *f == 1.4E-45); // ≈ 2^(-149)
        assert_insn!(r#"f32.const inf"#, [F32Const(f)] if *f == f32::INFINITY);
        assert_insn!(r#"f32.const nan"#, [F32Const(f)] if f.is_nan());
        assert_insn!(r#"f32.const nan:0x401234"#, [F32Const(f)] if f.is_nan());
        assert_insn!(r#"f32.const nan:0x1"#, [F32Const(f)] if f.is_nan());
        assert_insn!(r#"f32.const -inf"#, [F32Const(f)] if *f == f32::NEG_INFINITY);
        assert_insn!(r#"f32.const -nan"#, [F32Const(f)] if f.is_nan());
        assert_insn!(r#"f32.const -nan:0x401234"#, [F32Const(f)] if f.is_nan());
        assert_insn!(r#"f32.const 1.32"#, [F32Const(f)] if *f == 1.32);

        assert_error!(
            r#"f32.const nan:0x0"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. }
            if reason == "payload of NaN must be in range of 1 <= payload < 2^23"
        );
        assert_error!(
            r#"f32.const nan:0x100_0000"#, // Larger than 0x80_0000
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. }
            if reason == "payload of NaN must be in range of 1 <= payload < 2^23"
        );

        assert_insn!(r#"f64.const 42"#, [F64Const(f)] if *f == 42.0);
        assert_insn!(r#"f64.const 0x42"#, [F64Const(f)] if *f == 66.0);
        assert_insn!(r#"f64.const 36893488147419103232"#, [F64Const(f)] if *f == 36893488147419103232.0);
        assert_insn!(r#"f64.const 42."#, [F64Const(f)] if *f == 42.0);
        assert_insn!(r#"f64.const 42.0"#, [F64Const(f)] if *f == 42.0);
        assert_insn!(r#"f64.const 4_2."#, [F64Const(f)] if *f == 42.0);
        assert_insn!(r#"f64.const 0x1f."#, [F64Const(f)] if *f == 31.0);
        assert_insn!(r#"f64.const -42."#, [F64Const(f)] if *f == -42.0);
        assert_insn!(r#"f64.const -4_2."#, [F64Const(f)] if *f == -42.0);
        assert_insn!(r#"f64.const -0x1f."#, [F64Const(f)] if *f == -31.0);
        assert_insn!(r#"f64.const 1.2_3"#, [F64Const(f)] if *f == 1.23);
        assert_insn!(r#"f64.const 1.2_3E3"#, [F64Const(f)] if *f == 1.23e3);
        assert_insn!(r#"f64.const 100.4E-2"#, [F64Const(f)] if *f == 100.4e-2);
        assert_insn!(r#"f64.const 99E+1_3"#, [F64Const(f)] if *f == 99e+13);
        assert_insn!(r#"f64.const 0xe."#, [F64Const(f)] if *f == 0xe as f64);
        assert_insn!(r#"f64.const 0xe_f."#, [F64Const(f)] if *f == 0xef as f64);
        assert_insn!(r#"f64.const 0x1_f.2_e"#, [F64Const(f)] if *f == 0x1f as f64 + 2.0 / 16.0 + 14.0 / (16.0 * 16.0));
        assert_insn!(r#"f64.const 0xe.f"#, [F64Const(f)] if *f == 14.0 + 15.0 / 16.0);
        assert_insn!(r#"f64.const 0xe.fP3"#, [F64Const(f)] if *f == (14.0 + 15.0 / 16.0) * 2.0 * 2.0 * 2.0);
        assert_insn!(r#"f64.const 0xe.fP-1"#, [F64Const(f)] if *f == (14.0 + 15.0 / 16.0) / 2.0);
        assert_insn!(r#"f64.const inf"#, [F64Const(f)] if *f == f64::INFINITY);
        assert_insn!(r#"f64.const nan"#, [F64Const(f)] if f.is_nan());
        assert_insn!(r#"f64.const nan:0x8_0000_0000_1245"#, [F64Const(f)] if f.is_nan());
        assert_insn!(r#"f64.const nan:0x1"#, [F64Const(f)] if f.is_nan());
        assert_insn!(r#"f64.const -inf"#, [F64Const(f)] if *f == f64::NEG_INFINITY);
        assert_insn!(r#"f64.const -nan"#, [F64Const(f)] if f.is_nan());
        assert_insn!(r#"f64.const -nan:0x8_0000_0000_1245"#, [F64Const(f)] if f.is_nan());

        assert_error!(
            r#"f64.const nan:0x0"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. }
            if reason == "payload of NaN must be in range of 1 <= payload < 2^52"
        );
        assert_error!(
            r#"f64.const nan:0x20_0000_0000_0000"#, // Larger than 0x10_0000_0000_0000
            Vec<Instruction<'_>>,
            CannotParseNum{ reason, .. }
            if reason == "payload of NaN must be in range of 1 <= payload < 2^52"
        );
    }

    #[test]
    fn numeric_instructions() {
        use InsnKind::*;
        assert_insn!(r#"i32.clz"#, [I32Clz]);
        assert_insn!(r#"i32.ctz"#, [I32Ctz]);
        assert_insn!(r#"i32.popcnt"#, [I32Popcnt]);
        assert_insn!(r#"i32.add"#, [I32Add]);
        assert_insn!(r#"i32.sub"#, [I32Sub]);
        assert_insn!(r#"i32.mul"#, [I32Mul]);
        assert_insn!(r#"i32.div_s"#, [I32DivS]);
        assert_insn!(r#"i32.div_u"#, [I32DivU]);
        assert_insn!(r#"i32.rem_s"#, [I32RemS]);
        assert_insn!(r#"i32.rem_u"#, [I32RemU]);
        assert_insn!(r#"i32.and"#, [I32And]);
        assert_insn!(r#"i32.or"#, [I32Or]);
        assert_insn!(r#"i32.xor"#, [I32Xor]);
        assert_insn!(r#"i32.shl"#, [I32Shl]);
        assert_insn!(r#"i32.shr_s"#, [I32ShrS]);
        assert_insn!(r#"i32.shr_u"#, [I32ShrU]);
        assert_insn!(r#"i32.rotl"#, [I32Rotl]);
        assert_insn!(r#"i32.rotr"#, [I32Rotr]);
        assert_insn!(r#"i64.clz"#, [I64Clz]);
        assert_insn!(r#"i64.ctz"#, [I64Ctz]);
        assert_insn!(r#"i64.popcnt"#, [I64Popcnt]);
        assert_insn!(r#"i64.add"#, [I64Add]);
        assert_insn!(r#"i64.sub"#, [I64Sub]);
        assert_insn!(r#"i64.mul"#, [I64Mul]);
        assert_insn!(r#"i64.div_s"#, [I64DivS]);
        assert_insn!(r#"i64.div_u"#, [I64DivU]);
        assert_insn!(r#"i64.rem_s"#, [I64RemS]);
        assert_insn!(r#"i64.rem_u"#, [I64RemU]);
        assert_insn!(r#"i64.and"#, [I64And]);
        assert_insn!(r#"i64.or"#, [I64Or]);
        assert_insn!(r#"i64.xor"#, [I64Xor]);
        assert_insn!(r#"i64.shl"#, [I64Shl]);
        assert_insn!(r#"i64.shr_s"#, [I64ShrS]);
        assert_insn!(r#"i64.shr_u"#, [I64ShrU]);
        assert_insn!(r#"i64.rotl"#, [I64Rotl]);
        assert_insn!(r#"i64.rotr"#, [I64Rotr]);
        assert_insn!(r#"f32.abs"#, [F32Abs]);
        assert_insn!(r#"f32.neg"#, [F32Neg]);
        assert_insn!(r#"f32.ceil"#, [F32Ceil]);
        assert_insn!(r#"f32.floor"#, [F32Floor]);
        assert_insn!(r#"f32.trunc"#, [F32Trunc]);
        assert_insn!(r#"f32.nearest"#, [F32Nearest]);
        assert_insn!(r#"f32.sqrt"#, [F32Sqrt]);
        assert_insn!(r#"f32.add"#, [F32Add]);
        assert_insn!(r#"f32.sub"#, [F32Sub]);
        assert_insn!(r#"f32.mul"#, [F32Mul]);
        assert_insn!(r#"f32.div"#, [F32Div]);
        assert_insn!(r#"f32.min"#, [F32Min]);
        assert_insn!(r#"f32.max"#, [F32Max]);
        assert_insn!(r#"f32.copysign"#, [F32Copysign]);
        assert_insn!(r#"f64.abs"#, [F64Abs]);
        assert_insn!(r#"f64.neg"#, [F64Neg]);
        assert_insn!(r#"f64.ceil"#, [F64Ceil]);
        assert_insn!(r#"f64.floor"#, [F64Floor]);
        assert_insn!(r#"f64.trunc"#, [F64Trunc]);
        assert_insn!(r#"f64.nearest"#, [F64Nearest]);
        assert_insn!(r#"f64.sqrt"#, [F64Sqrt]);
        assert_insn!(r#"f64.add"#, [F64Add]);
        assert_insn!(r#"f64.sub"#, [F64Sub]);
        assert_insn!(r#"f64.mul"#, [F64Mul]);
        assert_insn!(r#"f64.div"#, [F64Div]);
        assert_insn!(r#"f64.min"#, [F64Min]);
        assert_insn!(r#"f64.max"#, [F64Max]);
        assert_insn!(r#"f64.copysign"#, [F64Copysign]);
        assert_insn!(r#"i32.eqz"#, [I32Eqz]);
        assert_insn!(r#"i32.eq"#, [I32Eq]);
        assert_insn!(r#"i32.ne"#, [I32Ne]);
        assert_insn!(r#"i32.lt_s"#, [I32LtS]);
        assert_insn!(r#"i32.lt_u"#, [I32LtU]);
        assert_insn!(r#"i32.gt_s"#, [I32GtS]);
        assert_insn!(r#"i32.gt_u"#, [I32GtU]);
        assert_insn!(r#"i32.le_s"#, [I32LeS]);
        assert_insn!(r#"i32.le_u"#, [I32LeU]);
        assert_insn!(r#"i32.ge_s"#, [I32GeS]);
        assert_insn!(r#"i32.ge_u"#, [I32GeU]);
        assert_insn!(r#"i64.eqz"#, [I64Eqz]);
        assert_insn!(r#"i64.eq"#, [I64Eq]);
        assert_insn!(r#"i64.ne"#, [I64Ne]);
        assert_insn!(r#"i64.lt_s"#, [I64LtS]);
        assert_insn!(r#"i64.lt_u"#, [I64LtU]);
        assert_insn!(r#"i64.gt_s"#, [I64GtS]);
        assert_insn!(r#"i64.gt_u"#, [I64GtU]);
        assert_insn!(r#"i64.le_s"#, [I64LeS]);
        assert_insn!(r#"i64.le_u"#, [I64LeU]);
        assert_insn!(r#"i64.ge_s"#, [I64GeS]);
        assert_insn!(r#"i64.ge_u"#, [I64GeU]);
        assert_insn!(r#"f32.eq"#, [F32Eq]);
        assert_insn!(r#"f32.ne"#, [F32Ne]);
        assert_insn!(r#"f32.lt"#, [F32Lt]);
        assert_insn!(r#"f32.gt"#, [F32Gt]);
        assert_insn!(r#"f32.le"#, [F32Le]);
        assert_insn!(r#"f32.ge"#, [F32Ge]);
        assert_insn!(r#"f64.eq"#, [F64Eq]);
        assert_insn!(r#"f64.ne"#, [F64Ne]);
        assert_insn!(r#"f64.lt"#, [F64Lt]);
        assert_insn!(r#"f64.gt"#, [F64Gt]);
        assert_insn!(r#"f64.le"#, [F64Le]);
        assert_insn!(r#"f64.ge"#, [F64Ge]);
        assert_insn!(r#"i32.wrap_i64"#, [I32WrapI64]);
        assert_insn!(r#"i32.trunc_f32_s"#, [I32TruncF32S]);
        assert_insn!(r#"i32.trunc_f32_u"#, [I32TruncF32U]);
        assert_insn!(r#"i32.trunc_f64_s"#, [I32TruncF64S]);
        assert_insn!(r#"i32.trunc_f64_u"#, [I32TruncF64U]);
        assert_insn!(r#"i64.extend_i32_s"#, [I64ExtendI32S]);
        assert_insn!(r#"i64.extend_i32_u"#, [I64ExtendI32U]);
        assert_insn!(r#"i64.trunc_f32_s"#, [I64TruncF32S]);
        assert_insn!(r#"i64.trunc_f32_u"#, [I64TruncF32U]);
        assert_insn!(r#"i64.trunc_f64_s"#, [I64TruncF64S]);
        assert_insn!(r#"i64.trunc_f64_u"#, [I64TruncF64U]);
        assert_insn!(r#"f32.convert_i32_s"#, [F32ConvertI32S]);
        assert_insn!(r#"f32.convert_i32_u"#, [F32ConvertI32U]);
        assert_insn!(r#"f32.convert_i64_s"#, [F32ConvertI64S]);
        assert_insn!(r#"f32.convert_i64_u"#, [F32ConvertI64U]);
        assert_insn!(r#"f32.demote_f64"#, [F32DemoteF64]);
        assert_insn!(r#"f64.convert_i32_s"#, [F64ConvertI32S]);
        assert_insn!(r#"f64.convert_i32_u"#, [F64ConvertI32U]);
        assert_insn!(r#"f64.convert_i64_s"#, [F64ConvertI64S]);
        assert_insn!(r#"f64.convert_i64_u"#, [F64ConvertI64U]);
        assert_insn!(r#"f64.promote_f32"#, [F64PromoteF32]);
        assert_insn!(r#"i32.reinterpret_f32"#, [I32ReinterpretF32]);
        assert_insn!(r#"i64.reinterpret_f64"#, [I64ReinterpretF64]);
        assert_insn!(r#"f32.reinterpret_i32"#, [F32ReinterpretI32]);
        assert_insn!(r#"f64.reinterpret_i64"#, [F64ReinterpretI64]);
        assert_insn!(r#"i32.extend8_s"#, [I32Extend8S]);
        assert_insn!(r#"i32.extend16_s"#, [I32Extend16S]);
        assert_insn!(r#"i64.extend8_s"#, [I64Extend8S]);
        assert_insn!(r#"i64.extend16_s"#, [I64Extend16S]);
        assert_insn!(r#"i64.extend32_s"#, [I64Extend32S]);
    }

    #[test]
    fn elem_segment() {
        use InsnKind::*;
        assert_parse!(
            r#"(elem i32.const 10)"#,
            Elem<'_>,
            Elem {
                idx: Index::Num(0),
                offset,
                init,
                ..
            } if matches!(offset[0].kind, I32Const(10)) && init.is_empty()
        );
        assert_parse!(
            r#"(elem 0x1f i32.const 10)"#,
            Elem<'_>,
            Elem {
                idx: Index::Num(0x1f),
                offset,
                init,
                ..
            } if matches!(offset[0].kind, I32Const(10)) && init.is_empty()
        );
        assert_parse!(
            r#"(elem $e i32.const 10)"#,
            Elem<'_>,
            Elem {
                idx: Index::Ident("$e"),
                offset,
                init,
                ..
            } if matches!(offset[0].kind, I32Const(10)) && init.is_empty()
        );
        assert_parse!(
            r#"(elem (offset))"#,
            Elem<'_>,
            Elem {
                offset,
                init,
                ..
            } if offset.is_empty() && init.is_empty()
        );
        assert_parse!(
            r#"(elem (offset nop))"#,
            Elem<'_>,
            Elem {
                offset,
                init,
                ..
            } if matches!(offset[0].kind, Nop) && init.is_empty()
        );
        assert_parse!(
            r#"(elem (offset (nop (nop))))"#,
            Elem<'_>,
            Elem {
                offset,
                init,
                ..
            } if offset.len() == 2 && init.is_empty()
        );
        assert_parse!(
            r#"(elem (offset nop nop))"#,
            Elem<'_>,
            Elem {
                offset,
                init,
                ..
            } if offset.len() == 2 && init.is_empty()
        );
        assert_parse!(
            r#"(elem (offset nop) 0xf $f)"#,
            Elem<'_>,
            Elem {
                offset,
                init,
                ..
            } if offset.len() == 1 &&
                 matches!(init.as_slice(), [Index::Num(0xf), Index::Ident("$f")])
        );
        assert_parse!(
            r#"(elem nop 0xf)"#,
            Elem<'_>,
            Elem {
                offset,
                init,
                ..
            } if matches!(offset[0].kind, Nop) &&
                 matches!(init.as_slice(), [Index::Num(0xf)])
        );
        assert_parse!(
            r#"(elem nop $f)"#,
            Elem<'_>,
            Elem {
                offset,
                init,
                ..
            } if matches!(offset[0].kind, Nop) &&
                 matches!(init.as_slice(), [Index::Ident("$f")])
        );
        assert_parse!(
            r#"(elem block end 0)"#,
            Elem<'_>,
            Elem { offset, .. } if matches!(offset[0].kind, Block{..})
        );
        assert_parse!(
            r#"(elem (i32.const 42) 0)"#,
            Elem<'_>,
            Elem { offset, .. } if matches!(offset[0].kind, I32Const(42))
        );
        assert_parse!(r#"(elem i32.const 0 func $f)"#, Elem<'_>, Elem { .. });
    }

    #[test]
    fn table_section_abbrev() {
        assert_parse!(
            r#"(table 0 0 funcref)"#,
            TableAbbrev<'_>,
            TableAbbrev::Table(Table { id: None, .. })
        );
        assert_parse!(
            r#"(table $tbl 0 0 funcref)"#,
            TableAbbrev<'_>,
            TableAbbrev::Table(Table { id: Some("$tbl"), .. })
        );
        assert_parse!(
            r#"(table $tbl funcref (elem 0 1))"#,
            TableAbbrev<'_>,
            TableAbbrev::Elem(
                Table{ id: Some("$tbl"), ty: TableType{ limit: Limits::Range{ min: 2, max: 2 } }, .. },
                Elem{ idx: Index::Num(0), offset, init, .. }
            )
            if matches!(offset[0].kind, InsnKind::I32Const(0)) &&
               matches!(init[0], Index::Num(0)) && matches!(init[1], Index::Num(1))
        );
        assert_parse!(
            r#"(table $tbl (import "m" "n") 2 2 funcref)"#,
            TableAbbrev<'_>,
            TableAbbrev::Table(Table {
                id: Some("$tbl"),
                ty: TableType {
                    limit: Limits::Range{ min: 2, max: 2 },
                },
                import: Some(Import {
                    mod_name: Name(m),
                    name: Name(n),
                }),
                ..
            })
            if m == "m" && n == "n"
        );
        let parser = assert_parse!(
            r#"(table $tbl (export "n") 2 funcref)"#,
            TableAbbrev<'_>,
            TableAbbrev::Table(Table {
                id: Some("$tbl"),
                ty: TableType {
                    limit: Limits::From { min: 2 }
                },
                ..
            })
        );
        match &parser.ctx.exports[0] {
            Export {
                name: Name(n),
                kind: ExportKind::Table,
                idx: Index::Num(0),
                ..
            } if n == "n" => { /* OK */ }
            e => panic!("did not match: {:?}", e),
        }
        let parser = assert_parse!(
            r#"(table $tbl (export "n1") (export "n2") 2 funcref)"#,
            TableAbbrev<'_>,
            TableAbbrev::Table(Table {
                id: Some("$tbl"),
                ty: TableType {
                    limit: Limits::From { min: 2 }
                },
                ..
            })
        );
        assert_eq!(parser.ctx.exports.len(), 2);
        let parser = assert_parse!(
            r#"(table $tbl (export "n1") (import "m" "n2") 2 funcref)"#,
            TableAbbrev<'_>,
            TableAbbrev::Table(Table {
                id: Some("$tbl"),
                ty: TableType {
                    limit: Limits::From{ min: 2 },
                },
                import: Some(Import {
                    mod_name: Name(m),
                    name: Name(n),
                }),
                ..
            })
            if m == "m" && n == "n2"
        );
        assert_eq!(parser.ctx.exports.len(), 1);
        match &parser.ctx.exports[0] {
            Export {
                name: Name(n),
                kind: ExportKind::Table,
                idx: Index::Num(0),
                ..
            } if n == "n1" => { /* OK */ }
            e => panic!("did not match: {:?}", e),
        }
        let parser = assert_parse!(
            r#"(table $tbl (export "n1") (export "n2") (import "m" "n3") 2 funcref)"#,
            TableAbbrev<'_>,
            TableAbbrev::Table(Table { import: Some(_), .. })
        );
        assert_eq!(parser.ctx.exports.len(), 2);
        let parser = assert_parse!(
            r#"(table $tbl (export "n1") funcref (elem 1 2 3))"#,
            TableAbbrev<'_>,
            TableAbbrev::Elem(..)
        );
        assert_eq!(parser.ctx.exports.len(), 1);

        assert_error!(
            r#"(table $t (import "m" "n") (export "n2") 2 funcref)"#,
            TableAbbrev<'_>,
            UnexpectedToken { .. }
        );
        assert_error!(
            r#"(table $t funcref (elem 1) (export "n"))"#,
            TableAbbrev<'_>,
            MissingParen { paren: ')', .. }
        );
    }

    #[test]
    fn data_segment() {
        assert_parse!(
            r#"(data 0 i32.const 0)"#,
            Data<'_>,
            Data{
                idx: Index::Num(0),
                offset,
                data,
                ..
            } if matches!(offset[0].kind, InsnKind::I32Const(0)) && data.is_empty()
        );
        assert_parse!(
            r#"(data 0 (offset i32.const 0))"#,
            Data<'_>,
            Data{
                idx: Index::Num(0),
                offset,
                data,
                ..
            } if matches!(offset[0].kind, InsnKind::I32Const(0)) && data.is_empty()
        );
        assert_parse!(
            r#"(data 0 (offset i32.const 0) "hello")"#,
            Data<'_>,
            Data{ data, ..  } if data.as_ref() == b"hello".as_ref()
        );
        assert_parse!(
            r#"(data 0 (offset i32.const 0) "hello" " dogs!")"#,
            Data<'_>,
            Data{ data, ..  } if data.as_ref() == b"hello dogs!".as_ref()
        );
        assert_parse!(
            r#"(data 0 (offset i32.const 0) "\t\n\r" "\"\'\\" "\u{3042}\41")"#,
            Data<'_>,
            Data{ data, ..  } if data.as_ref() == b"\t\n\r\"'\\\xe3\x81\x82A".as_ref()
        );
        assert_parse!(
            r#"(data 0 (offset i32.const 0) "\01\02\03\ff")"#,
            Data<'_>,
            Data{ data, ..  } if data.as_ref() == &[1, 2, 3, 255]
        );
        assert_parse!(
            r#"(data (i32.const 1024) "Hello, world\n\00")"#,
            Data<'_>,
            Data{
                idx: Index::Num(0),
                offset,
                data,
                ..
            } if matches!(offset[0].kind, InsnKind::I32Const(1024)) &&
                 data.as_ref() == b"Hello, world\n\0".as_ref()
        );

        assert_error!(r#"(data 0 "hello")"#, Data<'_>, UnexpectedToken { .. });
    }

    #[test]
    fn memory_section_abbrev() {
        assert_parse!(
            r#"(memory 3)"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Memory(Memory {
                id: None,
                ty: MemType {
                    limit: Limits::From { min: 3 }
                },
                ..
            })
        );
        assert_parse!(
            r#"(memory 1 3)"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Memory(Memory {
                id: None,
                ty: MemType {
                    limit: Limits::Range { min: 1, max: 3 },
                },
                ..
            })
        );
        assert_parse!(
            r#"(memory $m 1 3)"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Memory(Memory {
                id: Some("$m"),
                ty: MemType {
                    limit: Limits::Range { min: 1, max: 3 },
                },
                ..
            })
        );
        assert_parse!(
            r#"(memory $m (data "foo" "bar"))"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Data(
                Memory{
                    ty: MemType{ limit: Limits::Range{ min: 1, max: 1 } },
                    ..
                },
                Data {
                    idx: Index::Num(0),
                    offset,
                    data,
                    ..
                },
            )
            if matches!(offset[0].kind, InsnKind::I32Const(0)) &&
               data.as_ref() == b"foobar".as_ref()
        );
        assert_parse!(
            r#"(memory $m (import "m" "n") 2)"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Memory(Memory {
                id: Some("$m"),
                ty: MemType {
                    limit: Limits::From{ min: 2 },
                },
                import: Some(Import {
                    mod_name: Name(m),
                    name: Name(n),
                }),
                ..
            })
            if m == "m" && n == "n"
        );
        let parser = assert_parse!(
            r#"(memory $m (export "n") 0)"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Memory(Memory {
                ty: MemType {
                    limit: Limits::From { min: 0 }
                },
                ..
            })
        );
        match &parser.ctx.exports[0] {
            Export {
                name: Name(n),
                kind: ExportKind::Memory,
                idx: Index::Num(0),
                ..
            } if n == "n" => { /* OK */ }
            e => panic!("did not match: {:?}", e),
        }
        let parser = assert_parse!(
            r#"(memory $m (export "n") (export "n2") 0)"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Memory(Memory {
                ty: MemType {
                    limit: Limits::From { min: 0 }
                },
                ..
            })
        );
        assert_eq!(parser.ctx.exports[0].name.0, "n");
        assert_eq!(parser.ctx.exports[1].name.0, "n2");
        let parser = assert_parse!(
            r#"(memory $m (export "e") (import "m" "n") 2)"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Memory(Memory {
                id: Some("$m"),
                ty: MemType {
                    limit: Limits::From{ min: 2 },
                },
                import: Some(Import {
                    mod_name: Name(m),
                    name: Name(n),
                }),
                ..
            })
            if m == "m" && n == "n"
        );
        match &parser.ctx.exports[0] {
            Export {
                name: Name(n),
                kind: ExportKind::Memory,
                idx: Index::Num(0),
                ..
            } if n == "e" => { /* OK */ }
            e => panic!("did not match: {:?}", e),
        }
        let parser = assert_parse!(
            r#"(memory $m (export "e1") (export "e2") (import "m" "n") 2)"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Memory(Memory { import: Some(_), .. })
        );
        assert_eq!(parser.ctx.exports.len(), 2);
        let parser = assert_parse!(
            r#"(memory $m (export "e") (data "hello"))"#,
            MemoryAbbrev<'_>,
            MemoryAbbrev::Data(..)
        );
        assert_eq!(parser.ctx.exports.len(), 1);

        assert_error!(
            r#"(memory $m (import "m" "n") (export "n2") 0)"#,
            MemoryAbbrev<'_>,
            UnexpectedToken { .. }
        );
        assert_error!(
            r#"(memory $m (data "hello") (export "n"))"#,
            MemoryAbbrev<'_>,
            MissingParen { paren: ')', .. }
        );
    }

    #[test]
    fn global_section_abbrev() {
        assert_parse!(
            r#"(global i32)"#,
            Global<'_>,
            Global {
                id: None,
                ty: GlobalType {
                    mutable: false,
                    ty: ValType::I32,
                },
                kind: GlobalKind::Init(init),
                ..
            } if init.is_empty()
        );
        assert_parse!(
            r#"(global $g i32)"#,
            Global<'_>,
            Global {
                id: Some("$g"),
                ty: GlobalType {
                    mutable: false,
                    ty: ValType::I32,
                },
                kind: GlobalKind::Init(init),
                ..
            } if init.is_empty()
        );
        assert_parse!(
            r#"(global (mut i32))"#,
            Global<'_>,
            Global {
                ty: GlobalType {
                    mutable: true,
                    ty: ValType::I32,
                },
                kind: GlobalKind::Init(init),
                ..
            } if init.is_empty()
        );
        let parser = assert_parse!(
            r#"(global $g (export "n") i32)"#,
            Global<'_>,
            Global {
                ty: GlobalType {
                    mutable: false,
                    ty: ValType::I32
                },
                ..
            }
        );
        match &parser.ctx.exports[0] {
            Export {
                name: Name(n),
                kind: ExportKind::Global,
                idx: Index::Num(0),
                ..
            } if n == "n" => { /* OK */ }
            e => panic!("did not match: {:?}", e),
        }
        let parser = assert_parse!(
            r#"(global $g (export "n1") (export "n2") i32)"#,
            Global<'_>,
            Global { .. }
        );
        assert_eq!(parser.ctx.exports[0].name.0, "n1");
        assert_eq!(parser.ctx.exports[1].name.0, "n2");
        assert_parse!(
            r#"(global $g (import "m" "n") i32)"#,
            Global<'_>,
            Global {
                ty: GlobalType {
                    mutable: false,
                    ty: ValType::I32
                },
                id: Some("$g"),
                kind: GlobalKind::Import(Import {
                    mod_name: Name(m),
                    name: Name(n),
                }),
                ..
            }
            if m == "m" && n == "n"
        );
        let parser = assert_parse!(
            r#"(global $g (export "e") (import "m" "n") i32)"#,
            Global<'_>,
            Global {
                kind: GlobalKind::Import(_),
                ..
            }
        );
        assert_eq!(parser.ctx.exports.len(), 1);
        assert_parse!(
            r#"(global i32 i32.const 32 i32.load align=8)"#,
            Global<'_>,
            Global { kind: GlobalKind::Init(init), .. }
            if matches!(&init[0].kind, InsnKind::I32Const(32)) &&
               matches!(&init[1].kind, InsnKind::I32Load(Mem{ align: 3, .. }))
        );
        assert_parse!(
            r#"(global i32 (i32.add (i32.const 4)))"#,
            Global<'_>,
            Global { kind: GlobalKind::Init(init), .. }
            if matches!(&init[0].kind, InsnKind::I32Const(4)) &&
               matches!(&init[1].kind, InsnKind::I32Add)
        );

        assert_error!(
            r#"(global $g (import "m" "n") (export "e") i32)"#,
            Global<'_>,
            UnexpectedToken { .. }
        );
        assert_error!(
            r#"(global $g (mut i32) (export "e"))"#,
            Global<'_>,
            UnexpectedKeyword("export")
        );
    }

    #[test]
    fn start_function() {
        assert_parse!(r#"(start 3)"#, Start<'_>, Start { idx: Index::Num(3), .. });
        assert_parse!(
            r#"(start $f)"#,
            Start<'_>,
            Start {
                idx: Index::Ident("$f"),
                ..
            }
        );
    }

    #[test]
    fn hex_float_out_of_range() {
        macro_rules! assert_out_of_range {
            ($lit:expr) => {
                assert_error!(
                    $lit,
                    Vec<Instruction<'_>>,
                    CannotParseNum { reason, .. } if reason == "float constant out of range"
                );
            }
        }

        assert_out_of_range!("f32.const 0x1p128");
        assert_out_of_range!("f32.const -0x1p128");
        assert_out_of_range!("f32.const 0x1.ffffffp127");
        assert_out_of_range!("f32.const -0x1.ffffffp127");
        assert_out_of_range!("f64.const 0x1p1024");
        assert_out_of_range!("f64.const -0x1p1024");
        assert_out_of_range!("f64.const 0x1.fffffffffffff8p1023");
        assert_out_of_range!("f64.const -0x1.fffffffffffff8p1023");
    }

    #[test]
    fn hex_f32_literal_edge_cases() {
        use InsnKind::F32Const;

        macro_rules! assert_f32_eq {
            ($left:expr, $right:expr) => {
                assert_insn!(
                    concat!("(f32.const ", $left, ") (f32.const ", $right, ")"),
                    [F32Const(a), F32Const(b)] if a == b
                );
            }
        }

        // f32, small exponent
        assert_f32_eq!("+0x1.00000100000000001p-50", "+0x1.000002p-50");
        assert_f32_eq!("-0x1.00000100000000001p-50", "-0x1.000002p-50");
        assert_f32_eq!("+0x1.000001fffffffffffp-50", "+0x1.000002p-50");
        assert_f32_eq!("-0x1.000001fffffffffffp-50", "-0x1.000002p-50");
        assert_f32_eq!("+0x1.00000500000000001p-50", "+0x1.000006p-50");
        assert_f32_eq!("-0x1.00000500000000001p-50", "-0x1.000006p-50");
        assert_f32_eq!("+0x4000.004000001p-64", "+0x1.000002p-50");
        assert_f32_eq!("-0x4000.004000001p-64", "-0x1.000002p-50");
        assert_f32_eq!("+0x4000.014000001p-64", "+0x1.000006p-50");
        assert_f32_eq!("-0x4000.014000001p-64", "-0x1.000006p-50");

        // f32, large exponent
        assert_f32_eq!("+0x1.00000100000000001p+50", "+0x1.000002p+50");
        assert_f32_eq!("-0x1.00000100000000001p+50", "-0x1.000002p+50");
        assert_f32_eq!("+0x1.000001fffffffffffp+50", "+0x1.000002p+50");
        assert_f32_eq!("-0x1.000001fffffffffffp+50", "-0x1.000002p+50");
        assert_f32_eq!("+0x1.00000500000000001p+50", "+0x1.000006p+50");
        assert_f32_eq!("-0x1.00000500000000001p+50", "-0x1.000006p+50");
        assert_f32_eq!("+0x4000004000001", "+0x1.000002p+50");
        assert_f32_eq!("-0x4000004000001", "-0x1.000002p+50");

        // f32, subnormal
        assert_f32_eq!("+0x0.00000100000000001p-126", "+0x0.000002p-126");
        assert_f32_eq!("-0x0.00000100000000001p-126", "-0x0.000002p-126");
        assert_f32_eq!("+0x0.000002fffffffffffp-126", "+0x0.000002p-126");
        assert_f32_eq!("-0x0.000002fffffffffffp-126", "-0x0.000002p-126");
        assert_f32_eq!("+0x0.00000500000000001p-126", "+0x0.000006p-126");
        assert_f32_eq!("-0x0.00000500000000001p-126", "-0x0.000006p-126");
    }

    #[test]
    fn hex_f64_literal_edge_cases() {
        use InsnKind::F64Const;

        macro_rules! assert_f64_eq {
            ($left:expr, $right:expr) => {
                assert_insn!(
                    concat!("(f64.const ", $left, ") (f64.const ", $right, ")"),
                    [F64Const(a), F64Const(b)] if a == b
                );
            }
        }

        // f64, small exponent
        assert_f64_eq!("+0x1.000000000000080000000001p-600", "+0x1.0000000000001p-600");
        assert_f64_eq!("-0x1.000000000000080000000001p-600", "-0x1.0000000000001p-600");
        assert_f64_eq!("+0x1.000000000000280000000001p-600", "+0x1.0000000000003p-600");
        assert_f64_eq!("-0x1.000000000000280000000001p-600", "-0x1.0000000000003p-600");
        assert_f64_eq!("+0x8000000.000000400000000001p-627", "+0x1.0000000000001p-600");
        assert_f64_eq!("-0x8000000.000000400000000001p-627", "-0x1.0000000000001p-600");
        assert_f64_eq!("+0x8000000.000001400000000001p-627", "+0x1.0000000000003p-600");
        assert_f64_eq!("-0x8000000.000001400000000001p-627", "-0x1.0000000000003p-600");

        // f64, large exponent
        assert_f64_eq!("+0x1.000000000000080000000001p+600", "+0x1.0000000000001p+600");
        assert_f64_eq!("-0x1.000000000000080000000001p+600", "-0x1.0000000000001p+600");
        assert_f64_eq!("+0x1.000000000000280000000001p+600", "+0x1.0000000000003p+600");
        assert_f64_eq!("-0x1.000000000000280000000001p+600", "-0x1.0000000000003p+600");
        assert_f64_eq!("+0x2000000000000100000000001", "+0x1.0000000000001p+97");
        assert_f64_eq!("-0x2000000000000100000000001", "-0x1.0000000000001p+97");
        assert_f64_eq!("+0x20000000000001fffffffffff", "+0x1.0000000000001p+97");
        assert_f64_eq!("-0x20000000000001fffffffffff", "-0x1.0000000000001p+97");
        assert_f64_eq!("+0x2000000000000500000000001", "+0x1.0000000000003p+97");
        assert_f64_eq!("-0x2000000000000500000000001", "-0x1.0000000000003p+97");

        // f64, subnormal
        assert_f64_eq!("+0x1.000000000000280000000001p-1022", "+0x1.0000000000003p-1022");
        assert_f64_eq!("-0x1.000000000000280000000001p-1022", "-0x1.0000000000003p-1022");
    }

    #[test]
    fn hello_world() {
        assert_parse!(
            r#"
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
            "#,
            Module<'_>,
            Module { .. }
        );
    }
}
