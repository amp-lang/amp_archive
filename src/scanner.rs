use unicode_xid::UnicodeXID;

use crate::{
    error::{Error, NumberRadix},
    span::{FileId, Span},
};

const ONE_BYTE_MASK: u32 = 0x80;
const ONE_BYTE_BITS: u32 = 0x00;
const TWO_BYTES_MASK: u32 = 0xE0;
const TWO_BYTES_BITS: u32 = 0xC0;
const THREE_BYTES_MASK: u32 = 0xF0;
const THREE_BYTES_BITS: u32 = 0xE0;
const FOUR_BYTES_MASK: u32 = 0xF8;
const FOUR_BYTES_BITS: u32 = 0xF0;
const CONTINUATION_MASK: u32 = 0xC0;
// const CONTINUATION_BITS: u32 = 0x80;

#[inline(always)]
fn utf8_codepoint_size(first_byte: u8) -> usize {
    if (first_byte as u32 & ONE_BYTE_MASK) == ONE_BYTE_BITS {
        return 1;
    } else if (first_byte as u32 & TWO_BYTES_MASK) == TWO_BYTES_BITS {
        return 2;
    } else if (first_byte as u32 & THREE_BYTES_MASK) == THREE_BYTES_BITS {
        return 3;
    } else if (first_byte as u32 & FOUR_BYTES_MASK) == FOUR_BYTES_BITS {
        return 4;
    }

    unreachable!("Invalid UTF-8 character.");
}

/// Reads a character from a slice.  Assumes that the slice is valid UTF-8.
///
/// # Panics
/// Panics if the slice is not valid UTF-8.
fn char_from_slice(buffer: &[u8]) -> (char, usize) {
    let size = utf8_codepoint_size(buffer[0]);

    match size {
        1 => (
            char::from_u32(buffer[0] as u32 & !ONE_BYTE_MASK).unwrap(),
            size,
        ),
        2 => (
            char::from_u32(
                (buffer[0] as u32 & !TWO_BYTES_MASK) << 6 | (buffer[1] as u32 & !CONTINUATION_MASK),
            )
            .unwrap(),
            size,
        ),
        3 => (
            char::from_u32(
                (buffer[0] as u32 & !THREE_BYTES_MASK) << 12
                    | (buffer[1] as u32 & !CONTINUATION_MASK) << 6
                    | (buffer[2] as u32 & !CONTINUATION_MASK),
            )
            .unwrap(),
            size,
        ),
        4 => (
            char::from_u32(
                (buffer[0] as u32 & !FOUR_BYTES_MASK) << 18
                    | (buffer[1] as u32 & !CONTINUATION_MASK) << 12
                    | (buffer[2] as u32 & !CONTINUATION_MASK) << 6
                    | (buffer[3] as u32 & !CONTINUATION_MASK),
            )
            .unwrap(),
            size,
        ),
        _ => unreachable!("Invalid UTF-8 character."),
    }
}

/// A token from a [Scanner].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Token {
    /// `-`
    Minus,

    /// `->`
    Arrow,

    /// `:`
    Colon,

    /// `,`
    Comma,

    /// `.`
    Dot,

    /// `==`
    EqEq,

    /// `=`
    Eq,

    /// `!=`
    BangEq,

    /// `!`
    Bang,

    /// `;`
    Semi,

    /// `~`
    Tilde,

    /// `*`
    Star,

    /// `+`
    Plus,

    /// `/`
    Slash,

    /// `%`
    Percent,

    /// `(`
    LParen,

    /// `)`
    RParen,

    /// `.{`
    Constructor,

    /// `{`
    LBrace,

    /// `}`
    RBrace,

    /// `[`
    LBrack,

    /// `]`
    RBrack,

    /// A Unicode XID identifier.
    ///
    /// ```amp
    /// my_identifier
    /// _my_identifier
    /// my123identifier
    /// ```
    Identifier,

    /// The `func` keyword.
    ///
    /// ```amp
    /// func Main() {}
    /// ```
    KFunc,

    /// The `var` keyword
    ///
    /// ```
    /// var my_variable = 42;
    /// ```
    KVar,

    /// The `struct` keyword.
    ///
    /// ```amp
    /// struct MyStruct {
    ///     member: i32,
    /// }
    /// ```
    KStruct,

    /// A `const` keyword.
    ///
    /// ```amp
    /// ~const u8
    /// ```
    KConst,

    /// A `mut` keyword
    ///
    /// ```amp
    /// ~mut u8
    /// ```
    KMut,

    /// The `return` keyword.
    ///
    /// ```amp
    /// return 0;
    /// ```
    KReturn,

    /// The `true` keyword.
    KTrue,

    /// The `false` keyword.
    KFalse,

    /// The `while` keyword.
    ///
    /// ```amp
    /// while {
    ///     Print("Hello, world!");
    /// }
    /// ```
    KWhile,

    /// A normal, non-suspicious string literal.
    ///
    /// ```amp
    /// "Hello, world!"
    /// "Hello,\nworld!"
    /// "hello,
    /// world!"
    /// ```
    String,

    /// A hexadecimal literal.
    ///
    /// ```amp
    /// 0x1234
    /// 0x1234_5678
    /// ```
    Hex,

    /// A binary literal.
    ///
    /// ```amp
    /// 0x0101
    /// 0x0101_0101
    /// ```
    Binary,

    /// An octal literal.
    ///
    /// ```amp
    /// 0o01234567
    /// 0o0123_4567
    /// ```
    Octal,

    /// A decimal literal.
    ///
    /// ```amp
    /// 1234_5678
    /// ```
    Decimal,

    /// A floating point number.
    ///
    /// ```amp
    /// 100_000e1
    /// 1.0e+1
    /// 1.0e-1
    /// ```
    Float,
}

// TODO: implement other operators.
impl Token {
    /// Returns `true` if this token is a binary operator.
    ///
    /// # Notes
    /// Also returns `true` for the function call operation (`()`).
    pub fn is_binary_operator(&self) -> bool {
        match self {
            // The left parenthesis is a special operator.
            Token::LParen => true,
            Token::EqEq => true,
            Token::Eq => true,
            Token::BangEq => true,
            Token::Constructor => true,
            Token::Dot => true,
            Token::Star => true,
            Token::Plus => true,
            Token::Slash => true,
            Token::Minus => true,
            Token::Percent => true,
            _ => false,
        }
    }

    /// Returns `true` if this token is a unary operator.
    pub fn is_unary_operator(&self) -> bool {
        match self {
            Token::Star => true,
            Token::Tilde => true,
            _ => false,
        }
    }

    /// Returns the binding power of the token.
    ///
    /// # Panics
    /// Panics if the token is not a binary operator.
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            Token::Eq => (0, 1),
            Token::EqEq => (1, 2),
            Token::BangEq => (1, 2),
            Token::Plus => (4, 5),
            Token::Minus => (4, 5),
            Token::Star => (5, 6),
            Token::Slash => (5, 6),
            Token::Percent => (5, 6),
            // cast expressions are (6, 7)
            Token::LParen => (8, 7),
            Token::Constructor => (8, 7),
            Token::Dot => (8, 7),
            _ => unreachable!("Invalid operator."),
        }
    }

    /// Returns the prefix binding power of the token.
    ///
    /// # Panics
    /// Panics if the token is not a unary operator.
    pub fn prefix_binding_power(&self) -> u8 {
        match self {
            Token::Star => 5,
            Token::Tilde => 5,
            _ => unreachable!("Invalid operator."),
        }
    }
}

/// The state of the lexical scanner.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Scanner<'a> {
    src: &'a [u8],

    /// The span of the current token.
    current_span: Span,
}

impl<'a> Scanner<'a> {
    /// Creates a new [Scanner].
    pub fn new(file_id: FileId, src: &'a str) -> Self {
        Self {
            src: src.as_bytes(),
            current_span: Span::new(file_id, 0u32, 0u32),
        }
    }

    /// Returns the ID of the file being scanned.
    #[inline]
    pub fn file_id(&self) -> FileId {
        self.current_span.file_id
    }

    /// Returns the current span of the [Scanner].
    #[inline]
    pub fn span(&self) -> Span {
        self.current_span
    }

    /// Returns the current slice of the [Scanner].
    pub fn slice(&self) -> &'a str {
        unsafe {
            std::str::from_utf8_unchecked(
                &self.src[self.current_span.start as usize..self.current_span.end as usize],
            )
        }
    }

    /// Returns the next character in the scanner.
    #[inline]
    pub fn next_char(&mut self) -> Option<char> {
        if self.current_span.end as usize >= self.src.len() {
            return None;
        }

        let (char, size) =
            char_from_slice(&self.src[self.current_span.end as usize..self.src.len()]);
        self.current_span.end += size as u32;

        Some(char)
    }

    /// Peeks at the next_character in the scanner.
    pub fn peek_char(&mut self) -> Option<char> {
        if self.current_span.end as usize >= self.src.len() {
            return None;
        }

        let (char, _) = char_from_slice(&self.src[self.current_span.end as usize..self.src.len()]);

        Some(char)
    }

    /// Returns `true` if the provided character can start an identifier.
    #[inline(always)]
    fn is_iden_start(c: char) -> bool {
        c == '_' || c.is_xid_start()
    }

    /// Returns `true` if the provided character can continue an identifier.  The same as
    /// `UnicodeXID::is_xid_continue`, made for symmetry with `is_iden_start`.
    #[inline(always)]
    fn is_iden_continue(c: char) -> bool {
        c.is_xid_continue()
    }

    /// Resets the span for the next token.
    #[inline(always)]
    fn next_span(&mut self) {
        self.current_span.start = self.current_span.end;
    }

    /// Skips any leading skippable tokens.
    #[inline(always)]
    fn skip(&mut self) {
        // TODO: skip comments

        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.next_char();
            } else if c == '/' {
                let reset_span = self.current_span;
                self.next_char();

                if let Some('/') = self.peek_char() {
                    while let Some(c) = self.next_char() {
                        if c == '\n' {
                            break;
                        }
                    }
                } else {
                    self.current_span = reset_span;
                    break;
                }
            } else {
                break;
            }
        }

        self.next_span();
    }

    /// Scans an identifier.
    #[inline(always)]
    fn scan_identifier(&mut self) {
        while let Some(c) = self.peek_char() {
            if !Self::is_iden_continue(c) {
                break;
            }

            self.next_char();
        }
    }

    /// Scans a string.
    #[inline(always)]
    fn scan_string(&mut self) -> Result<(), Error> {
        let mut ended = false;

        while let Some(c) = self.next_char() {
            if c == '"' {
                ended = true;
                break;
            } else if c == '\\' {
                self.next_char()
                    .ok_or(Error::UnterminatedString(self.current_span))?;
            }
        }

        if ended == false {
            return Err(Error::UnterminatedString(self.current_span));
        }

        Ok(())
    }

    /// Scans the exponent of a floating point number.
    fn scan_exponent(&mut self) -> Result<(), Error> {
        if let Some(c) = self.peek_char() {
            if c == '+' || c == '-' {
                self.next_char();
            } else if !c.is_digit(10) {
                let start = self.current_span.end;

                if Self::is_iden_start(c) {
                    self.scan_identifier();
                }

                return Err(Error::InvalidExponent(Span::new(
                    self.current_span.file_id,
                    start,
                    self.current_span.end,
                )));
            }
        } else {
            return Err(Error::InvalidExponent(Span::new(
                self.current_span.file_id,
                self.current_span.end,
                self.current_span.end + 1,
            )));
        }

        self.scan_digits_radix(10, false)?;

        Ok(())
    }

    /// Scans the digits for a number.
    fn scan_digits_radix(&mut self, radix: u32, exponent_possible: bool) -> Result<Token, Error> {
        while let Some(char) = self.peek_char() {
            if (char == 'e' || char == 'E') && exponent_possible {
                self.next_char();
                self.scan_exponent()?;
                return Ok(Token::Float);
            } else if !char.is_digit(radix) && char != '_' {
                if Self::is_iden_start(char) {
                    let start = self.current_span.end;
                    self.scan_identifier();

                    return Err(Error::NumberPrefix(Span::new(
                        self.current_span.file_id,
                        start,
                        self.current_span.end,
                    )));
                } else if char.is_digit(10) {
                    let start = self.current_span.end;
                    let _ = self.scan_digits_radix(10, false); // we don't care if this fails

                    return Err(Error::InvalidDigits {
                        number: Span::new(
                            self.current_span.file_id,
                            self.current_span.start,
                            start,
                        ),
                        offending: Span::new(
                            self.current_span.file_id,
                            start,
                            self.current_span.end,
                        ),
                        radix: match radix {
                            2 => NumberRadix::Binary,
                            8 => NumberRadix::Octal,
                            10 => NumberRadix::Decimal,
                            16 => NumberRadix::Hexadecimal,
                            _ => unreachable!(),
                        },
                    });
                };

                break;
            }

            self.next_char();
        }

        return match radix {
            2 => Ok(Token::Binary),
            8 => Ok(Token::Octal),
            10 => Ok(Token::Decimal),
            16 => Ok(Token::Hex),
            _ => unreachable!(),
        };
    }

    /// Scans a number.
    #[inline(always)]
    fn scan_number(&mut self) -> Result<Token, Error> {
        // we've confirmed that there is a digit in the `next` method
        let first_digit = self.next_char().unwrap();

        if first_digit == '0' {
            if let Some(c) = self.peek_char() {
                if c == 'x' || c == 'X' {
                    self.next_char();
                    return self.scan_digits_radix(16, false);
                } else if c == 'b' || c == 'B' {
                    self.next_char();
                    return self.scan_digits_radix(2, false);
                } else if c == 'o' || c == 'O' {
                    self.next_char();
                    return self.scan_digits_radix(8, false);
                }
            }
        }

        let ty = self.scan_digits_radix(10, true)?;

        if let Some(next) = self.peek_char() {
            if next == '.' && ty != Token::Float {
                self.next_char();

                if let Some(next) = self.peek_char() {
                    if next.is_digit(10) {
                        self.scan_digits_radix(10, true)?;
                    }
                }

                return Ok(Token::Float);
            }
        }

        Ok(ty)
    }

    /// Peeks the next token in the scanner.
    pub fn peek(&mut self) -> Option<Result<Token, Error>> {
        let span = self.current_span;
        let token = self.next();
        self.current_span = span;
        token
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip();
        self.next_span();

        let first_char = self.peek_char()?;

        if Self::is_iden_start(first_char) {
            self.scan_identifier();

            return Some(Ok(match self.slice() {
                "func" => Token::KFunc,
                "var" => Token::KVar,
                "struct" => Token::KStruct,
                "const" => Token::KConst,
                "mut" => Token::KMut,
                "return" => Token::KReturn,
                "while" => Token::KWhile,
                "true" => Token::KTrue,
                "false" => Token::KFalse,
                _ => Token::Identifier,
            }));
        } else if first_char == '"' {
            self.next_char();
            return match self.scan_string() {
                Ok(()) => Some(Ok(Token::String)),
                Err(e) => Some(Err(e)),
            };
        } else if first_char.is_digit(10) {
            return Some(self.scan_number());
        }

        self.next_char();

        Some(Ok(match first_char {
            ':' => Token::Colon,
            ',' => Token::Comma,
            '=' => {
                if self.peek_char() == Some('=') {
                    self.next_char();
                    Token::EqEq
                } else {
                    Token::Eq
                }
            }
            '!' => {
                if self.peek_char() == Some('=') {
                    self.next_char();
                    Token::BangEq
                } else {
                    Token::Bang
                }
            }
            ';' => Token::Semi,
            '~' => Token::Tilde,
            '*' => Token::Star,
            '/' => Token::Slash,
            '%' => Token::Percent,
            '+' => Token::Plus,
            '-' => {
                if self.peek_char() == Some('>') {
                    self.next_char();
                    Token::Arrow
                } else {
                    Token::Minus
                }
            }
            '.' => {
                if self.peek_char() == Some('{') {
                    self.next_char();
                    Token::Constructor
                } else {
                    Token::Dot
                }
            }
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBrack,
            ']' => Token::RBrack,
            _ => return Some(Err(Error::InvalidToken(self.current_span))),
        }))
    }
}
