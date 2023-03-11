use codespan_reporting::diagnostic::{Diagnostic, Severity};

use crate::span::Span;

/// The radix of a number, for diagnostics.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u32)]
pub enum NumberRadix {
    /// A binary number.
    Binary = 2,

    /// An octal number.
    Octal = 8,

    /// A decimal number.
    Decimal = 10,

    /// A hexadecimal number.
    Hexadecimal = 16,
}

impl NumberRadix {
    /// Gets the string name of the number radix.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Binary => "binary",
            Self::Octal => "octal",
            Self::Decimal => "decimal",
            Self::Hexadecimal => "hexadecimal",
        }
    }
}

/// An error that occurred during scanning.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum Error {
    /// An invalid character was found in the lexer.
    InvalidToken(Span),

    /// A string was started but never terminated with a closing quote.
    UnterminatedString(Span),

    /// An invalid character was found in a number literal.
    ///
    /// ```amp
    /// 0b0123_u32
    ///        ^^^ Prefix
    /// ```
    NumberPrefix(Span),

    /// An invalid digit was found for a number literal.
    ///
    /// ```amp
    /// 0b1234
    ///    ^^^ Invalid digits
    /// ```
    InvalidDigits {
        /// The number that the digits were found in.  Excludes the offending span.
        number: Span,

        /// The location of the offending digits.
        offending: Span,

        /// The radix of the number.
        radix: NumberRadix,
    },

    /// An exponent was found that started with an underscore.
    ///
    /// ```amp
    /// 42e_
    ///    ^
    /// ```
    InvalidExponent(Span),
}

impl Error {
    /// Returns the error code for this error.
    pub fn error_code(&self) -> u32 {
        match self {
            Self::InvalidToken(_) => 1,
            Self::UnterminatedString(_) => 2,
            Self::NumberPrefix(_) => 3,
            Self::InvalidDigits { .. } => 4,
            Self::InvalidExponent(_) => 5,
        }
    }

    /// Returns a [Diagnostic] for this error.
    pub fn as_diagnostic(&self) -> Diagnostic<usize> {
        let mut diagnostic =
            Diagnostic::new(Severity::Error).with_code(format!("E{:0>4}", self.error_code()));

        match self {
            Self::InvalidToken(span) => {
                diagnostic.message = "Invalid character".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::UnterminatedString(span) => {
                diagnostic.message = "String never closes".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::NumberPrefix(span) => {
                diagnostic.message = "Numbers cannot have prefixes".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidDigits {
                number,
                offending,
                radix,
            } => {
                diagnostic.message = "Invalid digits were found for this number type".to_owned();
                diagnostic.labels.push(offending.primary());
                diagnostic
                    .labels
                    .push(number.secondary().with_message(match radix {
                        NumberRadix::Octal => format!("This is an {} number", radix.as_str()),
                        _ => format!("This is a {} number", radix.as_str()),
                    }));
            }
            Self::InvalidExponent(span) => {
                diagnostic.message = "An exponent must start with a digit".to_owned();
                diagnostic.labels.push(span.primary());
            }
        }

        diagnostic
    }
}
