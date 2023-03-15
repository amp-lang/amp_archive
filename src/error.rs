use codespan_reporting::diagnostic::{Diagnostic, Severity};

use crate::{
    ast,
    span::{Span, Spanned},
};

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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
#[repr(u32)]
pub enum Error {
    /// An invalid character was found in the lexer.
    InvalidToken(Span) = 1,

    /// A string was started but never terminated with a closing quote.
    UnterminatedString(Span) = 2,

    /// An invalid character was found in a number literal.
    ///
    /// ```amp
    /// 0b0123_u32
    ///        ^^^ Prefix
    /// ```
    NumberPrefix(Span) = 3,

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
    } = 4,

    /// An exponent was found that started with an underscore.
    ///
    /// ```amp
    /// 42e_
    ///    ^
    /// ```
    InvalidExponent(Span) = 5,

    /// Expected the name of a function.
    ExpectedFunctionName {
        /// The location of the function keyword
        func_keyword: Span,

        /// The offending location.
        offending: Span,
    } = 6,

    /// An argument wasn't closed before the end of the file.
    UnclosedArgumentList {
        /// The location of the opening parenthesis.
        opening_paren: Span,

        /// The span of the offending token.
        offending: Span,
    } = 7,

    /// Expected the argument list to close.
    ExpectedArgumentListClose {
        /// The location of the opening parenthesis.
        opening_paren: Span,

        /// The span of the offending token.
        offending: Span,
    } = 8,

    /// The parser expected an argument list.
    ExpectedArgumentList {
        /// The location of the function keyword.
        func_keyword: Span,

        /// The span of the end of the file.
        offending: Span,
    } = 9,

    /// A pointer type (`~const T` or `~mut T`) was missing the `const` or `mut` declaration.
    ExpectedPointerMutability {
        /// The location of the `~` in the pointer expression.
        tilde: Span,

        /// The span of the offending token.
        offending: Span,
    } = 10,

    /// The type of a pointer was missing.
    ExpectedPointerType {
        /// The location of the `~` in the pointer expression.
        tilde: Span,

        /// The span of the offending token.
        offending: Span,
    } = 11,

    MissingArgumentTypeAnnotation {
        /// The location of the argument name.
        name: Span,

        /// The span of the offending token.
        offending: Span,
    } = 12,

    ExpectedReturnType {
        /// The location of the `->` arrow operator.
        arrow: Span,

        /// The span of the offending token.
        offending: Span,
    } = 13,

    /// Expected a function definition (a code block) or a semicolon.
    ExpectedFunctionDefinition {
        /// The location of the function keyword.
        func_keyword: Span,

        /// The span of the offending token.
        offending: Span,
    } = 14,

    /// A semicolon was expected after an expression.
    ExpectedSemicolon(Span) = 15,

    /// A closing brace was expected.
    ExpectedClosingBrace {
        /// The location where the brace starts.
        starts: Span,

        /// The span of the offending token.
        offending: Span,
    } = 16,

    /// A type name was found that was not known by the compiler.
    UnknownNamedType(ast::Iden) = 17,

    /// Duplicate symbol declaration.
    DuplicateSymbol {
        /// The originally declared symbol.
        original: Span,

        /// The name of the offending function.
        name: Spanned<String>,
    } = 18,

    /// Expected a valid statement.
    InvalidStatement(Span) = 19,

    /// A function name was undeclared.
    UndeclaredFunction(Spanned<String>) = 20,

    /// A function call had an invalid callee.
    InvalidFunctionName(Span) = 21,

    /// An invalid value was found.
    InvalidValue(Span) = 22,

    /// An invalid argument count was found.
    InvalidArgumentCount {
        /// The location of the original declaration.
        decl: Span,

        /// The visualized type of the function signature.
        decl_type: String,

        /// The offending location.
        offending: Span,
    } = 23,

    /// An invalid escape code was found.
    InvalidEscape(Span) = 24,

    /// Invalid unicode escape syntax was found.
    InvalidUnicodeEscape(Span) = 25,

    /// An unclosed unicode escape was found.
    UnclosedUnicodeEscape(Span) = 26,

    /// An integer literal was too large.
    IntegerTooLarge(Span) = 27,

    /// Expected an argument of the given type.
    ExpectedArgumentOfType {
        /// The original declaration.
        decl: Span,

        /// The rendered name of the type.
        name: String,

        /// The offending argument location.
        offending: Span,
    } = 28,

    /// Expected a return type of the given type.
    InvalidReturnValue {
        /// The original declaration.
        decl: Span,

        /// The name of the expected type.
        name: String,

        /// The offending area.
        offending: Span,
    } = 29,
}

impl Error {
    /// Returns the error code for this error.
    #[inline]
    pub fn error_code(&self) -> u32 {
        unsafe { *(self as *const Self as *const u32) }
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
                diagnostic.message = "Unterminated string".to_owned();
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
            Self::ExpectedFunctionName {
                func_keyword,
                offending,
            } => {
                diagnostic.message = "Expected a function name".to_owned();
                diagnostic.labels.push(
                    func_keyword
                        .secondary()
                        .with_message("Function keyword here"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a function name here"),
                );
            }
            Self::UnclosedArgumentList {
                opening_paren,
                offending,
            } => {
                diagnostic.message = "Unclosed argument list".to_owned();
                diagnostic.labels.push(
                    opening_paren
                        .secondary()
                        .with_message("Opening parenthesis here"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a closing parenthesis here"),
                );
            }
            Self::ExpectedArgumentListClose {
                opening_paren,
                offending,
            } => {
                diagnostic.message = "Expected argument list to close".to_owned();
                diagnostic.labels.push(
                    opening_paren
                        .secondary()
                        .with_message("Opening parenthesis here"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a closing parenthesis here"),
                );
            }
            Self::ExpectedArgumentList {
                func_keyword,
                offending,
            } => {
                diagnostic.message = "Expected an argument list".to_owned();
                diagnostic.labels.push(
                    func_keyword
                        .secondary()
                        .with_message("Function declaration started here"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected an argument list here"),
                );
            }
            Self::ExpectedPointerMutability { tilde, offending } => {
                diagnostic.message = "Expected pointer mutability".to_owned();
                diagnostic
                    .labels
                    .push(tilde.secondary().with_message("Pointer type started here"));

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected either `mut` or `const` here"),
                );
            }
            Self::ExpectedPointerType { tilde, offending } => {
                diagnostic.message = "Expected pointer type".to_owned();
                diagnostic
                    .labels
                    .push(tilde.secondary().with_message("Pointer type started here"));

                diagnostic
                    .labels
                    .push(offending.primary().with_message("Expected a type here"));
            }
            Self::MissingArgumentTypeAnnotation { name, offending } => {
                diagnostic.message = "Missing type annotation for function argument".to_owned();
                diagnostic
                    .labels
                    .push(name.secondary().with_message("Argument started here"));

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a type annotation here (ex: `arg: i32`)"),
                );
            }
            Self::ExpectedReturnType { arrow, offending } => {
                diagnostic.message = "Expected return type".to_owned();
                diagnostic.labels.push(
                    arrow
                        .secondary()
                        .with_message("`->` declares the type that the function returns"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a type here (ex: `-> i32`)"),
                );
            }
            Self::ExpectedFunctionDefinition {
                func_keyword,
                offending,
            } => {
                diagnostic.message = "Expected a function definition or `;`".to_owned();
                diagnostic.labels.push(
                    func_keyword
                        .secondary()
                        .with_message("Function declaration started here"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a function definition or `;` here"),
                );
            }
            Self::ExpectedSemicolon(span) => {
                diagnostic.message = "Expected a semicolon".to_owned();
                diagnostic.labels.push(span.primary().with_message("here"));
            }
            Self::ExpectedClosingBrace { starts, offending } => {
                diagnostic.message = "Expected a closing brace".to_owned();
                diagnostic.labels.push(
                    starts
                        .secondary()
                        .with_message("Opening brace started here"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a closing brace here"),
                );
            }
            Self::UnknownNamedType(name) => {
                diagnostic.message = format!("Unknown type '{}'", name.value);
                diagnostic.labels.push(
                    name.span
                        .primary()
                        .with_message("Type not declared in this scope"),
                );
            }
            Self::DuplicateSymbol { original, name } => {
                diagnostic.message = "Duplicate symbol".to_owned();
                diagnostic.labels.push(
                    original
                        .secondary()
                        .with_message("Previous declaration of this symbol"),
                );

                diagnostic.labels.push(
                    name.span
                        .primary()
                        .with_message("Duplicate declaration of this symbol"),
                );
            }
            Self::InvalidStatement(span) => {
                diagnostic.message = "Expected a valid statement".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::UndeclaredFunction(name) => {
                diagnostic.message = format!("Undeclared function '{}'", name.value);
                diagnostic.labels.push(name.span.primary());
            }
            Self::InvalidFunctionName(span) => {
                diagnostic.message = "Invalid function name".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidValue(span) => {
                diagnostic.message = "Invalid value".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidArgumentCount {
                decl,
                decl_type,
                offending,
            } => {
                diagnostic.message = "Invalid argument count".to_owned();
                diagnostic
                    .labels
                    .push(decl.secondary().with_message("Declared here"));
                diagnostic.labels.push(
                    decl.secondary()
                        .with_message(format!("Expected '{}'", decl_type)),
                );
                diagnostic.labels.push(offending.primary());
            }
            Self::InvalidEscape(span) => {
                diagnostic.message = "Invalid escape sequence".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidUnicodeEscape(span) => {
                diagnostic.message = "Invalid unicode escape sequence".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::UnclosedUnicodeEscape(span) => {
                diagnostic.message = "Unclosed unicode escape sequence".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::IntegerTooLarge(span) => {
                diagnostic.message = "Integer is too large".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedArgumentOfType {
                decl,
                name,
                offending,
            } => {
                diagnostic.message = format!("Expected argument of type '{}'", name);
                diagnostic
                    .labels
                    .push(decl.secondary().with_message("Declared here"));
                diagnostic.labels.push(offending.primary());
            }
            Self::InvalidReturnValue {
                decl,
                name,
                offending,
            } => {
                diagnostic.message = format!("Expected '{}'", name);
                diagnostic
                    .labels
                    .push(decl.secondary().with_message("Declared here"));
                diagnostic.labels.push(offending.primary());
            }
        }

        diagnostic
    }
}
