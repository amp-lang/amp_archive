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

    /// Expected the type of an array.
    ExpectedArrayType(Span) = 31,

    /// An array type was unclosed.
    UnclosedArrayType { started: Span, offending: Span } = 32,

    /// Expected the length of an array or a slice mutability.
    ExpectedSliceMutability { started: Span, offending: Span } = 33,

    /// Expected the name of a variable.
    ExpectedVariableName { started: Span, offending: Span } = 34,

    /// Expected a value for a variable.
    ExpectedVariableValue { started: Span, offending: Span } = 35,

    /// Expected the type for a variable.
    ExpectedVariableType { started: Span, offending: Span } = 36,

    /// The compiler could not infer the type of a variable.
    CannotInferVarType(Span) = 37,

    /// Could not find a variable with the given name.
    UndeclaredVariable(Spanned<String>) = 38,

    /// Expected an expression.
    ExpectedExpression(Span) = 39,

    /// Cannot assign to the provided destination.
    InvalidAssignment(Span) = 40,

    /// Cannot assign a value to a variable of a different type.
    CannotAssignType {
        /// The original declaration.
        decl: Span,

        /// The name of the expected type.
        expected: String,

        /// The offending area.
        offending: Span,
    } = 41,

    /// Function doesn't return a value, and cannot be used as a value.
    VoidAsValue(Span) = 42,

    /// Cannot dereference a non-pointer type.
    InvalidDeref(Span) = 43,

    /// Cannot change an immutable pointer/slice.
    CannotChangeImmutable(Span) = 44,

    /// Expected a colon after a declaration.
    ExpectedColon(Span) = 45,

    /// Expected the type for a struct field.
    ExpectedFieldType(Span) = 46,

    /// Expected the name for a struct.
    ExpectedStructName(Span) = 47,

    /// Expected the fields of a struct type.
    ExpectedStructFields(Span) = 48,

    /// A duplicate struct field was found.
    DuplicateField(Spanned<String>) = 49,

    /// Expected an equals expression.
    ExpectedEq(Span) = 50,

    /// Expected a field value.
    ExpectedFieldValue(Span) = 51,

    /// An invalid type path was found.
    InvalidTypePath(Span) = 52,

    /// An invalid struct field was found in a constructor.
    UnknownStructField(Span) = 53,

    /// Expected a field of the given type.
    ExpectedFieldOfType {
        /// The original declaration.
        decl: Span,

        /// The name of the expected type.
        name: String,

        /// The offending argument location.
        offending: Span,
    } = 54,

    /// A field in a struct constructor was defined twice.
    DuplicateFieldDefinition(Span) = 55,

    /// Attempt to access a non-struct type.
    AccessNonStruct(Span) = 56,

    /// Expected a field name in an access.
    ExpectedFieldName(Span) = 57,

    /// The types of an expression were invalid.
    InvalidExprTypes {
        left: Spanned<String>,
        right: Spanned<String>,
        offending: Span,
    } = 58,

    /// Attempted to perform math with a non-number type.
    NonNumberMath {
        ty: Spanned<String>,
        offending: Span,
    } = 59,

    /// Expected a block.
    ExpectedBlock(Span) = 30,

    /// A non-boolean value was used as a condition in a conditional block.
    InvalidCondition(Span) = 60,

    /// Cannot compare two values of different types for equality.
    InvalidEq(Span) = 61,

    /// Expected a module name to import.
    ExpectedModuleName(Span) = 62,

    /// Cannot resolve the path of an imported module.
    CannotResolveImport(ast::Str) = 63,

    /// Cannot read the imported module.
    CannotReadImport(ast::Str) = 64,

    /// Expected the name following the `namespace` keyword.
    ExpectedNamespaceName(Span) = 65,

    /// Namespaces cannot follow modifiers.
    NamespaceModifier(Span) = 66,

    /// Expected an item in a path.
    ExpectedPathItem(Span) = 67,

    /// An invalid path was found.
    InvalidPath(Span) = 68,

    /// A namespace was not found.
    UndeclaredNamespace(Spanned<String>) = 69,

    /// Expected a closing parenthesis.
    ExpectedClosingParen(Span) = 70,

    /// Expected the type for an `as` conversion.
    ExpectedAsType(Span) = 71,

    /// Could not convert between two types.
    InvalidConversion {
        from: String,
        to: String,
        offending: Span,
    } = 72,

    /// Expected a closing `]`.
    ExpectedClosingBrack { started: Span, offending: Span } = 73,

    /// Expected a `uint` for an array index.
    ExpectedUintIndex(Span) = 74,

    /// An immutable value was referenced as mutable.
    CannotReferenceAsMut(Span) = 75,

    /// Cannot index the provided type.
    CannotIndex { ty: String, offending: Span } = 76,

    /// Expected `as` after source name.
    ExpectedAs(Span) = 77,

    /// No arguments can follow `...`.
    ArgCannotFollowVariadic(Span) = 78,

    /// A variadic argument was found in an Amp function.
    NonExternVariadic(Span) = 79,

    /// Expected a declaration in the root of the module.
    ExpectedDeclaration(Span) = 80,

    /// Expected a declaration after one or more modifiers.
    ExpectedDeclarationAfterModifier { modifier: Span, offending: Span } = 81,

    /// A private type was exposed by a public function
    ExposedPrivateType {
        /// The name of the type.
        name: Spanned<String>,

        /// The offending area.
        offending: Span,
    } = 82,

    /// Expected a type alias name.
    ExpectedTypeAliasName(Span) = 83,

    /// Expected the value of a type alias.
    ExpectedTypeAliasValue(Span) = 84,

    /// Expected a struct type to construct.
    CannotConstructNonStruct(Span) = 85,

    /// Expected the length of an array.
    ExpectedArrayLength(Span) = 86,

    /// An unsized struct field was not the last member in the struct.
    BadUnsizedStructField(Span) = 87,

    /// An unsized value was owned.
    OwnedUnsizedType(Span) = 88,

    /// Cannot construct a struct with private fields.
    CannotConstructPrivateStruct(Span) = 89,

    /// An attempt was made to access a private field of a struct.
    CannotAccessPrivateField(Span) = 90,

    /// An invalid bit was found in a bitfield.
    InvalidBitNot(Span) = 91,

    /// Can only negate boolean types.
    InvalidLogNot(Span) = 92,

    /// Can only compare two boolean types.
    InvalidLogAnd(Span) = 93,

    /// Can only compare two boolean types.
    InvalidLogOr(Span) = 94,

    /// Can only negate number types.
    InvalidNeg(Span) = 95,

    /// Expected the arguments of a function type.
    ExpectedFunctionTypeArgs(Span) = 96,

    /// Expected the return type after the arrow `->`.
    ExpectedFunctionTypeRet(Span) = 97,

    /// Attempted to access an undeclared member of a namespace.
    UndeclaredNamespaceMember(Span) = 98,

    /// Expected a value, instead got the name of a namespace.
    ExpectedValueGotNamespace(Span) = 99,

    /// Attempted to call a non-function type
    CannotCallNonFunction(Span) = 100,

    /// Attempt to make a struct with infinite size.
    InfinitelySizedStruct {
        /// The span of the struct declaration.
        span: Span,

        /// The span of the field which makes the struct infinitely sized.
        offending_field: Span,
    } = 101,
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
            Self::ExpectedArrayType(span) => {
                diagnostic.message = "Expected type for array".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::UnclosedArrayType { started, offending } => {
                diagnostic.message = "Unclosed array type".to_owned();
                diagnostic
                    .labels
                    .push(started.secondary().with_message("Array type started here"));

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a closing bracket here"),
                );
            }
            Self::ExpectedSliceMutability { started, offending } => {
                diagnostic.message = "Expected either `mut` or `const`".to_owned();
                diagnostic
                    .labels
                    .push(started.secondary().with_message("Slice type started here"));

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected `mut`/`const` here"),
                );
            }
            Self::ExpectedVariableName { started, offending } => {
                diagnostic.message = "Expected a variable name".to_owned();
                diagnostic.labels.push(
                    started
                        .secondary()
                        .with_message("Variable declaration started here"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a variable name here"),
                );
            }
            Self::ExpectedVariableValue { started, offending } => {
                diagnostic.message = "Expected a variable value".to_owned();
                diagnostic.labels.push(
                    started
                        .secondary()
                        .with_message("Variable declaration started here"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a variable value here"),
                );
            }
            Self::ExpectedVariableType { started, offending } => {
                diagnostic.message = "Expected a variable type".to_owned();
                diagnostic.labels.push(
                    started
                        .secondary()
                        .with_message("Variable declaration started here"),
                );

                diagnostic.labels.push(
                    offending
                        .primary()
                        .with_message("Expected a variable type here"),
                );
            }
            Self::CannotInferVarType(span) => {
                diagnostic.message = "Cannot infer type of variable".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::UndeclaredVariable(name) => {
                diagnostic.message = format!("Undeclared variable '{}'", name.value);
                diagnostic.labels.push(name.span.primary());
            }
            Self::ExpectedExpression(span) => {
                diagnostic.message = "Expected an expression".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidAssignment(span) => {
                diagnostic.message = "Cannot assign to this value".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::CannotAssignType {
                decl,
                expected,
                offending,
            } => {
                diagnostic.message = format!("Expected '{}'", expected);
                diagnostic
                    .labels
                    .push(decl.secondary().with_message("Declared here"));
                diagnostic.labels.push(offending.primary());
            }
            Self::VoidAsValue(span) => {
                diagnostic.message = "Cannot use nothing as a value".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidDeref(span) => {
                diagnostic.message = "Cannot dereference a non-pointer type".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::CannotChangeImmutable(span) => {
                diagnostic.message = "Cannot change an immutable value".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedColon(span) => {
                diagnostic.message = "Expected a colon".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedFieldType(span) => {
                diagnostic.message = "Expected a type for the struct field".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedStructName(span) => {
                diagnostic.message = "Expected a name for the struct".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedStructFields(span) => {
                diagnostic.message = "Expected a field for the struct".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::DuplicateField(name) => {
                diagnostic.message = format!("Duplicate field '{}'", name.value);
                diagnostic.labels.push(name.span.primary());
            }
            Self::ExpectedEq(span) => {
                diagnostic.message = "Expected `=`".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedFieldValue(span) => {
                diagnostic.message = "Expected a value for the struct field".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidTypePath(span) => {
                diagnostic.message = "Invalid type path".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::UnknownStructField(span) => {
                diagnostic.message = "Unknown struct field".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedFieldOfType {
                decl,
                name,
                offending,
            } => {
                diagnostic.message = format!("Expected field of type '{}'", name);
                diagnostic
                    .labels
                    .push(decl.secondary().with_message("Declared here"));
                diagnostic.labels.push(offending.primary());
            }
            Self::DuplicateFieldDefinition(span) => {
                diagnostic.message = "Duplicate field definition".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::AccessNonStruct(span) => {
                diagnostic.message = "Cannot access a field from a non-struct value".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedFieldName(span) => {
                diagnostic.message = "Expected a field name to access".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidExprTypes {
                left,
                right,
                offending,
            } => {
                diagnostic.message = "Unmatching expression types".to_owned();
                diagnostic
                    .labels
                    .push(left.span.secondary().with_message(&left.value));
                diagnostic
                    .labels
                    .push(right.span.secondary().with_message(&right.value));
                diagnostic.labels.push(offending.primary());
            }
            Self::NonNumberMath { ty, offending } => {
                diagnostic.message = "Cannot perform math on a non-number type".to_owned();
                diagnostic
                    .labels
                    .push(ty.span.secondary().with_message(&ty.value));
                diagnostic.labels.push(offending.primary());
            }
            Self::ExpectedBlock(span) => {
                diagnostic.message = "Expected a block".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidCondition(span) => {
                diagnostic.message = "Expected a boolean condition".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidEq(span) => {
                diagnostic.message = "Cannot compare two values of different types".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedModuleName(span) => {
                diagnostic.message = "Expected a module name to import".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::CannotResolveImport(import) => {
                diagnostic.message = format!("Cannot resolve import '{}'", import.value);
                diagnostic.labels.push(import.span.primary());
            }
            Self::CannotReadImport(import) => {
                diagnostic.message =
                    format!("Cannot read contents of imported module '{}'", import.value);
                diagnostic.labels.push(import.span.primary());
            }
            Self::ExpectedNamespaceName(span) => {
                diagnostic.message = "Expected the name of the namespace".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::NamespaceModifier(span) => {
                diagnostic.message = "Cannot apply a modifier to a namespace".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedPathItem(span) => {
                diagnostic.message = "Expected a path item".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidPath(span) => {
                diagnostic.message = "Invalid path".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::UndeclaredNamespace(namespace) => {
                diagnostic.message = format!("Undeclared namespace '{}'", namespace.value);
                diagnostic.labels.push(namespace.span.primary());
            }
            Self::ExpectedClosingParen(span) => {
                diagnostic.message = "Expected a closing parenthesis".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedAsType(span) => {
                diagnostic.message = "Expected a type to cast to".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidConversion {
                from,
                to,
                offending,
            } => {
                diagnostic.message = format!("Cannot convert '{}' to '{}'", from, to);

                diagnostic.labels.push(offending.primary());
            }
            Self::ExpectedClosingBrack { started, offending } => {
                diagnostic.message = "Expected a closing bracket".to_owned();
                diagnostic
                    .labels
                    .push(started.secondary().with_message("Opened here"));
                diagnostic.labels.push(offending.primary());
            }
            Self::ExpectedUintIndex(span) => {
                diagnostic.message = "Expected an unsigned integer (`uint`) as index".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::CannotReferenceAsMut(span) => {
                diagnostic.message = "Cannot reference an immutable value as mutable".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::CannotIndex { ty, offending } => {
                diagnostic.message = "Cannot index a non-array type".to_owned();
                diagnostic.labels.push(offending.primary().with_message(ty));
            }
            Self::ExpectedAs(span) => {
                diagnostic.message = "Expected `as` after external name".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ArgCannotFollowVariadic(span) => {
                diagnostic.message =
                    "Cannot have an argument after a variadic declaration".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::NonExternVariadic(span) => {
                diagnostic.message = "Variadic functions must be external".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedDeclaration(span) => {
                diagnostic.message = "Expected a declaration".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedDeclarationAfterModifier {
                modifier,
                offending,
            } => {
                diagnostic.message = format!("Expected a declaration after modifier(s)");
                diagnostic.labels.push(modifier.secondary());
                diagnostic.labels.push(offending.primary());
            }
            Self::ExposedPrivateType { name, offending } => {
                diagnostic.message =
                    format!("Exported function exposes private type '{}'", name.value);
                diagnostic.labels.push(name.span.secondary());
                diagnostic.labels.push(offending.primary());
            }
            Self::ExpectedTypeAliasName(span) => {
                diagnostic.message = "Expected a name for the type alias".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedTypeAliasValue(span) => {
                diagnostic.message = "Expected a value for the type alias".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::CannotConstructNonStruct(span) => {
                diagnostic.message = "Cannot construct a non-struct type".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedArrayLength(span) => {
                diagnostic.message = "Expected an array length".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::BadUnsizedStructField(span) => {
                diagnostic.message =
                    "A struct may only have an unsized field at the end of the declaration"
                        .to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::OwnedUnsizedType(span) => {
                diagnostic.message = "Size not known at compile time".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::CannotConstructPrivateStruct(span) => {
                diagnostic.message = "Cannot construct a struct with private fields".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::CannotAccessPrivateField(span) => {
                diagnostic.message = "Cannot access a private field".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidBitNot(span) => {
                diagnostic.message = "Can only perform bitwise NOT on integers".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidLogNot(span) => {
                diagnostic.message = "Can only negate boolean types".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidLogAnd(span) => {
                diagnostic.message = "Can only perform logical AND on boolean types".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidLogOr(span) => {
                diagnostic.message = "Can only perform logical OR on boolean types".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InvalidNeg(span) => {
                diagnostic.message = "Can only negate signed number types".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedFunctionTypeArgs(span) => {
                diagnostic.message = "Expected a list of type arguments".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedFunctionTypeRet(span) => {
                diagnostic.message = "Expected a return type".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::UndeclaredNamespaceMember(span) => {
                diagnostic.message = "Undeclared namespace member".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::ExpectedValueGotNamespace(span) => {
                diagnostic.message = "Expected a value, got a namespace".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::CannotCallNonFunction(span) => {
                diagnostic.message = "Cannot call a non-function type".to_owned();
                diagnostic.labels.push(span.primary());
            }
            Self::InfinitelySizedStruct {
                span,
                offending_field,
            } => {
                diagnostic.message = "Struct is infinitely sized".to_owned();
                diagnostic.labels.push(span.primary());
                diagnostic.labels.push(
                    offending_field
                        .secondary()
                        .with_message("This field makes the struct infinitely sized"),
                );
            }
        }

        diagnostic
    }
}
