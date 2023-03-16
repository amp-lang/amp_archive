use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

/// An identifier expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Iden {
    pub span: Span,
    pub value: String,
}

impl Parse for Iden {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token == Token::Identifier {
                    parser.scanner_mut().next();
                    return Some(Ok(Self {
                        span: parser.scanner().span(),
                        value: parser.scanner().slice().to_string(),
                    }));
                }

                return None;
            }
            Err(err) => return Some(Err(err)),
        }
    }
}

/// A string literal.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Str {
    pub span: Span,
    pub value: String,
}

impl Parse for Str {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let raw = match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token == Token::String {
                    parser.scanner_mut().next();
                    parser.scanner().slice()
                } else {
                    return None;
                }
            }
            Err(err) => return Some(Err(err)),
        };

        let pos_offset = parser.scanner().span().start + 1;
        let src = &raw[1..raw.len() - 1].to_string(); // removes quotes
        let mut value = String::new(); // it will have atleast the same amount of bytes as src.

        let mut chars = src.chars().enumerate();
        while let Some((start_idx, next)) = chars.next() {
            if next == '\\' {
                let (_, next) = chars
                    .next()
                    .expect("Scanner guarantees there's another character");

                match next {
                    'n' => value.push('\n'),
                    'r' => value.push('\r'),
                    't' => value.push('\t'),
                    '\\' => value.push('\\'),
                    '0' => value.push('\0'),
                    '"' => value.push('"'),
                    'x' => {
                        let slice_start = if let Some((idx, next)) = chars.next() {
                            if next != '{' {
                                return Some(Err(Error::InvalidUnicodeEscape(Span::new(
                                    parser.scanner().file_id(),
                                    pos_offset + start_idx as u32,
                                    pos_offset + start_idx as u32 + 3,
                                ))));
                            }

                            idx + 1
                        } else {
                            return Some(Err(Error::InvalidUnicodeEscape(Span::new(
                                parser.scanner().file_id(),
                                pos_offset + start_idx as u32,
                                pos_offset + start_idx as u32 + 2,
                            ))));
                        };

                        let mut ended = false;
                        while let Some((idx, next)) = chars.next() {
                            if next == '}' {
                                let slice = &src[slice_start..idx];

                                let codepoint = match u32::from_str_radix(slice, 16) {
                                    Ok(codepoint) => codepoint,
                                    Err(_) => {
                                        return Some(Err(Error::InvalidUnicodeEscape(Span::new(
                                            parser.scanner().file_id(),
                                            pos_offset + start_idx as u32,
                                            pos_offset + idx as u32 + 1,
                                        ))))
                                    }
                                };

                                value.push(match std::char::from_u32(codepoint) {
                                    Some(codepoint) => codepoint,
                                    None => {
                                        return Some(Err(Error::InvalidUnicodeEscape(Span::new(
                                            parser.scanner().file_id(),
                                            pos_offset + start_idx as u32,
                                            pos_offset + idx as u32 + 1,
                                        ))))
                                    }
                                });

                                ended = true;

                                break;
                            }
                        }

                        if !ended {
                            return Some(Err(Error::UnclosedUnicodeEscape(Span::new(
                                parser.scanner().file_id(),
                                pos_offset + start_idx as u32,
                                pos_offset + src.len() as u32,
                            ))));
                        }
                    }
                    _ => {
                        return Some(Err(Error::InvalidEscape(Span::new(
                            parser.scanner().file_id(),
                            pos_offset + start_idx as u32,
                            pos_offset + start_idx as u32 + 2,
                        ))))
                    }
                }

                continue;
            }

            value.push(next);
        }

        Some(Ok(Self {
            span: parser.scanner().span(),
            value,
        }))
    }
}

/// An integer literal.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Int {
    pub span: Span,
    pub value: i64,
}

impl Parse for Int {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token == Token::Decimal {
                    parser.scanner_mut().next();
                    return Some(Ok(Self {
                        span: parser.scanner().span(),
                        value: match parser.scanner().slice().parse() {
                            Ok(value) => value,
                            Err(_) => {
                                return Some(Err(Error::IntegerTooLarge(parser.scanner().span())))
                            }
                        },
                    }));
                } else if token == Token::Hex {
                    parser.scanner_mut().next();

                    return Some(Ok(Self {
                        span: parser.scanner().span(),
                        value: match i64::from_str_radix(&parser.scanner().slice()[2..], 16) {
                            Ok(value) => value,
                            Err(_) => {
                                return Some(Err(Error::IntegerTooLarge(parser.scanner().span())))
                            }
                        },
                    }));
                } else if token == Token::Octal {
                    parser.scanner_mut().next();

                    return Some(Ok(Self {
                        span: parser.scanner().span(),
                        value: match i64::from_str_radix(&parser.scanner().slice()[2..], 8) {
                            Ok(value) => value,
                            Err(_) => {
                                return Some(Err(Error::IntegerTooLarge(parser.scanner().span())))
                            }
                        },
                    }));
                } else if token == Token::Binary {
                    parser.scanner_mut().next();

                    return Some(Ok(Self {
                        span: parser.scanner().span(),
                        value: match i64::from_str_radix(&parser.scanner().slice()[2..], 2) {
                            Ok(value) => value,
                            Err(_) => {
                                return Some(Err(Error::IntegerTooLarge(parser.scanner().span())))
                            }
                        },
                    }));
                }

                return None;
            }
            Err(err) => return Some(Err(err)),
        }
    }
}
