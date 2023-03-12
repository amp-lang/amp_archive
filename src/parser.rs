use crate::{
    error::Error,
    scanner::{Scanner, Token},
};

pub trait Parse {
    /// Parses this value from the parser.
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>>
    where
        Self: Sized;
}

impl<T: Parse> Parse for Vec<T> {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let mut vec = Vec::new();

        let mut comma = false; // the current state

        loop {
            if comma {
                comma = false;
                match parser.scanner_mut().peek() {
                    Some(res) => match res {
                        Ok(token) => {
                            if token != Token::Comma {
                                break;
                            }

                            parser.scanner_mut().next();
                        }
                        Err(err) => return Some(Err(err)),
                    },
                    None => break,
                }
            } else {
                comma = true;
                if let Some(value) = T::parse(parser) {
                    vec.push(match value {
                        Ok(value) => value,
                        Err(err) => return Some(Err(err)),
                    });
                } else {
                    break;
                }
            }
        }

        Some(Ok(vec))
    }
}

/// The state of the parser.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Parser<'a> {
    /// The lexer that the parser wraps.
    scanner: Scanner<'a>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser.
    pub fn new(scanner: Scanner<'a>) -> Self {
        Self { scanner }
    }

    /// Returns a reference to the scanner.
    #[inline]
    pub fn scanner(&self) -> &Scanner<'a> {
        &self.scanner
    }

    /// Returns a mutable reference to the scanner.
    #[inline]
    pub fn scanner_mut(&mut self) -> &mut Scanner<'a> {
        &mut self.scanner
    }

    /// Parses a value from the parser.
    #[inline]
    pub fn parse<T: Parse>(&mut self) -> Option<Result<T, Error>> {
        T::parse(self)
    }
}
