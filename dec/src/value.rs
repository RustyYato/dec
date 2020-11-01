use crate::{error::*, prelude::*};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Value<T, P>(pub T, pub P);

impl<T, P: ParseOnce<I, E>, I, E: ParseError<I>> ParseOnce<I, E> for Value<T, P> {
    type Output = T;

    fn parse_once(self, input: I) -> Result<I, Self::Output, E> {
        let Value(value, parser) = self;
        let (input, _) = parser.parse_once(input)?;
        Ok((input, value))
    }
}

impl<T: Clone, P: ParseMut<I, E>, I, E: ParseError<I>> ParseMut<I, E> for Value<T, P> {
    fn parse_mut(&mut self, input: I) -> Result<I, Self::Output, E> {
        let Value(value, parser) = self;
        let (input, _) = parser.parse_mut(input)?;
        Ok((input, value.clone()))
    }
}

impl<T: Clone, P: Parse<I, E>, I, E: ParseError<I>> Parse<I, E> for Value<T, P> {
    fn parse(&self, input: I) -> Result<I, Self::Output, E> {
        let Value(value, parser) = self;
        let (input, _) = parser.parse(input)?;
        Ok((input, value.clone()))
    }
}
