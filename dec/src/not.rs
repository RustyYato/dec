use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Not<P>(pub P);

impl<P: ParseOnce<I, E>, I: Clone, E: ParseError<I>> ParseOnce<I, E> for Not<P> {
    type Output = ();

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        match self.0.parse_once(input.clone()) {
            Ok(_) => Err(Error::Error(ParseError::from_input_kind(
                input,
                ErrorKind::Not,
            ))),
            Err(Error::Error(_)) => Ok((input, ())),
            Err(err) => Err(err),
        }
    }
}

impl<P: ParseMut<I, E>, I: Clone, E: ParseError<I>> ParseMut<I, E> for Not<P> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Not(self.0.by_mut()).parse_once(input)
    }
}

impl<P: Parse<I, E>, I: Clone, E: ParseError<I>> Parse<I, E> for Not<P> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Not(self.0.by_ref()).parse_once(input)
    }
}
