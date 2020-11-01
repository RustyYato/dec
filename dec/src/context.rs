use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Context<P>(pub P, pub &'static str);

impl<P: ParseOnce<I, E>, I: Clone, E: ParseError<I>> ParseOnce<I, E> for Context<P> {
    type Output = P::Output;

    fn parse_once(self, input: I) -> Result<I, Self::Output, E> {
        match self.0.parse_once(input.clone()) {
            Ok(ok) => Ok(ok),
            Err(Error::Error(err)) => Err(Error::Error(err.add_context(input, self.1))),
            Err(Error::Failure(err)) => Err(Error::Failure(err.add_context(input, self.1))),
        }
    }
}

impl<P: ParseMut<I, E>, I: Clone, E: ParseError<I>> ParseMut<I, E> for Context<P> {
    fn parse_mut(&mut self, input: I) -> Result<I, Self::Output, E> {
        Context(self.0.by_mut(), self.1).parse_once(input)
    }
}

impl<P: Parse<I, E>, I: Clone, E: ParseError<I>> Parse<I, E> for Context<P> {
    fn parse(&self, input: I) -> Result<I, Self::Output, E> {
        Context(self.0.by_ref(), self.1).parse_once(input)
    }
}
