use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Opt<P>(pub P);

impl<P: ParseOnce<I, E>, I: Clone, E: ParseError<I>> ParseOnce<I, E> for Opt<P> {
    type Output = Option<P::Output>;

    fn parse_once(self, input: I) -> Result<I, Self::Output, E> {
        match self.0.parse_once(input.clone()) {
            Ok((input, output)) => Ok((input, Some(output))),
            Err(Error::Error(_)) => Ok((input, None)),
            Err(err) => Err(err),
        }
    }
}

impl<P: ParseMut<I, E>, I: Clone, E: ParseError<I>> ParseMut<I, E> for Opt<P> {
    fn parse_mut(&mut self, input: I) -> Result<I, Self::Output, E> {
        Opt(self.0.by_mut()).parse_once(input)
    }
}

impl<P: Parse<I, E>, I: Clone, E: ParseError<I>> Parse<I, E> for Opt<P> {
    fn parse(&self, input: I) -> Result<I, Self::Output, E> {
        Opt(self.0.by_ref()).parse_once(input)
    }
}
