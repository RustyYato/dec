use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lower<P>(pub P);

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lift<P>(pub P);

impl<P: ParseOnce<I, E>, I: Clone, E: ParseError<I>> ParseOnce<I, E> for Lower<P> {
    type Output = Result<P::Output, E>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        match self.0.parse_once(input.clone()) {
            Ok((input, output)) => Ok((input, Ok(output))),
            Err(Error::Error(err)) => Ok((input, Err(err))),
            Err(err) => Err(err),
        }
    }
}

impl<P: ParseMut<I, E>, I: Clone, E: ParseError<I>> ParseMut<I, E> for Lower<P> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Lower(self.0.by_mut()).parse_once(input)
    }
}

impl<P: Parse<I, E>, I: Clone, E: ParseError<I>> Parse<I, E> for Lower<P> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Lower(self.0.by_ref()).parse_once(input)
    }
}

impl<P: ParseOnce<I, E, Output = Result<O, E>>, O, I, E: ParseError<I>> ParseOnce<I, E>
    for Lift<P>
{
    type Output = O;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_once(input)?;
        match output {
            Ok(output) => Ok((input, output)),
            Err(err) => Err(Error::Error(err)),
        }
    }
}

impl<P: ParseMut<I, E, Output = Result<O, E>>, O, I, E: ParseError<I>> ParseMut<I, E> for Lift<P> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Lift(self.0.by_mut()).parse_once(input)
    }
}

impl<P: Parse<I, E, Output = Result<O, E>>, O, I, E: ParseError<I>> Parse<I, E> for Lift<P> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Lift(self.0.by_ref()).parse_once(input)
    }
}
