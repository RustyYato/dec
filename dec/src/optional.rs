use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Opt<P>(pub P);

impl<P, I, E> ParseOnce<I, E> for Opt<P>
where
    P: ParseOnce<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    type Output = Option<P::Output>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        match self.0.parse_once(input.clone()) {
            Ok((input, output)) => Ok((input, Some(output))),
            Err(Error::Error(_)) => Ok((input, None)),
            Err(err) => Err(err),
        }
    }
}

impl<P, I, E> ParseMut<I, E> for Opt<P>
where
    P: ParseMut<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Opt(self.0.by_mut()).parse_once(input)
    }
}

impl<P, I, E> Parse<I, E> for Opt<P>
where
    P: Parse<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Opt(self.0.by_ref()).parse_once(input)
    }
}
