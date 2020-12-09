use crate::{error::*, prelude::*};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Not<P>(pub P);

impl<P, I, E> ParseOnce<I, E> for Not<P>
where
    P: ParseOnce<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    type Output = ();

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        match self.0.parse_once(input.clone()) {
            Ok(_) => Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Not))),
            Err(Error::Error(_)) => Ok((input, ())),
            Err(err) => Err(err),
        }
    }
}

impl<P, I, E> ParseMut<I, E> for Not<P>
where
    P: ParseMut<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> { Not(self.0.by_mut()).parse_once(input) }
}

impl<P, I, E> Parse<I, E> for Not<P>
where
    P: Parse<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> { Not(self.0.by_ref()).parse_once(input) }
}
