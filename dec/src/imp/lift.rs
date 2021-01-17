use dec_core::{
    error::{Error, PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lower<P>(pub P);

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lift<P>(pub P);

impl<P, I, E> ParseOnce<I, E> for Lower<P>
where
    P: ParseOnce<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    type Output = Result<P::Output, E>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        match self.0.parse_once(input.clone()) {
            Ok((input, output)) => Ok((input, Ok(output))),
            Err(Error::Error(err)) => Ok((input, Err(err))),
            Err(err) => Err(err),
        }
    }
}

impl<P, I, E> ParseMut<I, E> for Lower<P>
where
    P: ParseMut<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> { Lower(self.0.by_mut()).parse_once(input) }
}

impl<P, I, E> Parse<I, E> for Lower<P>
where
    P: Parse<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> { Lower(self.0.by_ref()).parse_once(input) }
}

impl<P, I, E, O> ParseOnce<I, E> for Lift<P>
where
    P: ParseOnce<I, E, Output = Result<O, E>>,
    E: ParseError<I>,
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

impl<P, I, E, O> ParseMut<I, E> for Lift<P>
where
    P: ParseMut<I, E, Output = Result<O, E>>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> { Lift(self.0.by_mut()).parse_once(input) }
}

impl<P, I, E, O> Parse<I, E> for Lift<P>
where
    P: Parse<I, E, Output = Result<O, E>>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> { Lift(self.0.by_ref()).parse_once(input) }
}
