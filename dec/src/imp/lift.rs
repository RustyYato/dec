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

impl<P, I, E, Fail> ParseOnce<I, E, Fail> for Lower<P>
where
    P: ParseOnce<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
{
    type Output = Result<P::Output, E>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        match self.0.parse_once(input.clone()) {
            Ok((input, output)) => Ok((input, Ok(output))),
            Err(Error::Error(err)) => Ok((input, Err(err))),
            Err(err) => Err(err),
        }
    }
}

impl<P, I, E, Fail> ParseMut<I, E, Fail> for Lower<P>
where
    P: ParseMut<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { Lower(self.0.by_mut()).parse_once(input) }
}

impl<P, I, E, Fail> Parse<I, E, Fail> for Lower<P>
where
    P: Parse<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> { Lower(self.0.by_ref()).parse_once(input) }
}

impl<P, I, E, O, Fail> ParseOnce<I, E, Fail> for Lift<P>
where
    P: ParseOnce<I, E, Fail, Output = Result<O, E>>,
    E: ParseError<I>,
{
    type Output = O;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse_once(input)?;
        match output {
            Ok(output) => Ok((input, output)),
            Err(err) => Err(Error::Error(err)),
        }
    }
}

impl<P, I, E, O, Fail> ParseMut<I, E, Fail> for Lift<P>
where
    P: ParseMut<I, E, Fail, Output = Result<O, E>>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { Lift(self.0.by_mut()).parse_once(input) }
}

impl<P, I, E, O, Fail> Parse<I, E, Fail> for Lift<P>
where
    P: Parse<I, E, Fail, Output = Result<O, E>>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> { Lift(self.0.by_ref()).parse_once(input) }
}
