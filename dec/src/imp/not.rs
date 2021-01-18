use dec_core::{
    error::{Error, ErrorKind, PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Not<P>(pub P);

impl<P, I, E, Fail> ParseOnce<I, E, Fail> for Not<P>
where
    P: ParseOnce<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
{
    type Output = ();

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        match self.0.parse_once(input.clone()) {
            Ok(_) => Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Not))),
            Err(Error::Error(_)) => Ok((input, ())),
            Err(err) => Err(err),
        }
    }
}

impl<P, I, E, Fail> ParseMut<I, E, Fail> for Not<P>
where
    P: ParseMut<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { Not(self.0.by_mut()).parse_once(input) }
}

impl<P, I, E, Fail> Parse<I, E, Fail> for Not<P>
where
    P: Parse<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> { Not(self.0.by_ref()).parse_once(input) }
}
