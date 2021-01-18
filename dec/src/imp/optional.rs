use dec_core::{
    error::{Error, PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Opt<P>(pub P);

impl<P, I, E, Fail> ParseOnce<I, E, Fail> for Opt<P>
where
    P: ParseOnce<I, E, Fail>,
    E: ParseError<I>,
{
    type Output = Option<P::Output>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        match self.0.parse_once(input) {
            Ok((input, output)) => Ok((input, Some(output))),
            Err(Error::Error(err)) => Ok((err.into_input(), None)),
            Err(err) => Err(err),
        }
    }
}

impl<P, I, E, Fail> ParseMut<I, E, Fail> for Opt<P>
where
    P: ParseMut<I, E, Fail>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { Opt(self.0.by_mut()).parse_once(input) }
}

impl<P, I, E, Fail> Parse<I, E, Fail> for Opt<P>
where
    P: Parse<I, E, Fail>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> { Opt(self.0.by_ref()).parse_once(input) }
}
