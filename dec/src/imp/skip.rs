use dec_core::{
    error::{Error, ErrorKind, PResult, ParseError},
    InputSplit, Parse, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Skip(pub usize);

impl<I, E, Fail> ParseOnce<I, E, Fail> for Skip
where
    I: InputSplit,
    E: ParseError<I>,
{
    type Output = ();

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> { self.parse(input) }
}

impl<I, E, Fail> ParseMut<I, E, Fail> for Skip
where
    I: InputSplit,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { self.parse(input) }
}

impl<I, E, Fail> Parse<I, E, Fail> for Skip
where
    I: InputSplit,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        match input.advance(self.0) {
            Ok(input) => Ok((input, ())),
            Err(input) => Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Skip))),
        }
    }
}
