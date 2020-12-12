use dec_core::{
    error::{Error, ErrorKind, PResult, ParseError},
    InputSplit, Parse, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Skip(pub usize);

impl<I, E> ParseOnce<I, E> for Skip
where
    I: InputSplit,
    E: ParseError<I>,
{
    type Output = ();

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> { self.parse(input) }
}

impl<I, E> ParseMut<I, E> for Skip
where
    I: InputSplit,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> { self.parse(input) }
}

impl<I, E> Parse<I, E> for Skip
where
    I: InputSplit,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        match input.advance(self.0) {
            Ok(input) => Ok((input, ())),
            Err(input) => Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Skip))),
        }
    }
}
