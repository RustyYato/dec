use dec_core::{
    error::{Error, ErrorKind, PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Verify<P, F>(pub P, pub F);

impl<F, P, I, E, Fail> ParseOnce<I, E, Fail> for Verify<P, F>
where
    P: ParseOnce<I, E, Fail>,
    F: FnOnce(&P::Output) -> bool,
    I: Clone,
    E: ParseError<I>,
{
    type Output = P::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let old_input = input.clone();
        let (input, output) = self.0.parse_once(input)?;

        if (self.1)(&output) {
            Ok((input, output))
        } else {
            Err(Error::Error(ParseError::from_input_kind(old_input, ErrorKind::Verify)))
        }
    }
}

impl<F, P, I, E, Fail> ParseMut<I, E, Fail> for Verify<P, F>
where
    P: ParseMut<I, E, Fail>,
    F: FnMut(&P::Output) -> bool,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Verify(self.0.by_mut(), &mut self.1).parse_once(input)
    }
}

impl<F, P, I, E, Fail> Parse<I, E, Fail> for Verify<P, F>
where
    P: Parse<I, E, Fail>,
    F: Fn(&P::Output) -> bool,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Verify(self.0.by_ref(), &self.1).parse_once(input)
    }
}
