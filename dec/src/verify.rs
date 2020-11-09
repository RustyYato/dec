use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Verify<P, F>(pub P, pub F);

impl<P: ParseOnce<I, E>, F: FnOnce(&P::Output) -> bool, I: Clone, E: ParseError<I>> ParseOnce<I, E>
    for Verify<P, F>
{
    type Output = P::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let old_input = input.clone();
        let (input, output) = self.0.parse_once(input)?;

        if (self.1)(&output) {
            Ok((input, output))
        } else {
            Err(Error::Error(ParseError::from_input_kind(
                old_input,
                ErrorKind::Verify,
            )))
        }
    }
}

impl<P: ParseMut<I, E>, F: FnMut(&P::Output) -> bool, I: Clone, E: ParseError<I>> ParseMut<I, E>
    for Verify<P, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Verify(self.0.by_mut(), &mut self.1).parse_once(input)
    }
}

impl<P: Parse<I, E>, F: Fn(&P::Output) -> bool, I: Clone, E: ParseError<I>> Parse<I, E>
    for Verify<P, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Verify(self.0.by_ref(), &self.1).parse_once(input)
    }
}
