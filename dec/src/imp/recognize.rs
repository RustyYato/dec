use dec_core::{
    error::{PResult, ParseError},
    InputSplit, Parse, ParseExt, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Recognize<P>(pub P);

impl<P, I, E> ParseOnce<I, E> for Recognize<P>
where
    P: ParseOnce<I, E>,
    I: Clone + InputSplit,
    E: ParseError<I>,
{
    type Output = (I, P::Output);

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let old_input = input.clone();
        let len = input.len();
        let (input, output) = self.0.parse_once(input)?;
        let len = len - input.len();
        let recognized = old_input.cut(len);
        Ok((input, (recognized, output)))
    }
}

impl<P, I, E> ParseMut<I, E> for Recognize<P>
where
    P: ParseMut<I, E>,
    I: Clone + InputSplit,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> { Recognize(self.0.by_mut()).parse_once(input) }
}

impl<P, I, E> Parse<I, E> for Recognize<P>
where
    P: Parse<I, E>,
    I: Clone + InputSplit,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E>
    where
        P: Parse<I, E>,
        I: Clone + InputSplit,
        E: ParseError<I>,
    {
        Recognize(self.0.by_ref()).parse_once(input)
    }
}
