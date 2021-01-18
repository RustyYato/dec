use dec_core::{
    error::{PResult, ParseError},
    InputSplit, Parse, ParseExt, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Recognize<P>(pub P);

impl<P, I, E, Fail> ParseOnce<I, E, Fail> for Recognize<P>
where
    P: ParseOnce<I, E, Fail>,
    I: Clone + InputSplit,
    E: ParseError<I>,
{
    type Output = (I, P::Output);

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let old_input = input.clone();
        let len = input.len();
        let (input, output) = self.0.parse_once(input)?;
        let len = len - input.len();
        let recognized = old_input.cut(len);
        Ok((input, (recognized, output)))
    }
}

impl<P, I, E, Fail> ParseMut<I, E, Fail> for Recognize<P>
where
    P: ParseMut<I, E, Fail>,
    I: Clone + InputSplit,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { Recognize(self.0.by_mut()).parse_once(input) }
}

impl<P, I, E, Fail> Parse<I, E, Fail> for Recognize<P>
where
    P: Parse<I, E, Fail>,
    I: Clone + InputSplit,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail>
    where
        P: Parse<I, E, Fail>,
        I: Clone + InputSplit,
        E: ParseError<I>,
    {
        Recognize(self.0.by_ref()).parse_once(input)
    }
}
