use crate::error::*;
use crate::traits::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Recognize<P>(pub P);

impl<P: ParseOnce<I, E>, I: Clone + InputSplit, E: ParseError<I>> ParseOnce<I, E> for Recognize<P> {
    type Output = (I, P::Output);

    fn parse_once(self, input: I) -> Result<I, Self::Output, E> {
        let old_input = input.clone();
        let len = input.len();
        let (input, output) = self.0.parse_once(input)?;
        let len = len - input.len();
        let recognized = old_input.cut(len);
        Ok((input, (recognized, output)))
    }
}

impl<P: ParseMut<I, E>, I: Clone + InputSplit, E: ParseError<I>> ParseMut<I, E> for Recognize<P> {
    fn parse_mut(&mut self, input: I) -> Result<I, Self::Output, E> {
        Recognize(self.0.by_mut()).parse_once(input)
    }
}

impl<P: Parse<I, E>, I: Clone + InputSplit, E: ParseError<I>> Parse<I, E> for Recognize<P> {
    fn parse(&self, input: I) -> Result<I, Self::Output, E> {
        Recognize(self.0.by_ref()).parse_once(input)
    }
}
