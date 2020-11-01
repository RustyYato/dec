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
        let recognized = old_input.cut(len - input.len());
        Ok((input, (recognized, output)))
    }
}
