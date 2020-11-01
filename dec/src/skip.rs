use crate::prelude::*;
use crate::{error::*, traits::InputSplit};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Skip(pub usize);

impl<I: InputSplit, E: ParseError<I>> ParseOnce<I, E> for Skip {
    type Output = ();

    fn parse_once(self, input: I) -> Result<I, Self::Output, E> {
        self.parse(input)
    }
}

impl<I: InputSplit, E: ParseError<I>> ParseMut<I, E> for Skip {
    fn parse_mut(&mut self, input: I) -> Result<I, Self::Output, E> {
        self.parse(input)
    }
}

impl<I: InputSplit, E: ParseError<I>> Parse<I, E> for Skip {
    fn parse(&self, input: I) -> Result<I, Self::Output, E> {
        match input.advance(self.0) {
            Ok(input) => Ok((input, ())),
            Err(input) => Err(Error::Error(ParseError::from_input_kind(
                input,
                ErrorKind::Skip,
            ))),
        }
    }
}
