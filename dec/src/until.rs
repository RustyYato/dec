use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Until<P, Q, C> {
    pub parser: P,
    pub stop: Q,
    pub collection: C,
}

impl<
        P: ParseMut<I, E>,
        Q: ParseMut<I, E>,
        F: FnOnce() -> C,
        C: Extend<P::Output>,
        I: Clone,
        E: ParseError<I>,
    > ParseOnce<I, E> for Until<P, Q, F>
{
    type Output = (C, Q::Output);

    fn parse_once(mut self, mut input: I) -> PResult<I, Self::Output, E> {
        let mut collection = (self.collection)();

        loop {
            if let Ok((input, stop)) = self.stop.parse_mut(input.clone()) {
                return Ok((input, (collection, stop)));
            }

            let (i, value) = self.parser.parse_mut(input)?;

            input = i;
            collection.extend(Some(value));
        }
    }
}

impl<
        P: ParseMut<I, E>,
        Q: ParseMut<I, E>,
        F: FnMut() -> C,
        C: Extend<P::Output>,
        I: Clone,
        E: ParseError<I>,
    > ParseMut<I, E> for Until<P, Q, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Until {
            parser: self.parser.by_mut(),
            stop: self.stop.by_mut(),
            collection: &mut self.collection,
        }
        .parse_once(input)
    }
}

impl<
        P: Parse<I, E>,
        Q: Parse<I, E>,
        F: Fn() -> C,
        C: Extend<P::Output>,
        I: Clone,
        E: ParseError<I>,
    > Parse<I, E> for Until<P, Q, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Until {
            parser: self.parser.by_ref(),
            stop: self.stop.by_ref(),
            collection: &self.collection,
        }
        .parse_once(input)
    }
}
