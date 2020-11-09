use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FoldUntil<P, Q, V, F> {
    pub parser: P,
    pub stop: Q,
    pub func: F,
    pub acc: V,
}

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
        F: FnMut(V, P::Output) -> V,
        V,
        I: Clone,
        E: ParseError<I>,
    > ParseOnce<I, E> for FoldUntil<P, Q, V, F>
{
    type Output = (V, Q::Output);

    fn parse_once(self, mut input: I) -> PResult<I, Self::Output, E> {
        let Self {
            mut parser,
            mut stop,
            mut func,
            mut acc,
        } = self;

        loop {
            match stop.parse_mut(input.clone()) {
                Ok((input, stop)) => return Ok((input, (acc, stop))),
                Err(Error::Error(_)) => (),
                Err(err @ Error::Failure(_)) => return Err(err),
            }

            let (i, value) = parser.parse_mut(input)?;

            input = i;
            acc = func(acc, value);
        }
    }
}

impl<
        P: ParseMut<I, E>,
        Q: ParseMut<I, E>,
        F: FnMut(V, P::Output) -> V,
        V: Clone,
        I: Clone,
        E: ParseError<I>,
    > ParseMut<I, E> for FoldUntil<P, Q, V, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            stop,
            func,
            acc,
        } = self;

        FoldUntil {
            parser: parser.by_mut(),
            stop: stop.by_mut(),
            func,
            acc: acc.clone(),
        }
        .parse_once(input)
    }
}

impl<
        P: Parse<I, E>,
        Q: Parse<I, E>,
        F: Fn(V, P::Output) -> V,
        V: Clone,
        I: Clone,
        E: ParseError<I>,
    > Parse<I, E> for FoldUntil<P, Q, V, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            stop,
            func,
            acc,
        } = self;

        FoldUntil {
            parser: parser.by_ref(),
            stop: stop.by_ref(),
            func,
            acc: acc.clone(),
        }
        .parse_once(input)
    }
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

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        FoldUntil {
            acc: (self.collection)(),
            func: crate::extend,
            parser: self.parser,
            stop: self.stop,
        }
        .parse_once(input)
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
