use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FoldUntil<P, Q, A, F> {
    pub parser: P,
    pub stop: Q,
    pub func: F,
    pub mk_acc: A,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Until<P, Q, C> {
    pub parser: P,
    pub stop: Q,
    pub collection: C,
}

impl<P, Q, MkA, A, F, I, E> ParseOnce<I, E> for FoldUntil<P, Q, MkA, F>
where
    MkA: FnOnce() -> A,
    P: ParseMut<I, E>,
    Q: ParseMut<I, E>,
    F: FnMut(A, P::Output) -> A,
    I: Clone,
    E: ParseError<I>,
{
    type Output = (A, Q::Output);

    fn parse_once(self, mut input: I) -> PResult<I, Self::Output, E> {
        let Self {
            mut parser,
            mut stop,
            mut func,
            mk_acc,
        } = self;

        let mut acc = mk_acc();

        loop {
            match stop.parse_mut(input.clone()) {
                Err(Error::Error(_)) => (),
                Err(err @ Error::Failure(_)) => return Err(err),
                Ok((input, stop)) => return Ok((input, (acc, stop))),
            }

            let (i, value) = parser.parse_mut(input)?;

            input = i;
            acc = func(acc, value);
        }
    }
}

impl<P, Q, F, MkA, A, I, E> ParseMut<I, E> for FoldUntil<P, Q, MkA, F>
where
    MkA: FnMut() -> A,
    P: ParseMut<I, E>,
    Q: ParseMut<I, E>,
    F: FnMut(A, P::Output) -> A,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            stop,
            func,
            mk_acc,
        } = self;

        FoldUntil {
            parser: parser.by_mut(),
            stop: stop.by_mut(),
            func,
            mk_acc,
        }
        .parse_once(input)
    }
}

impl<P, Q, F, MkA, A, I, E> Parse<I, E> for FoldUntil<P, Q, MkA, F>
where
    MkA: Fn() -> A,
    P: Parse<I, E>,
    Q: Parse<I, E>,
    F: Fn(A, P::Output) -> A,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            stop,
            func,
            mk_acc,
        } = self;

        FoldUntil {
            parser: parser.by_ref(),
            stop: stop.by_ref(),
            func,
            mk_acc,
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
            mk_acc: self.collection,
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
