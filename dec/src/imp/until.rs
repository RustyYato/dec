use dec_core::{
    error::{Error, PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

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

impl<P, Q, MkA, A, F, I, E, Fail> ParseOnce<I, E, Fail> for FoldUntil<P, Q, MkA, F>
where
    MkA: FnOnce() -> A,
    P: ParseMut<I, E, Fail>,
    Q: ParseMut<I, E, Fail>,
    F: FnMut(A, P::Output) -> A,
    E: ParseError<I>,
{
    type Output = (A, Q::Output);

    fn parse_once(self, mut input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            mut parser,
            mut stop,
            mut func,
            mk_acc,
        } = self;

        let mut acc = mk_acc();

        loop {
            let i = match stop.parse_mut(input) {
                Err(Error::Error(err)) => err.into_input(),
                Err(err @ Error::Failure(_)) => return Err(err),
                Ok((input, stop)) => return Ok((input, (acc, stop))),
            };

            let (i, value) = parser.parse_mut(i)?;

            input = i;
            acc = func(acc, value);
        }
    }
}

impl<P, Q, F, MkA, A, I, E, Fail> ParseMut<I, E, Fail> for FoldUntil<P, Q, MkA, F>
where
    MkA: FnMut() -> A,
    P: ParseMut<I, E, Fail>,
    Q: ParseMut<I, E, Fail>,
    F: FnMut(A, P::Output) -> A,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
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

impl<P, Q, F, MkA, A, I, E, Fail> Parse<I, E, Fail> for FoldUntil<P, Q, MkA, F>
where
    MkA: Fn() -> A,
    P: Parse<I, E, Fail>,
    Q: Parse<I, E, Fail>,
    F: Fn(A, P::Output) -> A,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
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

impl<P, Q, F, C, I, E, Fail> ParseOnce<I, E, Fail> for Until<P, Q, F>
where
    P: ParseMut<I, E, Fail>,
    Q: ParseMut<I, E, Fail>,
    F: FnOnce() -> C,
    C: Extend<P::Output>,
    I: Clone,
    E: ParseError<I>,
{
    type Output = (C, Q::Output);

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        FoldUntil {
            mk_acc: self.collection,
            func: crate::utils::extend,
            parser: self.parser,
            stop: self.stop,
        }
        .parse_once(input)
    }
}

impl<P, Q, F, C, I, E, Fail> ParseMut<I, E, Fail> for Until<P, Q, F>
where
    P: ParseMut<I, E, Fail>,
    Q: ParseMut<I, E, Fail>,
    F: FnMut() -> C,
    C: Extend<P::Output>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Until {
            parser: self.parser.by_mut(),
            stop: self.stop.by_mut(),
            collection: &mut self.collection,
        }
        .parse_once(input)
    }
}

impl<P, Q, F, C, I, E, Fail> Parse<I, E, Fail> for Until<P, Q, F>
where
    P: Parse<I, E, Fail>,
    Q: Parse<I, E, Fail>,
    F: Fn() -> C,
    C: Extend<P::Output>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Until {
            parser: self.parser.by_ref(),
            stop: self.stop.by_ref(),
            collection: &self.collection,
        }
        .parse_once(input)
    }
}
