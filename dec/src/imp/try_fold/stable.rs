use super::*;

impl<P, A, MkA, F, I, E, E2> ParseOnce<I, E> for TryFold<P, MkA, F>
where
    MkA: FnOnce() -> A,
    P: ParseMut<I, E>,
    F: FnMut(A, P::Output) -> Result<A, E2>,
    I: Clone,
    E: ParseError<I>,
{
    type Output = Result<A, E2>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self { parser, mk_acc, func } = self;
        try_fold_parse_once(parser, mk_acc(), func, input)
    }
}

impl<P, A, MkA, F, I, E, E2> ParseMut<I, E> for TryFold<P, MkA, F>
where
    MkA: FnMut() -> A,
    P: ParseMut<I, E>,
    F: FnMut(A, P::Output) -> Result<A, E2>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self { parser, mk_acc, func } = self;
        try_fold_parse_once(parser.by_mut(), mk_acc(), func, input)
    }
}

impl<P, A, MkA, F, I, E, E2> Parse<I, E> for TryFold<P, MkA, F>
where
    MkA: Fn() -> A,
    P: Parse<I, E>,
    F: Fn(A, P::Output) -> Result<A, E2>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self { parser, mk_acc, func } = self;
        try_fold_parse_once(parser.by_ref(), mk_acc(), func, input)
    }
}
