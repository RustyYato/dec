use super::*;

impl<P, A, MkA, F, I, E, E2, Fail> ParseOnce<I, E, Fail> for TryFold<P, MkA, F>
where
    MkA: FnOnce() -> A,
    P: ParseMut<I, E, Fail>,
    F: FnMut(A, P::Output) -> Result<A, E2>,
    E: ParseError<I>,
{
    type Output = Result<A, E2>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self { parser, mk_acc, func } = self;
        try_fold_parse_once(parser, mk_acc(), func, input)
    }
}

impl<P, A, MkA, F, I, E, E2, Fail> ParseMut<I, E, Fail> for TryFold<P, MkA, F>
where
    MkA: FnMut() -> A,
    P: ParseMut<I, E, Fail>,
    F: FnMut(A, P::Output) -> Result<A, E2>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self { parser, mk_acc, func } = self;
        try_fold_parse_once(parser.by_mut(), mk_acc(), func, input)
    }
}

impl<P, A, MkA, F, I, E, E2, Fail> Parse<I, E, Fail> for TryFold<P, MkA, F>
where
    MkA: Fn() -> A,
    P: Parse<I, E, Fail>,
    F: Fn(A, P::Output) -> Result<A, E2>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self { parser, mk_acc, func } = self;
        try_fold_parse_once(parser.by_ref(), mk_acc(), func, input)
    }
}
