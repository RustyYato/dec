use super::*;

impl<I, E, P, S, Fp, Fs, MkA, A, E2, Fail> ParseOnce<I, E, Fail> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    P: ParseMut<I, E, Fail>,
    S: ParseMut<I, E, Fail>,
    Fp: FnMut(A, P::Output) -> Result<A, E2>,
    Fs: FnMut(A, S::Output) -> Result<A, E2>,
    MkA: FnOnce() -> A,
    E: ParseError<I>,
{
    type Output = Result<A, E2>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            item_func,
            sep_func,
            mk_acc,
        } = self;
        try_separated_fold_parse_once(input, item, sep, item_func, sep_func, mk_acc())
    }
}

impl<I, E, P, S, Fp, Fs, MkA, A, E2, Fail> ParseMut<I, E, Fail> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    P: ParseMut<I, E, Fail>,
    S: ParseMut<I, E, Fail>,
    Fp: FnMut(A, P::Output) -> Result<A, E2>,
    Fs: FnMut(A, S::Output) -> Result<A, E2>,
    MkA: FnMut() -> A,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            item_func,
            sep_func,
            mk_acc,
        } = self;
        try_separated_fold_parse_once(input, item.by_mut(), sep.by_mut(), item_func, sep_func, mk_acc())
    }
}

impl<I, E, P, S, Fp, Fs, MkA, A, E2, Fail> Parse<I, E, Fail> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    P: Parse<I, E, Fail>,
    S: Parse<I, E, Fail>,
    Fp: Fn(A, P::Output) -> Result<A, E2>,
    Fs: Fn(A, S::Output) -> Result<A, E2>,
    MkA: Fn() -> A,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            item_func,
            sep_func,
            mk_acc,
        } = self;
        try_separated_fold_parse_once(input, item.by_ref(), sep.by_ref(), item_func, sep_func, mk_acc())
    }
}
