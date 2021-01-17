use super::*;

impl<I, E, P, S, Fp, Fs, MkA, A, E2> ParseOnce<I, E> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    E: ParseError<I>,
    P: ParseMut<I, E>,
    S: ParseMut<I, E>,
    Fp: FnMut(A, P::Output) -> Result<A, E2>,
    Fs: FnMut(A, S::Output) -> Result<A, E2>,
    MkA: FnOnce() -> A,
{
    type Output = Result<A, E2>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
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

impl<I, E, P, S, Fp, Fs, MkA, A, E2> ParseMut<I, E> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    E: ParseError<I>,
    P: ParseMut<I, E>,
    S: ParseMut<I, E>,
    Fp: FnMut(A, P::Output) -> Result<A, E2>,
    Fs: FnMut(A, S::Output) -> Result<A, E2>,
    MkA: FnMut() -> A,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
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

impl<I, E, P, S, Fp, Fs, MkA, A, E2> Parse<I, E> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    E: ParseError<I>,
    P: Parse<I, E>,
    S: Parse<I, E>,
    Fp: Fn(A, P::Output) -> Result<A, E2>,
    Fs: Fn(A, S::Output) -> Result<A, E2>,
    MkA: Fn() -> A,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
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
