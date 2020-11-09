use super::*;

impl<
        P: ParseMut<I, E>,
        V,
        F: FnMut(V, P::Output) -> Result<V, E2>,
        I: Clone,
        E: ParseError<I>,
        E2,
    > ParseOnce<I, E> for TryFold<P, V, F>
{
    type Output = Result<V, E2>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            func,
        } = self;
        try_fold_parse_once(parser, value, func, input)
    }
}

impl<
        P: ParseMut<I, E>,
        V: Clone,
        F: FnMut(V, P::Output) -> Result<V, E2>,
        I: Clone,
        E: ParseError<I>,
        E2,
    > ParseMut<I, E> for TryFold<P, V, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            func,
        } = self;
        try_fold_parse_once(parser.by_mut(), value.clone(), func, input)
    }
}

impl<
        P: Parse<I, E>,
        V: Clone,
        F: Fn(V, P::Output) -> Result<V, E2>,
        I: Clone,
        E: ParseError<I>,
        E2,
    > Parse<I, E> for TryFold<P, V, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            func,
        } = self;
        try_fold_parse_once(parser.by_ref(), value.clone(), func, input)
    }
}
