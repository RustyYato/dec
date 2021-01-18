use super::*;

use std::ops::Try;

impl<P, MkA, A, F, I, E, R, Fail> ParseOnce<I, E, Fail> for TryFold<P, MkA, F>
where
    MkA: FnOnce() -> A,
    P: ParseMut<I, E, Fail>,
    F: FnMut(A, P::Output) -> R,
    R: Try<Ok = A>,
    E: ParseError<I>,
{
    type Output = R;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self { parser, mk_acc, func } = self;

        crate::utils::from_result(try_fold_parse_once(
            parser,
            mk_acc(),
            crate::utils::to_result(func),
            input,
        ))
    }
}

impl<P, MkA, A, F, I, E, R, Fail> ParseMut<I, E, Fail> for TryFold<P, MkA, F>
where
    MkA: FnMut() -> A,
    P: ParseMut<I, E, Fail>,
    F: FnMut(A, P::Output) -> R,
    R: Try<Ok = A>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self { parser, mk_acc, func } = self;

        TryFold {
            parser: parser.by_mut(),
            mk_acc,
            func,
        }
        .parse_once(input)
    }
}

impl<P, MkA, A, F, I, E, R, Fail> Parse<I, E, Fail> for TryFold<P, MkA, F>
where
    MkA: Fn() -> A,
    P: Parse<I, E, Fail>,
    F: Fn(A, P::Output) -> R,
    R: Try<Ok = A>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self { parser, mk_acc, func } = self;

        TryFold {
            parser: parser.by_ref(),
            mk_acc,
            func,
        }
        .parse_once(input)
    }
}
