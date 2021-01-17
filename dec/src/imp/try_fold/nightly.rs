use super::*;

use std::ops::Try;

impl<P, MkA, A, F, I, E, R> ParseOnce<I, E> for TryFold<P, MkA, F>
where
    MkA: FnOnce() -> A,
    P: ParseMut<I, E>,
    F: FnMut(A, P::Output) -> R,
    E: ParseError<I>,
    R: Try<Ok = A>,
{
    type Output = R;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self { parser, mk_acc, func } = self;

        crate::utils::from_result(try_fold_parse_once(
            parser,
            mk_acc(),
            crate::utils::to_result(func),
            input,
        ))
    }
}

impl<P, MkA, A, F, I, E, R> ParseMut<I, E> for TryFold<P, MkA, F>
where
    MkA: FnMut() -> A,
    P: ParseMut<I, E>,
    F: FnMut(A, P::Output) -> R,
    E: ParseError<I>,
    R: Try<Ok = A>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self { parser, mk_acc, func } = self;

        TryFold {
            parser: parser.by_mut(),
            mk_acc,
            func,
        }
        .parse_once(input)
    }
}

impl<P, MkA, A, F, I, E, R> Parse<I, E> for TryFold<P, MkA, F>
where
    MkA: Fn() -> A,
    P: Parse<I, E>,
    F: Fn(A, P::Output) -> R,
    E: ParseError<I>,
    R: Try<Ok = A>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self { parser, mk_acc, func } = self;

        TryFold {
            parser: parser.by_ref(),
            mk_acc,
            func,
        }
        .parse_once(input)
    }
}
