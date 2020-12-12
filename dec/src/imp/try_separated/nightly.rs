use super::*;

use std_core::ops::Try;

impl<I, E, P, S, Fp, Fs, MkA, R, A> ParseOnce<I, E> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    I: Clone,
    E: ParseError<I>,
    P: ParseMut<I, E>,
    S: ParseMut<I, E>,
    R: Try<Ok = A>,
    Fp: FnMut(A, P::Output) -> R,
    Fs: FnMut(A, S::Output) -> R,
    MkA: FnOnce() -> A,
{
    type Output = R;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            item,
            sep,
            item_func,
            sep_func,
            mk_acc,
        } = self;

        crate::utils::from_result(try_separated_fold_parse_once(
            input,
            item,
            sep,
            crate::utils::to_result(item_func),
            crate::utils::to_result(sep_func),
            mk_acc(),
        ))
    }
}

impl<I, E, P, S, Fp, Fs, MkA, R, A> ParseMut<I, E> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    I: Clone,
    E: ParseError<I>,
    P: ParseMut<I, E>,
    S: ParseMut<I, E>,
    R: Try<Ok = A>,
    Fp: FnMut(A, P::Output) -> R,
    Fs: FnMut(A, S::Output) -> R,
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

        TrySeparatedFold {
            item: item.by_mut(),
            sep: sep.by_mut(),
            item_func,
            sep_func,
            mk_acc,
        }
        .parse_once(input)
    }
}

impl<I, E, P, S, Fp, Fs, MkA, R, A> Parse<I, E> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    I: Clone,
    E: ParseError<I>,
    P: Parse<I, E>,
    S: Parse<I, E>,
    R: Try<Ok = A>,
    Fp: Fn(A, P::Output) -> R,
    Fs: Fn(A, S::Output) -> R,
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

        TrySeparatedFold {
            item: item.by_ref(),
            sep: sep.by_ref(),
            item_func,
            sep_func,
            mk_acc,
        }
        .parse_once(input)
    }
}
