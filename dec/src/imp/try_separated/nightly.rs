use super::*;

use core::ops::Try;

impl<I, E, P, S, Fp, Fs, MkA, R, A, Fail> ParseOnce<I, E, Fail> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    P: ParseMut<I, E, Fail>,
    S: ParseMut<I, E, Fail>,
    R: Try<Ok = A>,
    Fp: FnMut(A, P::Output) -> R,
    Fs: FnMut(A, S::Output) -> R,
    MkA: FnOnce() -> A,
    E: ParseError<I>,
{
    type Output = R;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
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

impl<I, E, P, S, Fp, Fs, MkA, R, A, Fail> ParseMut<I, E, Fail> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    P: ParseMut<I, E, Fail>,
    S: ParseMut<I, E, Fail>,
    R: Try<Ok = A>,
    Fp: FnMut(A, P::Output) -> R,
    Fs: FnMut(A, S::Output) -> R,
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

impl<I, E, P, S, Fp, Fs, MkA, R, A, Fail> Parse<I, E, Fail> for TrySeparatedFold<P, S, Fp, Fs, MkA>
where
    P: Parse<I, E, Fail>,
    S: Parse<I, E, Fail>,
    R: Try<Ok = A>,
    Fp: Fn(A, P::Output) -> R,
    Fs: Fn(A, S::Output) -> R,
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
