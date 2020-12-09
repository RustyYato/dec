use crate::{error::*, prelude::*};

#[cfg(not(feature = "nightly"))]
mod stable;

pub fn try_separated_fold<P, S, Fp, Fs, A: Clone>(
    acc: A,
    item: P,
    sep: S,
    item_func: Fp,
    sep_func: Fs,
) -> TrySeparatedFold<P, S, Fp, Fs, impl Clone + Fn() -> A> {
    TrySeparatedFold {
        item,
        sep,
        item_func,
        sep_func,
        mk_acc: move || acc.clone(),
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TrySeparatedFold<P, S, Fp, Fs, A> {
    pub item: P,
    pub sep: S,
    pub item_func: Fp,
    pub sep_func: Fs,
    pub mk_acc: A,
}

fn try_separated_fold_parse_once<I, E, E2, P, S, Fp, Fs, A>(
    input: I,
    mut item: P,
    mut sep: S,
    mut item_func: Fp,
    mut sep_func: Fs,
    acc: A,
) -> PResult<I, Result<A, E2>, E>
where
    I: Clone,
    E: ParseError<I>,
    P: ParseMut<I, E>,
    S: ParseMut<I, E>,
    Fp: FnMut(A, P::Output) -> Result<A, E2>,
    Fs: FnMut(A, S::Output) -> Result<A, E2>,
{
    fn mk_acc<T>(value: T) -> impl FnOnce() -> T { move || value }

    let mut do_sep = false;

    crate::imp::try_fold::TryFold {
        parser: move |input| {
            let (input, sep) = if do_sep {
                let (input, value) = sep.parse_mut(input)?;
                (input, Some(value))
            } else {
                do_sep = true;
                (input, None)
            };

            let (input, item) = item.parse_mut(input)?;

            Ok((input, (sep, item)))
        },
        func: move |mut acc, (sep, item)| {
            if let Some(sep) = sep {
                acc = sep_func(acc, sep)?;
            }
            item_func(acc, item)
        },
        mk_acc: mk_acc(acc),
    }
    .parse_once(input)
}
