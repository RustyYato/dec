use dec_core::{
    error::{Error, PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

#[cfg(feature = "nightly")]
mod nightly;
#[cfg(not(feature = "nightly"))]
mod stable;

pub fn try_fold<P, A: Clone, F>(acc: A, parser: P, func: F) -> TryFold<P, impl Fn() -> A + Clone, F> {
    TryFold {
        parser,
        mk_acc: move || acc.clone(),
        func,
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TryFold<P, A, F> {
    pub parser: P,
    pub mk_acc: A,
    pub func: F,
}

fn try_fold_parse_once<P, A, F, I, E, E2>(
    mut parser: P,
    mut acc: A,
    mut func: F,
    mut input: I,
) -> PResult<I, Result<A, E2>, E>
where
    P: ParseMut<I, E>,
    F: FnMut(A, P::Output) -> Result<A, E2>,
    I: Clone,
    E: ParseError<I>,
{
    loop {
        break match parser.parse_mut(input.clone()) {
            Ok((i, out)) => {
                input = i;
                match func(acc, out) {
                    Ok(val) => {
                        acc = val;
                        continue
                    }
                    Err(err) => Ok((input, Err(err))),
                }
            }
            Err(Error::Error(_)) => Ok((input, Ok(acc))),
            Err(err) => Err(err),
        }
    }
}
