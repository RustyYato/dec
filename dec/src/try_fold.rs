use crate::error::ParseError;
use crate::prelude::*;
use crate::traits::*;

#[cfg(feature = "nightly")]
mod nightly;
#[cfg(not(feature = "nightly"))]
mod stable;

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
        match parser.parse_mut(input.clone()) {
            Ok((i, out)) => {
                input = i;
                acc = match func(acc, out) {
                    Ok(val) => val,
                    Err(err) => return Ok((input, Err(err))),
                };
            }
            Err(Error::Error(_)) => return Ok((input, Ok(acc))),
            Err(err) => return Err(err),
        }
    }
}
