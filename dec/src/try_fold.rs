use crate::error::ParseError;
use crate::prelude::*;
use crate::traits::*;

#[cfg(feature = "nightly")]
mod nightly;
#[cfg(not(feature = "nightly"))]
mod stable;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TryFold<P, V, F> {
    pub parser: P,
    pub value: V,
    pub func: F,
}

fn try_fold_parse_once<
    P: ParseMut<I, E>,
    V,
    F: FnMut(V, P::Output) -> Result<V, E2>,
    I: Clone,
    E: ParseError<I>,
    E2,
>(
    mut parser: P,
    mut value: V,
    mut func: F,
    mut input: I,
) -> PResult<I, Result<V, E2>, E> {
    loop {
        match parser.parse_mut(input.clone()) {
            Ok((i, out)) => {
                value = match func(value, out) {
                    Ok(val) => val,
                    Err(err) => return Ok((input, Err(err))),
                };
                input = i;
            }
            Err(Error::Error(_)) => return Ok((input, Ok(value))),
            Err(err) => return Err(err),
        }
    }
}
