use super::*;

use std::ops::Try;

impl<
        P: ParseMut<I, E>,
        V,
        F: FnMut(V, P::Output) -> R,
        I: Clone,
        E: ParseError<I>,
        R: Try<Ok = V>,
    > ParseOnce<I, E> for TryFold<P, V, F>
{
    type Output = R;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            mut func,
        } = self;

        match try_fold_parse_once(
            parser,
            value,
            move |acc, value| R::into_result(func(acc, value)),
            input,
        ) {
            Ok((input, Ok(value))) => Ok((input, R::from_ok(value))),
            Ok((input, Err(value))) => Ok((input, R::from_error(value))),
            Err(err) => Err(err),
        }
    }
}

impl<
        P: ParseMut<I, E>,
        V: Clone,
        F: FnMut(V, P::Output) -> R,
        I: Clone,
        E: ParseError<I>,
        R: Try<Ok = V>,
    > ParseMut<I, E> for TryFold<P, V, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            func,
        } = self;

        TryFold {
            parser: parser.by_mut(),
            value: value.clone(),
            func,
        }
        .parse_once(input)
    }
}

impl<
        P: Parse<I, E>,
        V: Clone,
        F: Fn(V, P::Output) -> R,
        I: Clone,
        E: ParseError<I>,
        R: Try<Ok = V>,
    > Parse<I, E> for TryFold<P, V, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            func,
        } = self;

        TryFold {
            parser: parser.by_ref(),
            value: value.clone(),
            func,
        }
        .parse_once(input)
    }
}
