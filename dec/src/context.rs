use crate::error::*;
use crate::prelude::*;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Context<P>(pub &'static str, pub P);

impl<P: ParseOnce<I, E>, I: Clone, E: ParseError<I>> ParseOnce<I, E> for Context<P> {
    type Output = P::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self(ctx, parser) = self;
        match parser.parse_once(input.clone()) {
            Ok(ok) => Ok(ok),
            Err(Error::Error(err)) => Err(Error::Error(err.add_context(input, ctx))),
            Err(Error::Failure(err)) => Err(Error::Failure(err.add_context(input, ctx))),
        }
    }
}

impl<P: ParseMut<I, E>, I: Clone, E: ParseError<I>> ParseMut<I, E> for Context<P> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Context(self.0, self.1.by_mut()).parse_once(input)
    }
}

impl<P: Parse<I, E>, I: Clone, E: ParseError<I>> Parse<I, E> for Context<P> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Context(self.0, self.1.by_ref()).parse_once(input)
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AppendError<P>(pub ErrorKind, pub P);

impl<P: ParseOnce<I, E>, I: Clone, E: ParseError<I>> ParseOnce<I, E> for AppendError<P> {
    type Output = P::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self(kind, parser) = self;
        match parser.parse_once(input.clone()) {
            Ok(ok) => Ok(ok),
            Err(Error::Error(err)) => Err(Error::Error(err.append(input, kind))),
            Err(Error::Failure(err)) => Err(Error::Failure(err.append(input, kind))),
        }
    }
}

impl<P: ParseMut<I, E>, I: Clone, E: ParseError<I>> ParseMut<I, E> for AppendError<P> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        AppendError(self.0, self.1.by_mut()).parse_once(input)
    }
}

impl<P: Parse<I, E>, I: Clone, E: ParseError<I>> Parse<I, E> for AppendError<P> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        AppendError(self.0, self.1.by_ref()).parse_once(input)
    }
}
