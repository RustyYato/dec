use crate::{error::*, Parse, ParseExt, ParseMut, ParseOnce};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Context<P>(pub &'static str, pub P);

impl<P, I, E, Fail> ParseOnce<I, E, Fail> for Context<P>
where
    P: ParseOnce<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
    Fail: ParseError<I>,
{
    type Output = P::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self(ctx, parser) = self;
        match parser.parse_once(input.clone()) {
            Ok(ok) => Ok(ok),
            Err(Error::Error(err)) => Err(Error::Error(err.add_context(input, ctx))),
            Err(Error::Failure(err)) => Err(Error::Failure(err.add_context(input, ctx))),
        }
    }
}

impl<P, I, E, Fail> ParseMut<I, E, Fail> for Context<P>
where
    P: ParseMut<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
    Fail: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Context(self.0, self.1.by_mut()).parse_once(input)
    }
}

impl<P, I, E, Fail> Parse<I, E, Fail> for Context<P>
where
    P: Parse<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
    Fail: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Context(self.0, self.1.by_ref()).parse_once(input)
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AppendError<P>(pub ErrorKind, pub P);

impl<P, I, E, Fail> ParseOnce<I, E, Fail> for AppendError<P>
where
    P: ParseOnce<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
    Fail: ParseError<I>,
{
    type Output = P::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self(kind, parser) = self;
        match parser.parse_once(input.clone()) {
            Ok(ok) => Ok(ok),
            Err(Error::Error(err)) => Err(Error::Error(err.append(input, kind))),
            Err(Error::Failure(err)) => Err(Error::Failure(err.append(input, kind))),
        }
    }
}

impl<P, I, E, Fail> ParseMut<I, E, Fail> for AppendError<P>
where
    P: ParseMut<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
    Fail: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        AppendError(self.0, self.1.by_mut()).parse_once(input)
    }
}

impl<P, I, E, Fail> Parse<I, E, Fail> for AppendError<P>
where
    P: Parse<I, E, Fail>,
    I: Clone,
    E: ParseError<I>,
    Fail: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        AppendError(self.0, self.1.by_ref()).parse_once(input)
    }
}
