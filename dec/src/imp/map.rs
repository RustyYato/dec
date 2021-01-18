use dec_core::{
    error::{Error, PResult, ParseError},
    Parse, ParseMut, ParseOnce,
};

use core::marker::PhantomData;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Map<P, F>(pub P, pub F);

impl<P, Output, F, I, E, Fail> ParseOnce<I, E, Fail> for Map<P, F>
where
    P: ParseOnce<I, E, Fail>,
    F: FnOnce(P::Output) -> Output,
    E: ParseError<I>,
{
    type Output = Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse_once(input)?;
        Ok((input, (self.1)(output)))
    }
}

impl<P, Output, F, I, E, Fail> ParseMut<I, E, Fail> for Map<P, F>
where
    P: ParseMut<I, E, Fail>,
    F: FnMut(P::Output) -> Output,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse_mut(input)?;
        Ok((input, (self.1)(output)))
    }
}

impl<P, Output, F, I, E, Fail> Parse<I, E, Fail> for Map<P, F>
where
    P: Parse<I, E, Fail>,
    F: Fn(P::Output) -> Output,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse(input)?;
        Ok((input, (self.1)(output)))
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MapErr<P, F, E>(pub P, pub F, pub PhantomData<E>);

impl<P, Error, F, I, E, Fail> ParseOnce<I, Error, Fail> for MapErr<P, F, E>
where
    P: ParseOnce<I, E, Fail>,
    F: FnOnce(E) -> Error,
    Error: ParseError<I>,
{
    type Output = P::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, Error, Fail> {
        match self.0.parse_once(input) {
            Ok(res) => Ok(res),
            Err(self::Error::Error(err)) => Err(self::Error::Error((self.1)(err))),
            Err(self::Error::Failure(err)) => Err(self::Error::Failure(err)),
        }
    }
}

impl<P, Error, F, I, E, Fail> ParseMut<I, Error, Fail> for MapErr<P, F, E>
where
    P: ParseMut<I, E, Fail>,
    F: FnMut(E) -> Error,
    Error: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, Error, Fail> {
        match self.0.parse_mut(input) {
            Ok(res) => Ok(res),
            Err(self::Error::Error(err)) => Err(self::Error::Error((self.1)(err))),
            Err(self::Error::Failure(err)) => Err(self::Error::Failure(err)),
        }
    }
}

impl<P, Error, F, I, E, Fail> Parse<I, Error, Fail> for MapErr<P, F, E>
where
    P: Parse<I, E, Fail>,
    F: Fn(E) -> Error,
    E: ParseError<I>,
    Error: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, Error, Fail> {
        match self.0.parse(input) {
            Ok(res) => Ok(res),
            Err(self::Error::Error(err)) => Err(self::Error::Error((self.1)(err))),
            Err(self::Error::Failure(err)) => Err(self::Error::Failure(err)),
        }
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TryMap<P, F>(pub P, pub F);

impl<P, Output, F, I, E, Fail> ParseOnce<I, E, Fail> for TryMap<P, F>
where
    P: ParseOnce<I, E, Fail>,
    F: FnOnce(P::Output) -> Result<Output, E>,
    E: ParseError<I>,
{
    type Output = Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse_once(input)?;
        Ok((input, (self.1)(output)?))
    }
}

impl<P, Output, F, I, E, Fail> ParseMut<I, E, Fail> for TryMap<P, F>
where
    P: ParseMut<I, E, Fail>,
    F: FnMut(P::Output) -> Result<Output, E>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse_mut(input)?;
        Ok((input, (self.1)(output)?))
    }
}

impl<P, Output, F, I, E, Fail> Parse<I, E, Fail> for TryMap<P, F>
where
    P: Parse<I, E, Fail>,
    F: Fn(P::Output) -> Result<Output, E>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse(input)?;
        Ok((input, (self.1)(output)?))
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Then<P, F>(pub P, pub F);

impl<P, Q, F, I, E, Fail> ParseOnce<I, E, Fail> for Then<P, F>
where
    P: ParseOnce<I, E, Fail>,
    Q: ParseOnce<I, E, Fail>,
    F: FnOnce(P::Output) -> Q,
    E: ParseError<I>,
{
    type Output = Q::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse_once(input)?;
        (self.1)(output).parse_once(input)
    }
}

impl<P, Q, F, I, E, Fail> ParseMut<I, E, Fail> for Then<P, F>
where
    P: ParseMut<I, E, Fail>,
    Q: ParseOnce<I, E, Fail>,
    F: FnMut(P::Output) -> Q,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse_mut(input)?;
        (self.1)(output).parse_once(input)
    }
}

impl<P, Q, F, I, E, Fail> Parse<I, E, Fail> for Then<P, F>
where
    P: Parse<I, E, Fail>,
    Q: ParseOnce<I, E, Fail>,
    F: Fn(P::Output) -> Q,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse(input)?;
        (self.1)(output).parse_once(input)
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TryThen<P, F>(pub P, pub F);

impl<P, Q, F, I, E, Fail> ParseOnce<I, E, Fail> for TryThen<P, F>
where
    P: ParseOnce<I, E, Fail>,
    Q: ParseOnce<I, E, Fail>,
    F: FnOnce(P::Output) -> Result<Q, E>,
    E: ParseError<I>,
{
    type Output = Q::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse_once(input)?;
        (self.1)(output)?.parse_once(input)
    }
}

impl<P, Q, F, I, E, Fail> ParseMut<I, E, Fail> for TryThen<P, F>
where
    P: ParseMut<I, E, Fail>,
    Q: ParseOnce<I, E, Fail>,
    F: FnMut(P::Output) -> Result<Q, E>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse_mut(input)?;
        (self.1)(output)?.parse_once(input)
    }
}

impl<P, Q, F, I, E, Fail> Parse<I, E, Fail> for TryThen<P, F>
where
    P: Parse<I, E, Fail>,
    Q: ParseOnce<I, E, Fail>,
    F: Fn(P::Output) -> Result<Q, E>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let (input, output) = self.0.parse(input)?;
        (self.1)(output)?.parse_once(input)
    }
}
