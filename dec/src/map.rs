use crate::error::*;
use crate::traits::*;
use std::marker::PhantomData;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Map<P, F>(pub P, pub F);

impl<P, Output, F, I, E> ParseOnce<I, E> for Map<P, F>
where
    P: ParseOnce<I, E>,
    F: FnOnce(P::Output) -> Output,
    E: ParseError<I>,
{
    type Output = Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_once(input)?;
        Ok((input, (self.1)(output)))
    }
}

impl<P, Output, F, I, E> ParseMut<I, E> for Map<P, F>
where
    P: ParseMut<I, E>,
    F: FnMut(P::Output) -> Output,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_mut(input)?;
        Ok((input, (self.1)(output)))
    }
}

impl<P, Output, F, I, E> Parse<I, E> for Map<P, F>
where
    P: Parse<I, E>,
    F: Fn(P::Output) -> Output,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse(input)?;
        Ok((input, (self.1)(output)))
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MapErr<P, F, E>(pub P, pub F, pub PhantomData<E>);

impl<P, Error, F, I, E> ParseOnce<I, Error> for MapErr<P, F, E>
where
    P: ParseOnce<I, E>,
    F: FnOnce(E) -> Error,
    E: ParseError<I>,
    Error: ParseError<I>,
{
    type Output = P::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, Error> {
        match self.0.parse_once(input) {
            Ok(res) => Ok(res),
            Err(self::Error::Error(err)) => Err(self::Error::Error((self.1)(err))),
            Err(self::Error::Failure(err)) => Err(self::Error::Failure((self.1)(err))),
        }
    }
}

impl<P, Error, F, I, E> ParseMut<I, Error> for MapErr<P, F, E>
where
    P: ParseMut<I, E>,
    F: FnMut(E) -> Error,
    E: ParseError<I>,
    Error: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, Error> {
        match self.0.parse_mut(input) {
            Ok(res) => Ok(res),
            Err(self::Error::Error(err)) => Err(self::Error::Error((self.1)(err))),
            Err(self::Error::Failure(err)) => Err(self::Error::Failure((self.1)(err))),
        }
    }
}

impl<P, Error, F, I, E> Parse<I, Error> for MapErr<P, F, E>
where
    P: Parse<I, E>,
    F: Fn(E) -> Error,
    E: ParseError<I>,
    Error: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, Error> {
        match self.0.parse(input) {
            Ok(res) => Ok(res),
            Err(self::Error::Error(err)) => Err(self::Error::Error((self.1)(err))),
            Err(self::Error::Failure(err)) => Err(self::Error::Failure((self.1)(err))),
        }
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TryMap<P, F>(pub P, pub F);

impl<P, Output, F, I, E> ParseOnce<I, E> for TryMap<P, F>
where
    P: ParseOnce<I, E>,
    F: FnOnce(P::Output) -> Result<Output, E>,
    E: ParseError<I>,
{
    type Output = Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_once(input)?;
        Ok((input, (self.1)(output)?))
    }
}

impl<P, Output, F, I, E> ParseMut<I, E> for TryMap<P, F>
where
    P: ParseMut<I, E>,
    F: FnMut(P::Output) -> Result<Output, E>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_mut(input)?;
        Ok((input, (self.1)(output)?))
    }
}

impl<P, Output, F, I, E> Parse<I, E> for TryMap<P, F>
where
    P: Parse<I, E>,
    F: Fn(P::Output) -> Result<Output, E>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse(input)?;
        Ok((input, (self.1)(output)?))
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Then<P, F>(pub P, pub F);

impl<P, Q, F, I, E> ParseOnce<I, E> for Then<P, F>
where
    P: ParseOnce<I, E>,
    Q: ParseOnce<I, E>,
    F: FnOnce(P::Output) -> Q,
    E: ParseError<I>,
{
    type Output = Q::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_once(input)?;
        (self.1)(output).parse_once(input)
    }
}

impl<P, Q, F, I, E> ParseMut<I, E> for Then<P, F>
where
    P: ParseMut<I, E>,
    Q: ParseOnce<I, E>,
    F: FnMut(P::Output) -> Q,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_mut(input)?;
        (self.1)(output).parse_once(input)
    }
}

impl<P, Q, F, I, E> Parse<I, E> for Then<P, F>
where
    P: Parse<I, E>,
    Q: ParseOnce<I, E>,
    F: Fn(P::Output) -> Q,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse(input)?;
        (self.1)(output).parse_once(input)
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TryThen<P, F>(pub P, pub F);

impl<P, Q, F, I, E> ParseOnce<I, E> for TryThen<P, F>
where
    P: ParseOnce<I, E>,
    Q: ParseOnce<I, E>,
    F: FnOnce(P::Output) -> Result<Q, E>,
    E: ParseError<I>,
{
    type Output = Q::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_once(input)?;
        (self.1)(output)?.parse_once(input)
    }
}

impl<P, Q, F, I, E> ParseMut<I, E> for TryThen<P, F>
where
    P: ParseMut<I, E>,
    Q: ParseOnce<I, E>,
    F: FnMut(P::Output) -> Result<Q, E>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_mut(input)?;
        (self.1)(output)?.parse_once(input)
    }
}

impl<P, Q, F, I, E> Parse<I, E> for TryThen<P, F>
where
    P: Parse<I, E>,
    Q: ParseOnce<I, E>,
    F: Fn(P::Output) -> Result<Q, E>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse(input)?;
        (self.1)(output)?.parse_once(input)
    }
}
