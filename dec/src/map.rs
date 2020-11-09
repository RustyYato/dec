use crate::error::*;
use crate::traits::*;
use std::marker::PhantomData;

type StdResult<T, E> = std::result::Result<T, E>;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Map<P, F>(pub P, pub F);

impl<P: ParseOnce<I, E>, Output, F, I, E: ParseError<I>> ParseOnce<I, E> for Map<P, F>
where
    F: FnOnce(P::Output) -> Output,
{
    type Output = Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_once(input)?;
        Ok((input, (self.1)(output)))
    }
}

impl<P: ParseMut<I, E>, Output, F, I, E: ParseError<I>> ParseMut<I, E> for Map<P, F>
where
    F: FnMut(P::Output) -> Output,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_mut(input)?;
        Ok((input, (self.1)(output)))
    }
}

impl<P: Parse<I, E>, Output, F, I, E: ParseError<I>> Parse<I, E> for Map<P, F>
where
    F: Fn(P::Output) -> Output,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse(input)?;
        Ok((input, (self.1)(output)))
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MapErr<P, F, E>(pub P, pub F, pub PhantomData<E>);

impl<P: ParseOnce<I, E>, Error: ParseError<I>, F, I, E: ParseError<I>> ParseOnce<I, Error>
    for MapErr<P, F, E>
where
    F: FnOnce(E) -> Error,
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

impl<P: ParseMut<I, E>, Error: ParseError<I>, F, I, E: ParseError<I>> ParseMut<I, Error>
    for MapErr<P, F, E>
where
    F: FnMut(E) -> Error,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, Error> {
        match self.0.parse_mut(input) {
            Ok(res) => Ok(res),
            Err(self::Error::Error(err)) => Err(self::Error::Error((self.1)(err))),
            Err(self::Error::Failure(err)) => Err(self::Error::Failure((self.1)(err))),
        }
    }
}

impl<P: Parse<I, E>, Error: ParseError<I>, F, I, E: ParseError<I>> Parse<I, Error>
    for MapErr<P, F, E>
where
    F: Fn(E) -> Error,
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

impl<P: ParseOnce<I, E>, Output, F, I, E: ParseError<I>> ParseOnce<I, E> for TryMap<P, F>
where
    F: FnOnce(P::Output) -> StdResult<Output, E>,
{
    type Output = Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_once(input)?;
        Ok((input, (self.1)(output)?))
    }
}

impl<P: ParseMut<I, E>, Output, F, I, E: ParseError<I>> ParseMut<I, E> for TryMap<P, F>
where
    F: FnMut(P::Output) -> StdResult<Output, E>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_mut(input)?;
        Ok((input, (self.1)(output)?))
    }
}

impl<P: Parse<I, E>, Output, F, I, E: ParseError<I>> Parse<I, E> for TryMap<P, F>
where
    F: Fn(P::Output) -> StdResult<Output, E>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse(input)?;
        Ok((input, (self.1)(output)?))
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Then<P, F>(pub P, pub F);

impl<P: ParseOnce<I, E>, Q: ParseOnce<I, E>, F, I, E: ParseError<I>> ParseOnce<I, E> for Then<P, F>
where
    F: FnOnce(P::Output) -> Q,
{
    type Output = Q::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_once(input)?;
        (self.1)(output).parse_once(input)
    }
}

impl<P: ParseMut<I, E>, Q: ParseOnce<I, E>, F, I, E: ParseError<I>> ParseMut<I, E> for Then<P, F>
where
    F: FnMut(P::Output) -> Q,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_mut(input)?;
        (self.1)(output).parse_once(input)
    }
}

impl<P: Parse<I, E>, Q: ParseOnce<I, E>, F, I, E: ParseError<I>> Parse<I, E> for Then<P, F>
where
    F: Fn(P::Output) -> Q,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse(input)?;
        (self.1)(output).parse_once(input)
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TryThen<P, F>(pub P, pub F);

impl<P: ParseOnce<I, E>, Q: ParseOnce<I, E>, F, I, E: ParseError<I>> ParseOnce<I, E>
    for TryThen<P, F>
where
    F: FnOnce(P::Output) -> StdResult<Q, E>,
{
    type Output = Q::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_once(input)?;
        (self.1)(output)?.parse_once(input)
    }
}

impl<P: ParseMut<I, E>, Q: ParseOnce<I, E>, F, I, E: ParseError<I>> ParseMut<I, E> for TryThen<P, F>
where
    F: FnMut(P::Output) -> StdResult<Q, E>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse_mut(input)?;
        (self.1)(output)?.parse_once(input)
    }
}

impl<P: Parse<I, E>, Q: ParseOnce<I, E>, F, I, E: ParseError<I>> Parse<I, E> for TryThen<P, F>
where
    F: Fn(P::Output) -> StdResult<Q, E>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let (input, output) = self.0.parse(input)?;
        (self.1)(output)?.parse_once(input)
    }
}
