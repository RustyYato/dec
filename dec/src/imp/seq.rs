use dec_core::{
    error::{PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

pub use crate::{map::Map, seq::All};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Fst<A, B>(pub A, pub B);
#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Snd<A, B>(pub A, pub B);
#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Mid<A, B, C>(pub A, pub B, pub C);

fn _fst<A, B>((a, _): (A, B)) -> A { a }

fn _snd<A, B>((_, b): (A, B)) -> B { b }

fn _mid<A, B, C>((_, b, _): (A, B, C)) -> B { b }

impl<A, B, I, E> ParseOnce<I, E> for Fst<A, B>
where
    A: ParseOnce<I, E>,
    B: ParseOnce<I, E>,
    E: ParseError<I>,
{
    type Output = A::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> { Map(All((self.0, self.1)), _fst).parse_once(input) }
}

impl<A, B, I, E> ParseMut<I, E> for Fst<A, B>
where
    A: ParseMut<I, E>,
    B: ParseMut<I, E>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Map(All((self.0.by_mut(), self.1.by_mut())), _fst).parse_once(input)
    }
}

impl<A, B, I, E> Parse<I, E> for Fst<A, B>
where
    A: Parse<I, E>,
    B: Parse<I, E>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Map(All((self.0.by_ref(), self.1.by_ref())), _fst).parse_once(input)
    }
}

impl<A, B, I, E> ParseOnce<I, E> for Snd<A, B>
where
    A: ParseOnce<I, E>,
    B: ParseOnce<I, E>,
    E: ParseError<I>,
{
    type Output = B::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> { Map(All((self.0, self.1)), _snd).parse_once(input) }
}

impl<A, B, I, E> ParseMut<I, E> for Snd<A, B>
where
    A: ParseMut<I, E>,
    B: ParseMut<I, E>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Map(All((self.0.by_mut(), self.1.by_mut())), _snd).parse_once(input)
    }
}

impl<A, B, I, E> Parse<I, E> for Snd<A, B>
where
    A: Parse<I, E>,
    B: Parse<I, E>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Map(All((self.0.by_ref(), self.1.by_ref())), _snd).parse_once(input)
    }
}

impl<A, B, C, I, E> ParseOnce<I, E> for Mid<A, B, C>
where
    A: ParseOnce<I, E>,
    B: ParseOnce<I, E>,
    C: ParseOnce<I, E>,
    E: ParseError<I>,
{
    type Output = B::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        Map(All((self.0, self.1, self.2)), _mid).parse_once(input)
    }
}

impl<A, B, C, I, E> ParseMut<I, E> for Mid<A, B, C>
where
    A: ParseMut<I, E>,
    B: ParseMut<I, E>,
    C: ParseMut<I, E>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Map(All((self.0.by_mut(), self.1.by_mut(), self.2.by_mut())), _mid).parse_once(input)
    }
}

impl<A, B, C, I, E> Parse<I, E> for Mid<A, B, C>
where
    A: Parse<I, E>,
    B: Parse<I, E>,
    C: Parse<I, E>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Map(All((self.0.by_ref(), self.1.by_ref(), self.2.by_ref())), _mid).parse_once(input)
    }
}
