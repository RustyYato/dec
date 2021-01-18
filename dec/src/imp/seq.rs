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

impl<A, B, I, E, Fail> ParseOnce<I, E, Fail> for Fst<A, B>
where
    A: ParseOnce<I, E, Fail>,
    B: ParseOnce<I, E, Fail>,
    E: ParseError<I>,
{
    type Output = A::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> { Map(All((self.0, self.1)), _fst).parse_once(input) }
}

impl<A, B, I, E, Fail> ParseMut<I, E, Fail> for Fst<A, B>
where
    A: ParseMut<I, E, Fail>,
    B: ParseMut<I, E, Fail>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Map(All((self.0.by_mut(), self.1.by_mut())), _fst).parse_once(input)
    }
}

impl<A, B, I, E, Fail> Parse<I, E, Fail> for Fst<A, B>
where
    A: Parse<I, E, Fail>,
    B: Parse<I, E, Fail>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Map(All((self.0.by_ref(), self.1.by_ref())), _fst).parse_once(input)
    }
}

impl<A, B, I, E, Fail> ParseOnce<I, E, Fail> for Snd<A, B>
where
    A: ParseOnce<I, E, Fail>,
    B: ParseOnce<I, E, Fail>,
    E: ParseError<I>,
{
    type Output = B::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> { Map(All((self.0, self.1)), _snd).parse_once(input) }
}

impl<A, B, I, E, Fail> ParseMut<I, E, Fail> for Snd<A, B>
where
    A: ParseMut<I, E, Fail>,
    B: ParseMut<I, E, Fail>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Map(All((self.0.by_mut(), self.1.by_mut())), _snd).parse_once(input)
    }
}

impl<A, B, I, E, Fail> Parse<I, E, Fail> for Snd<A, B>
where
    A: Parse<I, E, Fail>,
    B: Parse<I, E, Fail>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Map(All((self.0.by_ref(), self.1.by_ref())), _snd).parse_once(input)
    }
}

impl<A, B, C, I, E, Fail> ParseOnce<I, E, Fail> for Mid<A, B, C>
where
    A: ParseOnce<I, E, Fail>,
    B: ParseOnce<I, E, Fail>,
    C: ParseOnce<I, E, Fail>,
    E: ParseError<I>,
{
    type Output = B::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Map(All((self.0, self.1, self.2)), _mid).parse_once(input)
    }
}

impl<A, B, C, I, E, Fail> ParseMut<I, E, Fail> for Mid<A, B, C>
where
    A: ParseMut<I, E, Fail>,
    B: ParseMut<I, E, Fail>,
    C: ParseMut<I, E, Fail>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Map(All((self.0.by_mut(), self.1.by_mut(), self.2.by_mut())), _mid).parse_once(input)
    }
}

impl<A, B, C, I, E, Fail> Parse<I, E, Fail> for Mid<A, B, C>
where
    A: Parse<I, E, Fail>,
    B: Parse<I, E, Fail>,
    C: Parse<I, E, Fail>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        Map(All((self.0.by_ref(), self.1.by_ref(), self.2.by_ref())), _mid).parse_once(input)
    }
}
