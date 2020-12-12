use dec_core::{
    error::{PResult, ParseError},
    Parse, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Value<T, P>(pub T, pub P);

impl<T, P, I, E> ParseOnce<I, E> for Value<T, P>
where
    P: ParseOnce<I, E>,
    E: ParseError<I>,
{
    type Output = T;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Value(value, parser) = self;
        let (input, _) = parser.parse_once(input)?;
        Ok((input, value))
    }
}

impl<T, P, I, E> ParseMut<I, E> for Value<T, P>
where
    T: Clone,
    P: ParseMut<I, E>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Value(value, parser) = self;
        let (input, _) = parser.parse_mut(input)?;
        Ok((input, value.clone()))
    }
}

impl<T, P, I, E> Parse<I, E> for Value<T, P>
where
    T: Clone,
    P: Parse<I, E>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Value(value, parser) = self;
        let (input, _) = parser.parse(input)?;
        Ok((input, value.clone()))
    }
}
