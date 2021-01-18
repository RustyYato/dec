use dec_core::{
    error::{CaptureInput, Error, ErrorKind, PResult, ParseError},
    Parse, ParseMut, ParseOnce,
};

pub fn tag<T>(tag: T) -> Tag<T> { Tag(tag) }

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Tag<T>(pub T);

impl<T, I, E, Fail> ParseOnce<I, E, Fail> for Tag<T>
where
    T: dec_core::Tag<I>,
    E: ParseError<I>,
{
    type Output = T::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> { self.parse(input) }
}

impl<T, I, E, Fail> ParseMut<I, E, Fail> for Tag<T>
where
    T: dec_core::Tag<I>,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { self.parse(input) }
}

impl<T, I, E, Fail> Parse<I, E, Fail> for Tag<T>
where
    T: dec_core::Tag<I>,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        match self.0.parse_tag(input) {
            Ok(value) => Ok(value),
            Err(Error::Error(CaptureInput(input))) => Err(Error::Error(E::from_input_kind(input, ErrorKind::Tag))),
            Err(Error::Failure(inf)) => match inf {},
        }
    }
}

#[cfg(test)]
mod test {
    use dec_core::error::DefaultError;

    use super::*;

    #[test]
    fn test_tag_str() {
        assert_eq!(
            ParseOnce::<_, DefaultError<_>>::parse_once(Tag("hi"), "hibye"),
            Ok(("bye", "hi"))
        );
        assert_eq!(
            ParseOnce::<_, (&str, ErrorKind)>::parse_once(Tag("hi"), "byehi"),
            Err(Error::Error(("byehi", ErrorKind::Tag)))
        );
    }

    #[test]
    fn test_tag_char() {
        assert_eq!(
            ParseOnce::<_, DefaultError<_>>::parse_once(Tag('h'), "hibye"),
            Ok(("ibye", "h"))
        );
        assert_eq!(
            ParseOnce::<_, (&str, ErrorKind)>::parse_once(Tag('h'), "byehi"),
            Err(Error::Error(("byehi", ErrorKind::Tag)))
        );
    }

    #[test]
    fn test_tag_str_from_byte() {
        assert_eq!(
            ParseOnce::<_, DefaultError<_>>::parse_once(Tag('h'), "hibye".as_bytes()),
            Ok(("ibye".as_bytes(), "h"))
        );
        assert_eq!(
            ParseOnce::<_, DefaultError<_>>::parse_once(Tag('😃'), "😃hibye".as_bytes()),
            Ok(("hibye".as_bytes(), "😃"))
        );

        assert_eq!(
            ParseOnce::<_>::parse_once(Tag('h'), "byehi"),
            Err(Error::Error(("byehi", ErrorKind::Tag)))
        );
        assert_eq!(
            ParseOnce::<_>::parse_once(Tag('😃'), "hibye😃".as_bytes()),
            Err(Error::Error(("hibye😃".as_bytes(), ErrorKind::Tag)))
        );
    }
}
