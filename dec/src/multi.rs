use crate::error::*;
use crate::fold::FoldRange;
use crate::traits::*;

use std::ops::RangeBounds;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Range<R, P, F>(pub R, pub P, pub F);

pub fn many0<P, O>(parser: P) -> Range<std::ops::RangeFull, P, impl Copy + Fn() -> Vec<O>> {
    Range(.., parser, Vec::new)
}

pub fn many1<P, O>(parser: P) -> Range<std::ops::RangeFrom<usize>, P, impl Copy + Fn() -> Vec<O>> {
    Range(1.., parser, Vec::new)
}

pub fn range<P, O, R: RangeBounds<usize>>(
    range: R,
    parser: P,
) -> Range<R, P, impl Copy + Fn() -> Vec<O>> {
    Range(range, parser, Vec::new)
}

pub fn imany0<P>(parser: P) -> Range<std::ops::RangeFull, P, impl Copy + Fn() -> crate::Ignore> {
    Range(.., parser, crate::Ignore)
}

pub fn imany1<P>(
    parser: P,
) -> Range<std::ops::RangeFrom<usize>, P, impl Copy + Fn() -> crate::Ignore> {
    Range(1.., parser, crate::Ignore)
}

pub fn irange<P, R: RangeBounds<usize>>(
    range: R,
    parser: P,
) -> Range<R, P, impl Copy + Fn() -> crate::Ignore> {
    Range(range, parser, crate::Ignore)
}

fn extend<C: Extend<T>, T>(mut vec: C, item: T) -> C {
    vec.extend(Some(item));
    vec
}

impl<
        R: RangeBounds<usize>,
        F: FnOnce() -> C,
        C: Extend<P::Output>,
        P: ParseMut<I, E>,
        I: Clone,
        E: ParseError<I>,
    > ParseOnce<I, E> for Range<R, P, F>
{
    type Output = C;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self(range, parser, collection) = self;
        FoldRange {
            range,
            parser,
            func: extend,
            value: collection(),
        }
        .parse_once(input)
    }
}

impl<
        R: RangeBounds<usize> + Clone,
        F: FnMut() -> C,
        C: Extend<P::Output>,
        P: ParseMut<I, E>,
        I: Clone,
        E: ParseError<I>,
    > ParseMut<I, E> for Range<R, P, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self(range, parser, collection) = self;
        Range(range.clone(), parser.by_mut(), collection).parse_once(input)
    }
}

impl<
        R: RangeBounds<usize> + Clone,
        F: Fn() -> C,
        C: Extend<P::Output>,
        P: Parse<I, E>,
        I: Clone,
        E: ParseError<I>,
    > Parse<I, E> for Range<R, P, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self(range, parser, collection) = self;
        Range(range.clone(), parser.by_ref(), collection).parse_once(input)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tag::Tag;

    #[test]
    fn range() {
        let parser = Range(.., Tag('.'), Vec::new);
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, "...input"),
            Ok(("input", vec!['.'; 3]))
        );
        let parser = Range(..2, Tag('.'), Vec::new);
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, "...input"),
            Ok(("..input", vec!['.'; 1]))
        );
        let parser = Range(..=2, Tag('.'), Vec::new);
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, "...input"),
            Ok((".input", vec!['.'; 2]))
        );
        let parser = Range(..2, Tag('.'), Vec::new);
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, ".input"),
            Ok(("input", vec!['.'; 1]))
        );
        let parser = Range(2.., Tag('.'), Vec::new);
        assert_eq!(
            ParseOnce::parse_once(parser, ".input"),
            Err(Error::Error(("input", ErrorKind::Tag)))
        );
        let parser = Range(2.., Tag('.'), Vec::new);
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, "..input"),
            Ok(("input", vec!['.'; 2]))
        );

        let parser = Range(1..=3, Tag('.'), String::new);
        assert_eq!(
            ParseOnce::parse_once(parser.by_ref(), "input"),
            Err(Error::Error(("input", ErrorKind::Tag)))
        );
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser.by_ref(), ".input"),
            Ok(("input", ".".to_string()))
        );
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser.by_ref(), "..input"),
            Ok(("input", "..".to_string()))
        );
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser.by_ref(), "...input"),
            Ok(("input", "...".to_string()))
        );
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser.by_ref(), "....input"),
            Ok((".input", "...".to_string()))
        );
    }
}
