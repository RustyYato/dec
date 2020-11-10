use crate::error::*;
use crate::fold::FoldRange;
use crate::traits::*;

use std::ops::RangeBounds;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Range<R, P, F> {
    pub range: R,
    pub parser: P,
    pub collection: F,
}

pub fn many0<P, O>(parser: P) -> Range<std::ops::RangeFull, P, impl Copy + Fn() -> Vec<O>> {
    Range {
        parser,
        range: ..,
        collection: Vec::new,
    }
}

pub fn many1<P, O>(parser: P) -> Range<std::ops::RangeFrom<usize>, P, impl Copy + Fn() -> Vec<O>> {
    Range {
        parser,
        range: 1..,
        collection: Vec::new,
    }
}

pub fn range<P, O, R: RangeBounds<usize>>(
    range: R,
    parser: P,
) -> Range<R, P, impl Copy + Fn() -> Vec<O>> {
    Range {
        range,
        parser,
        collection: Vec::new,
    }
}

pub fn imany0<P>(parser: P) -> Range<std::ops::RangeFull, P, impl Copy + Fn() -> crate::Ignore> {
    Range {
        parser,
        range: ..,
        collection: crate::Ignore,
    }
}

pub fn imany1<P>(
    parser: P,
) -> Range<std::ops::RangeFrom<usize>, P, impl Copy + Fn() -> crate::Ignore> {
    Range {
        parser,
        range: 1..,
        collection: crate::Ignore,
    }
}

pub fn irange<P, R: RangeBounds<usize>>(
    range: R,
    parser: P,
) -> Range<R, P, impl Copy + Fn() -> crate::Ignore> {
    Range {
        range,
        parser,
        collection: crate::Ignore,
    }
}

impl<R, F, C, P, I, E> ParseOnce<I, E> for Range<R, P, F>
where
    R: RangeBounds<usize>,
    F: FnOnce() -> C,
    C: Extend<P::Output>,
    P: ParseMut<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    type Output = C;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            range,
            parser,
            collection,
        } = self;
        FoldRange {
            range,
            parser,
            func: crate::extend,
            mk_acc: collection,
        }
        .parse_once(input)
    }
}

impl<R, F, C, P, I, E> ParseMut<I, E> for Range<R, P, F>
where
    R: RangeBounds<usize> + Clone,
    F: FnMut() -> C,
    C: Extend<P::Output>,
    P: ParseMut<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            range,
            parser,
            collection,
        } = self;
        Range {
            range: range.clone(),
            parser: parser.by_mut(),
            collection,
        }
        .parse_once(input)
    }
}

impl<R, F, C, P, I, E> Parse<I, E> for Range<R, P, F>
where
    R: RangeBounds<usize> + Clone,
    F: Fn() -> C,
    C: Extend<P::Output>,
    P: Parse<I, E>,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            range,
            parser,
            collection,
        } = self;
        Range {
            range: range.clone(),
            parser: parser.by_ref(),
            collection,
        }
        .parse_once(input)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tag::Tag;

    #[test]
    fn test_range() {
        let parser = range(.., Tag('.'));
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, "...input"),
            Ok(("input", vec!['.'; 3]))
        );
        let parser = range(..2, Tag('.'));
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, "...input"),
            Ok(("..input", vec!['.'; 1]))
        );
        let parser = range(..=2, Tag('.'));
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, "...input"),
            Ok((".input", vec!['.'; 2]))
        );
        let parser = range(..2, Tag('.'));
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, ".input"),
            Ok(("input", vec!['.'; 1]))
        );
        let parser = range(2.., Tag('.'));
        assert_eq!(
            ParseOnce::parse_once(parser, ".input"),
            Err(Error::Error(("input", ErrorKind::RangeStart)))
        );
        let parser = range(2.., Tag('.'));
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(parser, "..input"),
            Ok(("input", vec!['.'; 2]))
        );

        let parser = Range {
            range: 1..=3,
            parser: Tag('.'),
            collection: String::new,
        };
        assert_eq!(
            ParseOnce::parse_once(parser.by_ref(), "input"),
            Err(Error::Error(("input", ErrorKind::RangeStart)))
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
