use std::ops::RangeBounds;

use crate::{error::*, imp::ranged::Ranged, map::Map, traits::*, try_fold::TryFold};

pub fn fold<P, A: Clone, F>(acc: A, parser: P, func: F) -> Fold<P, impl Fn() -> A + Clone, F> {
    Fold {
        parser,
        mk_acc: move || acc.clone(),
        func,
    }
}

pub fn fold_exact<P, A: Clone, F>(
    count: usize,
    acc: A,
    parser: P,
    func: F,
) -> FoldRange<P, impl Fn() -> A + Clone, F, std::ops::RangeInclusive<usize>> {
    FoldRange {
        range: count..=count,
        parser,
        mk_acc: move || acc.clone(),
        func,
    }
}

pub fn fold_range<R: RangeBounds<usize>, P, A: Clone, F>(
    range: R,
    acc: A,
    parser: P,
    func: F,
) -> FoldRange<P, impl Fn() -> A + Clone, F, R> {
    FoldRange {
        range,
        parser,
        mk_acc: move || acc.clone(),
        func,
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Fold<P, A, F> {
    pub parser: P,
    pub mk_acc: A,
    pub func: F,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FoldRange<P, A, F, R> {
    pub parser: P,
    pub mk_acc: A,
    pub func: F,
    pub range: R,
}

impl<P, MkA, A, F, I, E> ParseOnce<I, E> for Fold<P, MkA, F>
where
    MkA: FnOnce() -> A,
    P: ParseMut<I, E>,
    F: FnMut(A, P::Output) -> A,
    I: Clone,
    E: ParseError<I>,
{
    type Output = A;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self { parser, mk_acc, func } = self;

        TryFold {
            parser,
            mk_acc,
            func: crate::utils::ok(func),
        }
        .parse_once(input)
        .map(crate::utils::unwrap_absurd)
    }
}

impl<P, MkA, A, F, I, E> ParseMut<I, E> for Fold<P, MkA, F>
where
    MkA: FnMut() -> A,
    P: ParseMut<I, E>,
    F: FnMut(A, P::Output) -> A,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self { parser, mk_acc, func } = self;

        Fold {
            parser: parser.by_mut(),
            mk_acc,
            func,
        }
        .parse_once(input)
    }
}

impl<P, MkA, A, F, I, E> Parse<I, E> for Fold<P, MkA, F>
where
    MkA: Fn() -> A,
    P: Parse<I, E>,
    F: Fn(A, P::Output) -> A,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self { parser, mk_acc, func } = self;

        Fold {
            parser: parser.by_ref(),
            mk_acc,
            func,
        }
        .parse_once(input)
    }
}

impl<R, P, A, MkA, F, I, E> ParseOnce<I, E> for FoldRange<P, MkA, F, R>
where
    R: RangeBounds<usize>,
    P: ParseMut<I, E>,
    MkA: FnOnce() -> A,
    F: FnMut(A, P::Output) -> A,
    I: Clone,
    E: ParseError<I>,
{
    type Output = A;

    fn parse_once(self, mut input: I) -> PResult<I, Self::Output, E> {
        let Self {
            mut parser,
            mk_acc,
            mut func,
            range,
        } = self;

        let mut acc = mk_acc();

        let (mut prefix, mut tail) = match Ranged::new(range) {
            Some(ranged) => ranged.split(),
            None => return Ok((input, acc)),
        };

        if prefix.next().is_some() {
            let (next_input, next_acc) = TryFold {
                parser: parser.by_mut(),
                func: crate::utils::step(&mut func, prefix),
                mk_acc: crate::utils::value(acc),
            }
            .parse_once(input)?;

            input = next_input;
            acc = match next_acc {
                // ended because index reached range start
                Err(acc) => acc,
                // ended because parser failed
                Ok(_) => return Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::RangeStart))),
            }
        }

        if tail.next().is_some() {
            Map(
                TryFold {
                    parser,
                    func: crate::utils::step(func, tail),
                    mk_acc: crate::utils::value(acc),
                },
                crate::utils::into_inner,
            )
            .parse_once(input)
        } else {
            Ok((input, acc))
        }
    }
}

impl<R, P, MkA, A, F, I, E> ParseMut<I, E> for FoldRange<P, MkA, F, R>
where
    R: RangeBounds<usize> + Clone,
    P: ParseMut<I, E>,
    MkA: FnMut() -> A,
    F: FnMut(A, P::Output) -> A,
    I: Clone,
    E: ParseError<I>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            mk_acc,
            func,
            range,
        } = self;

        FoldRange {
            parser: parser.by_mut(),
            mk_acc,
            func,
            range: range.clone(),
        }
        .parse_once(input)
    }
}

impl<R, P, MkA, A, F, I, E> Parse<I, E> for FoldRange<P, MkA, F, R>
where
    R: RangeBounds<usize> + Clone,
    P: Parse<I, E>,
    MkA: Fn() -> A,
    F: Fn(A, P::Output) -> A,
    I: Clone,
    E: ParseError<I>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            mk_acc,
            func,
            range,
        } = self;

        FoldRange {
            parser: parser.by_ref(),
            mk_acc,
            func,
            range: range.clone(),
        }
        .parse_once(input)
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Range<P, R, MkC> {
    pub range: R,
    pub parser: P,
    pub collection: MkC,
}

impl<I, E, P, R, MkC, C> ParseOnce<I, E> for Range<P, R, MkC>
where
    I: Clone,
    E: ParseError<I>,
    P: ParseMut<I, E>,
    R: RangeBounds<usize>,
    MkC: FnOnce() -> C,
    C: Extend<P::Output>,
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
            func: crate::utils::extend,
            mk_acc: collection,
        }
        .parse_once(input)
    }
}

impl<I, E, P, R, MkC, C> ParseMut<I, E> for Range<P, R, MkC>
where
    I: Clone,
    E: ParseError<I>,
    P: ParseMut<I, E>,
    R: RangeBounds<usize> + Clone,
    MkC: FnMut() -> C,
    C: Extend<P::Output>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            range,
            parser,
            collection,
        } = self;
        Range {
            parser: parser.by_mut(),
            range: range.clone(),
            collection,
        }
        .parse_once(input)
    }
}

impl<I, E, P, R, MkC, C> Parse<I, E> for Range<P, R, MkC>
where
    I: Clone,
    E: ParseError<I>,
    P: Parse<I, E>,
    R: RangeBounds<usize> + Clone,
    MkC: Fn() -> C,
    C: Extend<P::Output>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            range,
            parser,
            collection,
        } = self;
        Range {
            parser: parser.by_ref(),
            range: range.clone(),
            collection,
        }
        .parse_once(input)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{error::Error, seq::*, tag::Tag};

    #[test]
    fn foo() -> Result<(), Error<()>> {
        let parser = FoldRange {
            parser: Tag("."),
            mk_acc: || 0,
            func: |acc, _| acc + 1,
            range: ..3,
        };

        let (input, value) = parser.parse_once("....")?;
        assert_eq!(input, "..");
        assert_eq!(value, 2);

        let parser = FoldRange {
            parser: Tag("."),
            mk_acc: || 0,
            func: |acc, _| acc + 1,
            range: ..=2,
        };

        let (input, value) = parser.parse_once(".")?;
        assert_eq!(input, "");
        assert_eq!(value, 1);

        let (input, value) = parser.parse_once("..")?;
        assert_eq!(input, "");
        assert_eq!(value, 2);

        let (input, value) = parser.parse_once("....")?;
        assert_eq!(input, "..");
        assert_eq!(value, 2);

        let parser = FoldRange {
            parser: Tag("."),
            mk_acc: || 0,
            func: |acc, _| acc + 1,
            range: 2..,
        };

        let _: Error<()> = parser.parse(".").unwrap_err();

        let (input, value) = parser.parse("..")?;
        assert_eq!(input, "");
        assert_eq!(value, 2);

        let (input, value) = parser.parse("....")?;
        assert_eq!(input, "");
        assert_eq!(value, 4);

        let parser = FoldRange {
            parser: Tag("."),
            mk_acc: || 0,
            func: |acc, _| acc + 1,
            range: 2..=4,
        };

        let _: Error<()> = parser.parse(".").unwrap_err();

        let (input, value) = parser.parse("..")?;
        assert_eq!(input, "");
        assert_eq!(value, 2);

        let (input, value) = parser.parse("....")?;
        assert_eq!(input, "");
        assert_eq!(value, 4);

        let (input, value) = parser.parse(".....")?;
        assert_eq!(input, ".");
        assert_eq!(value, 4);

        let parser = FoldRange {
            parser: Tag("."),
            mk_acc: || 0,
            func: |acc, _| acc + 1,
            range: 4..=4,
        };

        let _: Error<()> = parser.parse(".").unwrap_err();
        let _: Error<()> = parser.parse("").unwrap_err();
        let _: Error<()> = parser.parse("..").unwrap_err();
        let _: Error<()> = parser.parse("...").unwrap_err();

        let (input, value) = parser.parse("....")?;
        assert_eq!(input, "");
        assert_eq!(value, 4);

        let (input, value) = parser.parse(".....")?;
        assert_eq!(input, ".");
        assert_eq!(value, 4);

        Ok(())
    }

    #[test]
    #[should_panic(expected = "malformed range")]
    fn invalid_range() {
        let _: PResult<_, _, ()> = FoldRange {
            parser: Tag("."),
            mk_acc: || 0,
            func: |acc, _| acc + 1,
            range: 4..=2,
        }
        .parse_once("");
    }

    #[test]
    fn test_range() {
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(range(.., Tag('.')), "...input"),
            Ok(("input", vec!['.'; 3]))
        );
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(range(..2, Tag('.')), "...input"),
            Ok(("..input", vec!['.'; 1]))
        );
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(range(..=2, Tag('.')), "...input"),
            Ok((".input", vec!['.'; 2]))
        );
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(range(..2, Tag('.')), ".input"),
            Ok(("input", vec!['.'; 1]))
        );
        assert_eq!(
            ParseOnce::parse_once(range(2.., Tag('.')), ".input"),
            Err(Error::Error(("input", ErrorKind::RangeStart)))
        );
        assert_eq!(
            ParseOnce::<_, ()>::parse_once(range(2.., Tag('.')), "..input"),
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
