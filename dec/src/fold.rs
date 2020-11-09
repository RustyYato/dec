use crate::error::*;
use crate::map::Map;
use crate::traits::*;
use crate::try_fold::TryFold;

use std::ops::{Bound, RangeBounds};

pub fn fold<P, A, F>(mk_acc: A, parser: P, func: F) -> Fold<P, A, F> {
    Fold {
        parser,
        mk_acc,
        func,
    }
}

pub fn fold_exact<P, A, F>(
    count: usize,
    parser: P,
    mk_acc: A,
    func: F,
) -> FoldRange<P, A, F, std::ops::RangeInclusive<usize>> {
    FoldRange {
        range: count..=count,
        parser,
        mk_acc,
        func,
    }
}

pub fn fold_range<R: RangeBounds<usize>, P, A, F>(
    range: R,
    parser: P,
    mk_acc: A,
    func: F,
) -> FoldRange<P, A, F, R> {
    FoldRange {
        range,
        parser,
        mk_acc,
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

fn absurd<T>(x: std::convert::Infallible) -> T {
    match x {}
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
        let Self {
            parser,
            mk_acc,
            mut func,
        } = self;

        TryFold {
            parser,
            mk_acc,
            func: move |acc, value| Ok(func(acc, value)),
        }
        .parse_once(input)
        .map(|(input, x)| (input, x.unwrap_or_else(absurd)))
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
        let Self {
            parser,
            mk_acc,
            func,
        } = self;

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
        let Self {
            parser,
            mk_acc,
            func,
        } = self;

        Fold {
            parser: parser.by_ref(),
            mk_acc,
            func,
        }
        .parse_once(input)
    }
}

fn into_inner<T>(r: Result<T, T>) -> T {
    match r {
        Ok(x) | Err(x) => x,
    }
}

fn validate_range(start: Bound<usize>, end: Bound<usize>) -> bool {
    match (start, end) {
        (Bound::Excluded(x), Bound::Excluded(y))
        | (Bound::Included(x), Bound::Included(y))
        | (Bound::Included(x), Bound::Excluded(y)) => x <= y,
        (Bound::Excluded(x), Bound::Included(y)) => x < y,
        (Bound::Unbounded, _) | (_, Bound::Unbounded) => true,
    }
}

fn copied(x: Bound<&usize>) -> Bound<usize> {
    match x {
        Bound::Included(&x) => Bound::Included(x),
        Bound::Excluded(&x) => Bound::Excluded(x),
        Bound::Unbounded => Bound::Unbounded,
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

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            mut parser,
            mk_acc,
            mut func,
            range,
        } = self;

        let start = copied(range.start_bound());
        let end = copied(range.end_bound());

        assert!(validate_range(start, end), "malformed range");

        let mut index = 0_usize;

        // fast track no-ops
        match end {
            Bound::Excluded(1) | Bound::Excluded(0) | Bound::Included(0) => {
                return Ok((input, mk_acc()))
            }
            _ => (),
        }

        let start = match start {
            Bound::Included(x) => Some(x),
            Bound::Excluded(x) => x.checked_sub(1),
            Bound::Unbounded => None,
        };

        let (input, value) = if let Some(start) = start {
            let index = &mut index;
            let func = &mut func;
            let (input, value) = TryFold {
                parser: parser.by_mut(),
                mk_acc: mk_acc,
                func: move |acc, value| {
                    let acc = func(acc, value);
                    *index += 1;
                    if *index < start {
                        Ok(acc)
                    } else {
                        Err(acc)
                    }
                },
            }
            .parse_once(input)?;

            match value {
                // ended because index reached range start
                Err(value) => (input, value),
                // ended because parser failed
                Ok(_) => {
                    return Err(Error::Error(ParseError::from_input_kind(
                        input,
                        ErrorKind::RangeStart,
                    )))
                }
            }
        } else {
            (input, mk_acc())
        };

        let is_done = match end {
            Bound::Included(end) => match index.checked_add(1) {
                None => true,
                Some(x) => {
                    index = x;
                    index > end
                }
            },
            Bound::Excluded(end) => {
                index += 1;
                index >= end
            }
            Bound::Unbounded => false,
        };

        if is_done {
            return Ok((input, value));
        }

        Map(
            TryFold {
                parser,
                mk_acc: move || value,
                func: move |acc, value| {
                    let acc = func(acc, value);
                    let is_done = match end {
                        Bound::Included(end) => match index.checked_add(1) {
                            None => true,
                            Some(x) => {
                                index = x;
                                index > end
                            }
                        },
                        Bound::Excluded(end) => {
                            index += 1;
                            index >= end
                        }
                        Bound::Unbounded => false,
                    };
                    if is_done {
                        Err(acc)
                    } else {
                        Ok(acc)
                    }
                },
            },
            into_inner,
        )
        .parse_once(input)
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::Error;
    use crate::tag::Tag;

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
}
