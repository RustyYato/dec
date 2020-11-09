use crate::error::*;
use crate::map::Map;
use crate::traits::*;
use crate::try_fold::TryFold;

use std::ops::{Bound, RangeBounds};

pub fn fold<P, V, F>(parser: P, value: V, func: F) -> Fold<P, V, F> {
    Fold {
        parser,
        value,
        func,
    }
}

pub fn fold_exact<P, V, F>(
    count: usize,
    parser: P,
    value: V,
    func: F,
) -> FoldRange<P, V, F, std::ops::RangeInclusive<usize>> {
    FoldRange {
        range: count..=count,
        parser,
        value,
        func,
    }
}

pub fn fold_range<R: RangeBounds<usize>, P, V, F>(
    range: R,
    parser: P,
    value: V,
    func: F,
) -> FoldRange<P, V, F, R> {
    FoldRange {
        range,
        parser,
        value,
        func,
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Fold<P, V, F> {
    pub parser: P,
    pub value: V,
    pub func: F,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FoldRange<P, V, F, R> {
    pub parser: P,
    pub value: V,
    pub func: F,
    pub range: R,
}

fn absurd<T>(x: std::convert::Infallible) -> T {
    match x {}
}

impl<P: ParseMut<I, E>, V, F: FnMut(V, P::Output) -> V, I: Clone, E: ParseError<I>> ParseOnce<I, E>
    for Fold<P, V, F>
{
    type Output = V;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            mut func,
        } = self;

        TryFold {
            parser,
            value,
            func: move |acc, value| Ok(func(acc, value)),
        }
        .parse_once(input)
        .map(|(input, x)| (input, x.unwrap_or_else(absurd)))
    }
}

impl<P: ParseMut<I, E>, V: Clone, F: FnMut(V, P::Output) -> V, I: Clone, E: ParseError<I>>
    ParseMut<I, E> for Fold<P, V, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            func,
        } = self;

        Fold {
            parser: parser.by_mut(),
            value: value.clone(),
            func,
        }
        .parse_once(input)
    }
}

impl<P: Parse<I, E>, V: Clone, F: Fn(V, P::Output) -> V, I: Clone, E: ParseError<I>> Parse<I, E>
    for Fold<P, V, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            func,
        } = self;

        Fold {
            parser: parser.by_ref(),
            value: value.clone(),
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

impl<
        R: RangeBounds<usize>,
        P: ParseMut<I, E>,
        V,
        F: FnMut(V, P::Output) -> V,
        I: Clone,
        E: ParseError<I>,
    > ParseOnce<I, E> for FoldRange<P, V, F, R>
{
    type Output = V;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            mut parser,
            value,
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
                return Ok((input, value))
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
                value,
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
            (input, value)
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
                value,
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

impl<
        R: RangeBounds<usize> + Clone,
        P: ParseMut<I, E>,
        V: Clone,
        F: FnMut(V, P::Output) -> V,
        I: Clone,
        E: ParseError<I>,
    > ParseMut<I, E> for FoldRange<P, V, F, R>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            func,
            range,
        } = self;

        FoldRange {
            parser: parser.by_mut(),
            value: value.clone(),
            func,
            range: range.clone(),
        }
        .parse_once(input)
    }
}

impl<
        R: RangeBounds<usize> + Clone,
        P: Parse<I, E>,
        V: Clone,
        F: Fn(V, P::Output) -> V,
        I: Clone,
        E: ParseError<I>,
    > Parse<I, E> for FoldRange<P, V, F, R>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            parser,
            value,
            func,
            range,
        } = self;

        FoldRange {
            parser: parser.by_ref(),
            value: value.clone(),
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
            value: 0,
            func: |acc, _| acc + 1,
            range: ..3,
        };

        let (input, value) = parser.parse_once("....")?;
        assert_eq!(input, "..");
        assert_eq!(value, 2);

        let parser = FoldRange {
            parser: Tag("."),
            value: 0,
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
            value: 0,
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
            value: 0,
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
            value: 0,
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
            value: 0,
            func: |acc, _| acc + 1,
            range: 4..=2,
        }
        .parse_once("");
    }
}
