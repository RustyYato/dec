use std::ops::RangeBounds;

use crate::prelude::*;
use crate::{error::*, traits::InputEq};

use crate::seq::{Fold, FoldRange};

pub fn separated<R: std::ops::RangeBounds<usize>, S, P, O>(
    range: R,
    sep: S,
    item: P,
) -> SeparatedRange<R, S, P, impl Copy + Fn() -> Vec<O>> {
    SeparatedRange {
        range,
        sep,
        item,
        collection: Vec::new,
    }
}

pub fn iseparated<R: std::ops::RangeBounds<usize>, S, P, C, O>(
    range: R,
    sep: S,
    item: P,
) -> SeparatedRange<R, S, P, impl Copy + Fn() -> crate::Ignore> {
    SeparatedRange {
        range,
        sep,
        item,
        collection: crate::Ignore,
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SeparatedFoldRange<R, A, S, P, Fs, Fp> {
    pub sep: S,
    pub item: P,
    pub sep_func: Fs,
    pub item_func: Fp,
    pub acc: A,
    pub range: R,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SeparatedRange<R, S, P, C> {
    pub range: R,
    pub sep: S,
    pub item: P,
    pub collection: C,
}

impl<R, A, Fs, Fp, S, P, I, E> ParseOnce<I, E> for SeparatedFoldRange<R, A, S, P, Fs, Fp>
where
    I: InputEq + Clone,
    E: ParseError<I>,
    S: ParseMut<I, E>,
    P: ParseMut<I, E>,
    Fs: FnMut(A, S::Output) -> A,
    Fp: FnMut(A, P::Output) -> A,
    R: RangeBounds<usize>,
{
    type Output = A;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        let Self {
            mut sep,
            mut item,
            mut sep_func,
            mut item_func,
            acc,
            range,
        } = self;

        let mut do_sep = false;

        FoldRange {
            parser: move |input| {
                let (input, sep) = if do_sep {
                    let (input, value) = sep.parse_mut(input)?;
                    (input, Some(value))
                } else {
                    do_sep = true;
                    (input, None)
                };

                let (input, item) = item.parse_mut(input)?;

                Ok((input, (sep, item)))
            },
            func: move |mut acc, (sep, item)| {
                if let Some(sep) = sep {
                    acc = sep_func(acc, sep);
                }
                item_func(acc, item)
            },
            value: acc,
            range,
        }
        .parse_once(input)
    }
}

impl<R, A, Fs, Fp, S, P, I, E> ParseMut<I, E> for SeparatedFoldRange<R, A, S, P, Fs, Fp>
where
    A: Clone,
    I: InputEq + Clone,
    E: ParseError<I>,
    S: ParseMut<I, E>,
    P: ParseMut<I, E>,
    Fs: FnMut(A, S::Output) -> A,
    Fp: FnMut(A, P::Output) -> A,
    R: RangeBounds<usize> + Clone,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        SeparatedFoldRange {
            sep: self.sep.by_mut(),
            item: self.item.by_mut(),
            sep_func: &mut self.sep_func,
            item_func: &mut self.item_func,
            acc: self.acc.clone(),
            range: self.range.clone(),
        }
        .parse_once(input)
    }
}

impl<R, A, Fs, Fp, S, P, I, E> Parse<I, E> for SeparatedFoldRange<R, A, S, P, Fs, Fp>
where
    A: Clone,
    I: InputEq + Clone,
    E: ParseError<I>,
    S: Parse<I, E>,
    P: Parse<I, E>,
    Fs: Fn(A, S::Output) -> A,
    Fp: Fn(A, P::Output) -> A,
    R: RangeBounds<usize> + Clone,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        SeparatedFoldRange {
            sep: self.sep.by_ref(),
            item: self.item.by_ref(),
            sep_func: &self.sep_func,
            item_func: &self.item_func,
            acc: self.acc.clone(),
            range: self.range.clone(),
        }
        .parse_once(input)
    }
}

impl<R, S, P, C, I, E, A> ParseOnce<I, E> for SeparatedRange<R, S, P, C>
where
    I: InputEq + Clone,
    E: ParseError<I>,
    S: ParseMut<I, E>,
    P: ParseMut<I, E>,
    C: FnOnce() -> A,
    A: Extend<P::Output>,
    R: RangeBounds<usize>,
{
    type Output = A;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        SeparatedFoldRange {
            sep: self.sep,
            item: self.item,
            sep_func: |acc: A, _: S::Output| acc,
            item_func: |mut collection: A, value: P::Output| {
                collection.extend(Some(value));
                collection
            },
            acc: (self.collection)(),
            range: self.range,
        }
        .parse_once(input)
    }
}

impl<R, S, P, C, I, E, A> ParseMut<I, E> for SeparatedRange<R, S, P, C>
where
    I: InputEq + Clone,
    E: ParseError<I>,
    S: ParseMut<I, E>,
    P: ParseMut<I, E>,
    C: FnMut() -> A,
    A: Extend<P::Output>,
    R: RangeBounds<usize> + Clone,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        SeparatedRange {
            collection: &mut self.collection,
            item: self.item.by_mut(),
            sep: self.sep.by_mut(),
            range: self.range.clone(),
        }
        .parse_once(input)
    }
}

impl<R, S, P, C, I, E, A> Parse<I, E> for SeparatedRange<R, S, P, C>
where
    I: InputEq + Clone,
    E: ParseError<I>,
    S: Parse<I, E>,
    P: Parse<I, E>,
    C: Fn() -> A,
    A: Extend<P::Output>,
    R: RangeBounds<usize> + Clone,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        SeparatedRange {
            collection: &self.collection,
            item: self.item.by_ref(),
            sep: self.sep.by_ref(),
            range: self.range.clone(),
        }
        .parse_once(input)
    }
}

#[test]
fn test() {
    use crate::tag::Tag;

    let _: crate::prelude::PResult<_, _, ()> = separated(.., Tag(','), Tag('a')).parse_once("");

    let mut parser = SeparatedFoldRange {
        item: Tag('a'),
        sep: Tag(','),
        range: 2..=3,
        item_func: |(i, s), _| (i + 1, s),
        sep_func: |(i, s), _| (i, s + 1),
        acc: (0, 0),
    };

    ParseOnce::<_, ()>::parse_once(parser.by_mut(), "a,").unwrap_err();

    let (input, (i, s)) = ParseOnce::<_, ()>::parse_once(parser.by_mut(), "a,a,").unwrap();

    assert_eq!(input, ",");
    assert_eq!(i, 2);
    assert_eq!(s, 1);

    let (input, (i, s)) = ParseOnce::<_, ()>::parse_once(parser.by_mut(), "a,a,a,a,a").unwrap();

    assert_eq!(input, ",a,a");
    assert_eq!(i, 3);
    assert_eq!(s, 2);
}
