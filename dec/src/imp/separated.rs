use core::ops::RangeBounds;

use dec_core::{
    error::{Error, ErrorKind, PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

use crate::imp::{map::Map, ranged::Ranged, try_separated::TrySeparatedFold};

#[cfg(feature = "alloc")]
pub fn separated<O, P, S, R: RangeBounds<usize>>(
    range: R,
    sep: S,
    item: P,
) -> SeparatedRange<P, S, R, impl Copy + Fn() -> std::vec::Vec<O>> {
    SeparatedRange {
        range,
        sep,
        item,
        mk_collection: std::vec::Vec::new,
    }
}

pub fn iseparated<P, S, R: RangeBounds<usize>>(
    range: R,
    sep: S,
    item: P,
) -> SeparatedRange<P, S, R, impl Copy + Fn() -> crate::Ignore> {
    SeparatedRange {
        range,
        sep,
        item,
        mk_collection: crate::Ignore,
    }
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SeparatedFold<P, S, Fp, Fs, MkA> {
    pub item: P,
    pub sep: S,
    pub item_func: Fp,
    pub sep_func: Fs,
    pub mk_acc: MkA,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SeparatedFoldRange<P, S, Fp, Fs, MkA, R> {
    pub item: P,
    pub sep: S,
    pub item_func: Fp,
    pub sep_func: Fs,
    pub mk_acc: MkA,
    pub range: R,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SeparatedRange<P, S, R, MkC> {
    pub item: P,
    pub sep: S,
    pub range: R,
    pub mk_collection: MkC,
}

impl<I, E, P, S, Fp, Fs, MkA, A, Fail> ParseOnce<I, E, Fail> for SeparatedFold<P, S, Fp, Fs, MkA>
where
    E: ParseError<I>,
    S: ParseMut<I, E, Fail>,
    P: ParseMut<I, E, Fail>,
    Fs: FnMut(A, S::Output) -> A,
    Fp: FnMut(A, P::Output) -> A,
    MkA: FnOnce() -> A,
{
    type Output = A;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            item_func,
            sep_func,
            mk_acc,
        } = self;

        TrySeparatedFold {
            item,
            sep,
            item_func: crate::utils::ok(item_func),
            sep_func: crate::utils::ok(sep_func),
            mk_acc,
        }
        .parse_once(input)
        .map(crate::utils::unwrap_absurd)
    }
}

impl<I, E, P, S, Fp, Fs, MkA, A, Fail> ParseMut<I, E, Fail> for SeparatedFold<P, S, Fp, Fs, MkA>
where
    E: ParseError<I>,
    S: ParseMut<I, E, Fail>,
    P: ParseMut<I, E, Fail>,
    Fs: FnMut(A, S::Output) -> A,
    Fp: FnMut(A, P::Output) -> A,
    MkA: FnMut() -> A,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            item_func,
            sep_func,
            mk_acc,
        } = self;

        SeparatedFold {
            item: item.by_mut(),
            sep: sep.by_mut(),
            item_func,
            sep_func,
            mk_acc,
        }
        .parse_once(input)
    }
}

impl<I, E, P, S, Fp, Fs, MkA, A, Fail> Parse<I, E, Fail> for SeparatedFold<P, S, Fp, Fs, MkA>
where
    E: ParseError<I>,
    S: Parse<I, E, Fail>,
    P: Parse<I, E, Fail>,
    Fs: Fn(A, S::Output) -> A,
    Fp: Fn(A, P::Output) -> A,
    MkA: Fn() -> A,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            item_func,
            sep_func,
            mk_acc,
        } = self;

        SeparatedFold {
            item: item.by_ref(),
            sep: sep.by_ref(),
            item_func,
            sep_func,
            mk_acc,
        }
        .parse_once(input)
    }
}

impl<I, E, P, S, Fp, Fs, MkA, A, R, Fail> ParseOnce<I, E, Fail> for SeparatedFoldRange<P, S, Fp, Fs, MkA, R>
where
    E: ParseError<I>,
    P: ParseMut<I, E, Fail>,
    S: ParseMut<I, E, Fail>,
    Fp: FnMut(A, P::Output) -> A,
    Fs: FnMut(A, S::Output) -> A,
    MkA: FnOnce() -> A,
    R: RangeBounds<usize>,
{
    type Output = A;

    fn parse_once(self, mut input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            mut item,
            mut sep,
            mut item_func,
            mut sep_func,
            mk_acc,
            range,
        } = self;

        let mut acc = mk_acc();

        let (mut prefix, mut tail) = match Ranged::new(range) {
            Some(range) => range.split(),
            None => return Ok((input, acc)),
        };

        if let Some(()) = prefix.next() {
            let (next_input, next_acc) = TrySeparatedFold {
                item: item.by_mut(),
                sep: sep.by_mut(),
                item_func: crate::utils::step(&mut item_func, prefix),
                sep_func: crate::utils::ok(&mut sep_func),
                mk_acc: crate::utils::value(acc),
            }
            .parse_once(input)?;

            input = next_input;
            acc = match next_acc {
                // ended because index reached range start
                Err(acc) => acc,
                // ended because parser failed
                Ok(_) => return Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::RangeStart))),
            };
        }

        if tail.next().is_some() {
            Map(
                TrySeparatedFold {
                    item,
                    sep,
                    item_func: crate::utils::step(item_func, tail),
                    sep_func: crate::utils::ok(sep_func),
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

impl<I, E, P, S, Fp, Fs, MkA, A, R, Fail> ParseMut<I, E, Fail> for SeparatedFoldRange<P, S, Fp, Fs, MkA, R>
where
    E: ParseError<I>,
    P: ParseMut<I, E, Fail>,
    S: ParseMut<I, E, Fail>,
    Fp: FnMut(A, P::Output) -> A,
    Fs: FnMut(A, S::Output) -> A,
    MkA: FnMut() -> A,
    R: RangeBounds<usize> + Clone,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            item_func,
            sep_func,
            mk_acc,
            range,
        } = self;

        SeparatedFoldRange {
            item: item.by_mut(),
            sep: sep.by_mut(),
            item_func,
            sep_func,
            mk_acc,
            range: range.clone(),
        }
        .parse_once(input)
    }
}

impl<I, E, P, S, Fp, Fs, MkA, A, R, Fail> Parse<I, E, Fail> for SeparatedFoldRange<P, S, Fp, Fs, MkA, R>
where
    E: ParseError<I>,
    P: Parse<I, E, Fail>,
    S: Parse<I, E, Fail>,
    Fp: Fn(A, P::Output) -> A,
    Fs: Fn(A, S::Output) -> A,
    MkA: Fn() -> A,
    R: RangeBounds<usize> + Clone,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            item_func,
            sep_func,
            mk_acc,
            range,
        } = self;

        SeparatedFoldRange {
            item: item.by_ref(),
            sep: sep.by_ref(),
            item_func,
            sep_func,
            mk_acc,
            range: range.clone(),
        }
        .parse_once(input)
    }
}

impl<I, E, P, S, R, MkC, C, Fail> ParseOnce<I, E, Fail> for SeparatedRange<P, S, R, MkC>
where
    E: ParseError<I>,
    P: ParseMut<I, E, Fail>,
    S: ParseMut<I, E, Fail>,
    R: RangeBounds<usize>,
    MkC: FnOnce() -> C,
    C: Extend<P::Output>,
{
    type Output = C;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            range,
            mk_collection,
        } = self;

        SeparatedFoldRange {
            item,
            sep,
            item_func: crate::utils::extend,
            sep_func: crate::utils::fst,
            mk_acc: mk_collection,
            range,
        }
        .parse_once(input)
    }
}

impl<I, E, P, S, R, MkC, C, Fail> ParseMut<I, E, Fail> for SeparatedRange<P, S, R, MkC>
where
    E: ParseError<I>,
    P: ParseMut<I, E, Fail>,
    S: ParseMut<I, E, Fail>,
    R: RangeBounds<usize> + Clone,
    MkC: FnMut() -> C,
    C: Extend<P::Output>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            range,
            mk_collection,
        } = self;

        SeparatedRange {
            item: item.by_mut(),
            sep: sep.by_mut(),
            mk_collection,
            range: range.clone(),
        }
        .parse_once(input)
    }
}

impl<I, E, P, S, R, MkC, C, Fail> Parse<I, E, Fail> for SeparatedRange<P, S, R, MkC>
where
    E: ParseError<I>,
    P: Parse<I, E, Fail>,
    S: Parse<I, E, Fail>,
    R: RangeBounds<usize> + Clone,
    MkC: Fn() -> C,
    C: Extend<P::Output>,
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        let Self {
            item,
            sep,
            range,
            mk_collection,
        } = self;

        SeparatedRange {
            item: item.by_ref(),
            sep: sep.by_ref(),
            mk_collection,
            range: range.clone(),
        }
        .parse_once(input)
    }
}

// #[test]
// fn test() {
//     use crate::tag::Tag;

//     let _: crate::prelude::PResult<_, _, ()> = separated(.., Tag(','), Tag('a')).parse_once("");

//     let mut parser = SeparatedFoldRange {
//         item: Tag('a'),
//         sep: Tag(','),
//         range: 2..=3,
//         item_func: |(i, s), _| (i + 1, s),
//         sep_func: |(i, s), _| (i, s + 1),
//         mk_acc: || (0, 0),
//     };

//     ParseOnce::<_, ()>::parse_once(parser.by_mut(), "a,").unwrap_err();

//     let (input, (i, s)) = ParseOnce::<_, ()>::parse_once(parser.by_mut(), "a,a,").unwrap();

//     assert_eq!(input, ",");
//     assert_eq!(i, 2);
//     assert_eq!(s, 1);

//     let (input, (i, s)) = ParseOnce::<_, ()>::parse_once(parser.by_mut(), "a,a,a,a,a").unwrap();

//     assert_eq!(input, ",a,a");
//     assert_eq!(i, 3);
//     assert_eq!(s, 2);
// }
