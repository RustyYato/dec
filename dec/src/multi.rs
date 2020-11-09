use crate::error::*;
use crate::traits::*;

use crate::ext::{Mut, Ref};

use std::ops::{Bound, RangeBounds};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Fold<P, V, F> {
    pub parser: P,
    pub value: V,
    pub func: F,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
struct FoldTo<P, V, F> {
    parser: P,
    value: V,
    func: F,
    max: usize,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FoldRange<P, V, F, R> {
    pub parser: P,
    pub value: V,
    pub func: F,
    pub range: R,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FoldN<P, V, F> {
    pub parser: P,
    pub value: V,
    pub func: F,
    pub count: usize,
}

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Range<R, P, F>(pub R, pub P, pub F);

pub fn fold<P, V, F>(parser: P, value: V, func: F) -> Fold<P, V, F> {
    Fold {
        parser,
        value,
        func,
    }
}

pub fn many0<P, O>(parser: P) -> Range<std::ops::RangeFull, P, impl Copy + Fn() -> Vec<O>> {
    Range(.., parser, Vec::new)
}

pub fn many1<P, O>(parser: P) -> Range<std::ops::RangeFrom<usize>, P, impl Copy + Fn() -> Vec<O>> {
    Range(1.., parser, Vec::new)
}

pub fn range<P, O>(
    range: impl RangeBounds<usize>,
    parser: P,
) -> Range<(Bound<usize>, Bound<usize>), P, impl Copy + Fn() -> Vec<O>> {
    Range(
        (
            copy_bound(range.start_bound()),
            copy_bound(range.end_bound()),
        ),
        parser,
        Vec::new,
    )
}

pub fn imany0<P>(parser: P) -> Range<std::ops::RangeFull, P, impl Copy + Fn() -> crate::Ignore> {
    Range(.., parser, crate::Ignore)
}

pub fn imany1<P>(
    parser: P,
) -> Range<std::ops::RangeFrom<usize>, P, impl Copy + Fn() -> crate::Ignore> {
    Range(1.., parser, crate::Ignore)
}

pub fn irange<P>(
    range: impl RangeBounds<usize>,
    parser: P,
) -> Range<(Bound<usize>, Bound<usize>), P, impl Copy + Fn() -> crate::Ignore> {
    Range(
        (
            copy_bound(range.start_bound()),
            copy_bound(range.end_bound()),
        ),
        parser,
        crate::Ignore,
    )
}

fn copy_bound(b: Bound<&usize>) -> Bound<usize> {
    match b {
        Bound::Excluded(&x) => Bound::Excluded(x),
        Bound::Included(&x) => Bound::Included(x),
        Bound::Unbounded => Bound::Unbounded,
    }
}

pub(crate) fn parse_bounds(
    start: Bound<&usize>,
    end: Bound<&usize>,
) -> (Option<usize>, Option<usize>) {
    let start = match start {
        Bound::Unbounded => None,
        Bound::Excluded(&usize::MAX) => panic!("Overflowed lower bound"),
        Bound::Excluded(&x) => Some(x.wrapping_add(1)),
        Bound::Included(&x) => Some(x),
    };
    let end = match end {
        Bound::Unbounded => None,
        Bound::Excluded(&x) => Some(x.saturating_sub(1)),
        Bound::Included(&x) => Some(x),
    };

    if let (Some(start), Some(end)) = (start, end) {
        assert!(start <= end);
    }

    (start, end)
}

impl<P: ParseMut<I, E>, F: FnMut(V, P::Output) -> V, V, I: InputEq + Clone, E: ParseError<I>>
    ParseOnce<I, E> for Fold<P, V, F>
{
    type Output = V;

    fn parse_once(self, mut input: I) -> PResult<I, Self::Output, E> {
        let Self {
            mut parser,
            mut value,
            mut func,
        } = self;
        loop {
            match parser.parse_mut(input.clone()) {
                Ok((i, out)) => {
                    input = i;
                    value = func(value, out);
                }
                Err(Error::Error(_)) => return Ok((input, value)),
                Err(err) => return Err(err),
            }
        }
    }
}

impl<
        P: ParseMut<I, E>,
        F: FnMut(V, P::Output) -> V,
        V: Clone,
        I: InputEq + Clone,
        E: ParseError<I>,
    > ParseMut<I, E> for Fold<P, V, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        Fold {
            parser: Mut(&mut self.parser),
            value: self.value.clone(),
            func: &mut self.func,
        }
        .parse_once(input)
    }
}

impl<P: Parse<I, E>, F: Fn(V, P::Output) -> V, V: Clone, I: InputEq + Clone, E: ParseError<I>>
    Parse<I, E> for Fold<P, V, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        Fold {
            parser: Ref(&self.parser),
            value: self.value.clone(),
            func: &self.func,
        }
        .parse_once(input)
    }
}

impl<P: ParseMut<I, E>, F: FnMut(V, P::Output) -> V, V, I: Clone, E: ParseError<I>> ParseOnce<I, E>
    for FoldTo<P, V, F>
{
    type Output = V;

    fn parse_once(self, mut input: I) -> PResult<I, Self::Output, E> {
        let Self {
            mut parser,
            mut value,
            mut func,
            max,
        } = self;
        for _ in 0..max {
            match parser.parse_mut(input.clone()) {
                Ok((i, out)) => {
                    input = i;
                    value = func(value, out);
                }
                Err(Error::Error(_)) => break,
                Err(err) => return Err(err),
            }
        }
        Ok((input, value))
    }
}

fn fold_range_impl<P, V, F, I, E>(
    start: Bound<&usize>,
    end: Bound<&usize>,
    fold: Fold<P, V, F>,
    mut input: I,
) -> PResult<I, V, E>
where
    I: InputEq + Clone,
    E: ParseError<I>,
    P: ParseMut<I, E>,
    F: FnMut(V, P::Output) -> V,
{
    let Fold {
        mut parser,
        mut func,
        mut value,
    } = fold;
    let (start, end) = parse_bounds(start, end);

    if let Some(start) = start {
        for _ in 0..start {
            let (i, v) = parser.parse_mut(input)?;
            input = i;
            value = func(value, v);
        }
    }

    if let Some(end) = end {
        FoldTo {
            parser,
            func,
            value,
            max: end - start.unwrap_or(0),
        }
        .parse_once(input)
    } else {
        Fold {
            parser,
            func,
            value,
        }
        .parse_once(input)
    }
}

impl<
        R: RangeBounds<usize>,
        P: ParseMut<I, E>,
        F: FnMut(V, P::Output) -> V,
        V,
        I: InputEq + Clone,
        E: ParseError<I>,
    > ParseOnce<I, E> for FoldRange<P, V, F, R>
{
    type Output = V;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        fold_range_impl(
            self.range.start_bound(),
            self.range.end_bound(),
            Fold {
                parser: self.parser,
                func: self.func,
                value: self.value,
            },
            input,
        )
    }
}

impl<
        R: RangeBounds<usize>,
        P: ParseMut<I, E>,
        F: FnMut(V, P::Output) -> V,
        V: Clone,
        I: InputEq + Clone,
        E: ParseError<I>,
    > ParseMut<I, E> for FoldRange<P, V, F, R>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        fold_range_impl(
            self.range.start_bound(),
            self.range.end_bound(),
            Fold {
                parser: Mut(&mut self.parser),
                func: &mut self.func,
                value: self.value.clone(),
            },
            input,
        )
    }
}

impl<
        R: RangeBounds<usize>,
        P: Parse<I, E>,
        F: Fn(V, P::Output) -> V,
        V: Clone,
        I: InputEq + Clone,
        E: ParseError<I>,
    > Parse<I, E> for FoldRange<P, V, F, R>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        fold_range_impl(
            self.range.start_bound(),
            self.range.end_bound(),
            Fold {
                parser: Ref(&self.parser),
                func: &self.func,
                value: self.value.clone(),
            },
            input,
        )
    }
}

impl<P: ParseMut<I, E>, F: FnMut(V, P::Output) -> V, V, I, E: ParseError<I>> ParseOnce<I, E>
    for FoldN<P, V, F>
{
    type Output = V;

    fn parse_once(self, mut input: I) -> PResult<I, Self::Output, E> {
        let Self {
            mut parser,
            mut value,
            mut func,
            count,
        } = self;
        for _ in 0..count {
            let (i, out) = parser.parse_mut(input)?;
            input = i;
            value = func(value, out);
        }
        Ok((input, value))
    }
}

impl<P: ParseMut<I, E>, F: FnMut(V, P::Output) -> V, V: Clone, I, E: ParseError<I>> ParseMut<I, E>
    for FoldN<P, V, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        FoldN {
            parser: Mut(&mut self.parser),
            value: self.value.clone(),
            func: &mut self.func,
            count: self.count,
        }
        .parse_once(input)
    }
}

impl<P: Parse<I, E>, F: Fn(V, P::Output) -> V, V: Clone, I, E: ParseError<I>> Parse<I, E>
    for FoldN<P, V, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        FoldN {
            parser: Ref(&self.parser),
            value: self.value.clone(),
            func: &self.func,
            count: self.count,
        }
        .parse_once(input)
    }
}

fn extend<C: Extend<T>, T>(mut vec: C, item: T) -> C {
    vec.extend(Some(item));
    vec
}

fn range_impl<P, I, C, E>(
    start: Bound<&usize>,
    end: Bound<&usize>,
    mut parser: P,
    collection: C,
    input: I,
) -> PResult<I, C, E>
where
    I: InputEq + Clone,
    P: ParseMut<I, E>,
    E: ParseError<I>,
    C: Extend<P::Output>,
{
    let (start, end) = parse_bounds(start, end);

    let ((input, collection), start) = if let Some(start) = start {
        (
            FoldN {
                parser: Mut(&mut parser),
                value: collection,
                count: start,
                func: extend,
            }
            .parse_once(input)?,
            start,
        )
    } else {
        ((input, collection), 0)
    };

    if let Some(end) = end {
        FoldTo {
            parser,
            value: collection,
            max: end - start,
            func: extend,
        }
        .parse_once(input)
    } else {
        fold(parser, collection, extend).parse_once(input)
    }
}

impl<
        R: RangeBounds<usize>,
        F: FnOnce() -> C,
        C: Extend<P::Output>,
        P: ParseMut<I, E>,
        I: InputEq + Clone,
        E: ParseError<I>,
    > ParseOnce<I, E> for Range<R, P, F>
{
    type Output = C;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        range_impl(
            self.0.start_bound(),
            self.0.end_bound(),
            self.1,
            (self.2)(),
            input,
        )
    }
}

impl<
        R: RangeBounds<usize>,
        F: FnMut() -> C,
        C: Extend<P::Output>,
        P: ParseMut<I, E>,
        I: InputEq + Clone,
        E: ParseError<I>,
    > ParseMut<I, E> for Range<R, P, F>
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        range_impl(
            self.0.start_bound(),
            self.0.end_bound(),
            Mut(&mut self.1),
            (self.2)(),
            input,
        )
    }
}

impl<
        R: RangeBounds<usize>,
        F: Fn() -> C,
        C: Extend<P::Output>,
        P: Parse<I, E>,
        I: InputEq + Clone,
        E: ParseError<I>,
    > Parse<I, E> for Range<R, P, F>
{
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        range_impl(
            self.0.start_bound(),
            self.0.end_bound(),
            Ref(&self.1),
            (self.2)(),
            input,
        )
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
