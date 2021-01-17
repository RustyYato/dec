#![cfg_attr(feature = "nightly", feature(unsized_locals, try_trait))]
#![forbid(unsafe_code)]
#![no_std]

#[cfg(all(not(test), feature = "alloc"))]
extern crate alloc as std;

use imp::*;

extern crate core;

mod imp {
    pub mod tag;

    pub(crate) mod all;
    pub(crate) mod any;
    pub(crate) mod fold;
    pub(crate) mod lift;
    pub(crate) mod map;
    pub(crate) mod not;
    pub(crate) mod optional;
    #[cfg(all(feature = "rayon", feature = "std"))]
    pub(crate) mod par_any;
    pub(crate) mod recognize;
    pub(crate) mod separated;
    pub(crate) mod seq;
    pub(crate) mod skip;
    pub(crate) mod try_fold;
    pub(crate) mod try_separated;
    pub(crate) mod until;
    pub(crate) mod value;
    pub(crate) mod verify;

    mod ranged;
    pub(crate) mod utils;
}

#[cfg(feature = "alloc")]
pub mod punctuated;

pub use imp::tag;

pub mod branch {
    use crate::*;

    pub use any::Any;
    #[cfg(all(feature = "rayon", feature = "std"))]
    pub use par_any::ParAny;

    pub fn any<P>(tuple: P) -> Any<P> { Any(tuple) }
    #[cfg(all(feature = "rayon", feature = "std"))]
    pub fn par_any<P>(tuple: P) -> ParAny<P> { ParAny(tuple) }
}

#[forbid(unsafe_code)]
pub mod seq {
    use crate::*;

    use core::ops::{RangeBounds, RangeFrom, RangeFull};

    pub use all::All;
    pub use fold::{fold, fold_exact, fold_range, Fold, FoldRange, Range};
    pub use imp::seq::{Fst, Mid, Snd};
    #[cfg(feature = "alloc")]
    pub use separated::separated;
    pub use separated::{iseparated, SeparatedFold, SeparatedFoldRange, SeparatedRange};
    pub use try_fold::{try_fold, TryFold};
    pub use try_separated::{try_separated_fold, TrySeparatedFold};
    pub use until::{FoldUntil, Until};

    pub fn all<P>(tuple: P) -> All<P> { All(tuple) }

    pub fn fst<A, B>(first: A, second: B) -> Fst<A, B> { Fst(first, second) }

    pub fn snd<A, B>(first: A, second: B) -> Snd<A, B> { Snd(first, second) }

    pub fn mid<A, B, C>(first: A, second: B, third: C) -> Mid<A, B, C> { Mid(first, second, third) }

    #[cfg(feature = "alloc")]
    pub fn until<O, S, P>(stop: S, parser: P) -> Until<P, S, impl Fn() -> Vec<O> + Copy> {
        Until {
            collection: Vec::new,
            parser,
            stop,
        }
    }

    pub fn iuntil<S, P>(stop: S, parser: P) -> Until<P, S, impl Fn() -> crate::Ignore + Copy> {
        Until {
            collection: crate::Ignore,
            parser,
            stop,
        }
    }

    #[cfg(feature = "alloc")]
    pub fn many0<P, O>(parser: P) -> Range<P, RangeFull, impl Copy + Fn() -> Vec<O>> {
        Range {
            parser,
            range: ..,
            collection: Vec::new,
        }
    }

    #[cfg(feature = "alloc")]
    pub fn many1<P, O>(parser: P) -> Range<P, RangeFrom<usize>, impl Copy + Fn() -> Vec<O>> {
        Range {
            parser,
            range: 1..,
            collection: Vec::new,
        }
    }

    #[cfg(feature = "alloc")]
    pub fn range<P, O, R: RangeBounds<usize>>(range: R, parser: P) -> Range<P, R, impl Copy + Fn() -> Vec<O>> {
        Range {
            range,
            parser,
            collection: Vec::new,
        }
    }

    pub fn imany0<P>(parser: P) -> Range<P, RangeFull, impl Copy + Fn() -> crate::Ignore> {
        Range {
            parser,
            range: ..,
            collection: crate::Ignore,
        }
    }

    pub fn imany1<P>(parser: P) -> Range<P, RangeFrom<usize>, impl Copy + Fn() -> crate::Ignore> {
        Range {
            parser,
            range: 1..,
            collection: crate::Ignore,
        }
    }

    pub fn irange<P, R: RangeBounds<usize>>(range: R, parser: P) -> Range<P, R, impl Copy + Fn() -> crate::Ignore> {
        Range {
            range,
            parser,
            collection: crate::Ignore,
        }
    }
}

#[forbid(unsafe_code)]
pub mod map {
    use crate::*;

    pub use imp::map::{Map, MapErr, Then, TryMap, TryThen};
    pub use value::Value;

    pub fn value<V, P>(value: V, parser: P) -> Value<V, P> { Value(value, parser) }

    pub fn map<P, F>(parser: P, f: F) -> Map<P, F> { Map(parser, f) }

    pub fn map_err<E, P, F>(parser: P, f: F) -> MapErr<P, F, E> { MapErr(parser, f, core::marker::PhantomData) }

    pub fn then<P, F>(parser: P, f: F) -> Then<P, F> { Then(parser, f) }

    pub fn try_map<P, F>(parser: P, f: F) -> TryMap<P, F> { TryMap(parser, f) }

    pub fn try_then<P, F>(parser: P, f: F) -> TryThen<P, F> { TryThen(parser, f) }
}

#[forbid(unsafe_code)]
pub mod combinator {
    use crate::*;

    pub use lift::{Lift, Lower};
    pub use not::Not;
    pub use optional::Opt;
    pub use recognize::Recognize;
    pub use verify::Verify;

    pub fn lift<P>(parser: P) -> Lift<P> { Lift(parser) }

    pub fn lower<P>(parser: P) -> Lower<P> { Lower(parser) }

    pub fn not<P>(parser: P) -> Not<P> { Not(parser) }

    pub fn opt<P>(parser: P) -> Opt<P> { Opt(parser) }

    pub fn recognize<P>(parser: P) -> Recognize<P> { Recognize(parser) }

    pub fn verify<P, F>(parser: P, verify: F) -> Verify<P, F> { Verify(parser, verify) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ignore();

impl<A> Extend<A> for Ignore {
    fn extend<T: IntoIterator<Item = A>>(&mut self, _: T) {}
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Count(pub usize);

impl Count {
    pub fn new() -> Self { Self(0) }

    pub fn start_at(at: usize) -> impl Fn() -> Self { move || Self(at) }
}

impl<A> Extend<A> for Count {
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        self.0 = iter.into_iter().count().saturating_add(self.0);
    }
}
