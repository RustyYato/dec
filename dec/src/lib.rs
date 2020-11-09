#![cfg_attr(feature = "nightly", feature(unsized_locals, try_trait))]

#[path = "map.rs"]
mod _map;
#[path = "seq.rs"]
mod _seq;
mod all;
mod any;
mod compare;
mod context;
mod fold;
mod lift;
mod multi;
mod not;
mod optional;
mod recognize;
mod separated;
mod skip;
mod try_fold;
mod until;
mod value;
mod verify;

pub mod bits;
pub mod ext;
pub mod punctuated;

pub mod error;
pub mod iter;
pub mod traits;

pub mod tag;

pub mod prelude {
    use crate::*;

    pub use error::{Error, ErrorKind, PResult};
    pub use traits::{Parse, ParseMut, ParseOnce, ParserRef};
}

pub mod branch {
    use crate::*;

    pub use any::Any;

    pub fn any<P>(tuple: P) -> Any<P> {
        Any(tuple)
    }
}

pub mod seq {
    use crate::*;

    pub use _seq::{Fst, Mid, Snd};
    pub use all::All;
    pub use fold::{fold, fold_exact, fold_range, Fold, FoldRange};
    pub use multi::{imany0, imany1, irange, many0, many1, range, Range};
    pub use separated::{iseparated, separated, SeparatedFoldRange, SeparatedRange};
    pub use until::{FoldUntil, Until};

    pub fn all<P>(tuple: P) -> All<P> {
        All(tuple)
    }

    pub fn fst<A, B>(first: A, second: B) -> Fst<A, B> {
        Fst(first, second)
    }

    pub fn snd<A, B>(first: A, second: B) -> Snd<A, B> {
        Snd(first, second)
    }

    pub fn mid<A, B, C>(first: A, second: B, third: C) -> Mid<A, B, C> {
        Mid(first, second, third)
    }

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
}

pub mod map {
    use crate::*;

    pub use _map::{Map, MapErr, Then, TryMap, TryThen};
    pub use value::Value;

    pub fn value<V, P>(value: V, parser: P) -> Value<V, P> {
        Value(value, parser)
    }

    pub fn map<P, F>(parser: P, f: F) -> Map<P, F> {
        Map(parser, f)
    }

    pub fn map_err<E, P, F>(parser: P, f: F) -> MapErr<P, F, E> {
        MapErr(parser, f, core::marker::PhantomData)
    }

    pub fn then<P, F>(parser: P, f: F) -> Then<P, F> {
        Then(parser, f)
    }

    pub fn try_map<P, F>(parser: P, f: F) -> TryMap<P, F> {
        TryMap(parser, f)
    }

    pub fn try_then<P, F>(parser: P, f: F) -> TryThen<P, F> {
        TryThen(parser, f)
    }
}

pub mod combinator {
    use crate::error::ErrorKind;
    use crate::*;

    pub use context::{AppendError, Context};
    pub use lift::{Lift, Lower};
    pub use not::Not;
    pub use optional::Opt;
    pub use recognize::Recognize;
    pub use verify::Verify;

    pub fn context<P>(ctx: &'static str, parser: P) -> Context<P> {
        Context(ctx, parser)
    }

    pub fn append_error<P>(kind: ErrorKind, parser: P) -> AppendError<P> {
        AppendError(kind, parser)
    }

    pub fn lift<P>(parser: P) -> Lift<P> {
        Lift(parser)
    }

    pub fn lower<P>(parser: P) -> Lower<P> {
        Lower(parser)
    }

    pub fn not<P>(parser: P) -> Not<P> {
        Not(parser)
    }

    pub fn opt<P>(parser: P) -> Opt<P> {
        Opt(parser)
    }

    pub fn recognize<P>(parser: P) -> Recognize<P> {
        Recognize(parser)
    }

    pub fn verify<P, F>(parser: P, verify: F) -> Verify<P, F> {
        Verify(parser, verify)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ignore();

impl<A> Extend<A> for Ignore {
    fn extend<T: IntoIterator<Item = A>>(&mut self, _: T) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Count(pub usize);

impl Count {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn start_at(at: usize) -> impl Fn() -> Self {
        move || Self(at)
    }
}

impl<A> Extend<A> for Count {
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        self.0 = iter.into_iter().count().saturating_add(self.0);
    }
}

fn extend<C: Extend<V>, V>(mut collection: C, value: V) -> C {
    collection.extend(Some(value));
    collection
}
