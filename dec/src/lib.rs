#![cfg_attr(feature = "nightly", feature(unsized_locals))]

#[path = "map.rs"]
mod _map;
#[path = "seq.rs"]
mod _seq;
mod all;
mod any;
mod compare;
mod context;
mod multi;
mod not;
mod optional;
mod recognize;
mod separated;
mod skip;
mod until;
mod value;
mod verify;

pub mod bits;
pub mod ext;

pub mod error;
pub mod iter;
pub mod traits;

pub mod tag;

pub mod prelude {
    use crate::*;

    pub use error::{Error, ErrorKind, Result as PResult};
    pub use traits::{Parse, ParseMut, ParseOnce, ParserRef};
}

pub mod branch {
    use crate::*;

    pub use any::Any;
}

pub mod seq {
    use crate::*;

    pub use _seq::{Fst, Mid, Snd};
    pub use all::All;
    pub use multi::{fold, Fold, FoldN, FoldRange};
    pub use multi::{imany0, imany1, irange, many0, many1, range, Range};
    pub use separated::{iseparated, separated, SeparatedFoldRange, SeparatedRange};
    pub use until::Until;
}

pub mod map {
    use crate::*;

    pub use _map::{Map, MapErr, Then, TryMap, TryThen};
}

pub mod combinator {
    use crate::*;

    pub use context::Context;
    pub use not::Not;
    pub use optional::Opt;
    pub use recognize::Recognize;
    pub use value::Value;
    pub use verify::Verify;
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
