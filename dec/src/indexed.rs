use crate::traits::{Compare, InputEq, InputSplit};
use core::ops::Range as Span;

type DefaultPos = u32;

pub trait Spanned<P> {
    fn span(&self) -> Span<P>;
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indexed<I, P = DefaultPos> {
    inner: I,
    pos: P,
}

impl<I: InputEq, P: PartialEq> InputEq for Indexed<I, P> {
    fn eq(&self, other: &Self) -> bool { self.pos == other.pos && self.inner.eq(&other.inner) }
}

impl<I: InputSplit, P: Pos> InputSplit for Indexed<I, P> {
    fn len(&self) -> usize { self.inner.len() }

    fn cut(mut self, at: usize) -> Self {
        self.inner = self.inner.cut(at);
        self
    }

    fn advance(mut self, at: usize) -> std::result::Result<Self, Self> {
        match self.inner.advance(at) {
            Ok(inner) => {
                self.inner = inner;
                self.pos = self.pos.add(at);
                Ok(self)
            }
            Err(inner) => {
                self.inner = inner;
                Err(self)
            }
        }
    }
}

pub trait Pos: Copy {
    fn add(self, inc: usize) -> Self;
}

impl<I, P: Pos + Default> Indexed<I, P> {
    pub fn new(inner: I) -> Self { Self::with_pos(inner, P::default()) }
}

impl<I, P: Pos> Indexed<I, P> {
    pub fn with_pos(inner: I, pos: P) -> Self { Self { inner, pos } }

    pub fn pos(&self) -> P { self.pos.clone() }

    pub fn inner(&self) -> &I { &self.inner }
}

impl<I: InputSplit, P: Pos> Spanned<P> for Indexed<I, P> {
    fn span(&self) -> Span<P> {
        Span {
            start: self.pos,
            end: self.pos.add(self.inner.len()),
        }
    }
}

impl<I: Spanned<P>, T, P: Pos> Spanned<P> for (I, T) {
    fn span(&self) -> Span<P> { self.0.span() }
}

impl<I: InputSplit, P: Pos, T: Compare<I>> Compare<Indexed<I, P>> for T {
    type Output = T::Output;

    fn compare(&self, mut indexed_input: Indexed<I, P>) -> (Indexed<I, P>, Option<Self::Output>) {
        let len = indexed_input.inner.len();
        let (input, result) = self.compare(indexed_input.inner);
        indexed_input.inner = input;

        match result {
            None => (indexed_input, None),
            Some(value) => {
                let lexeme_len = len - indexed_input.inner.len();
                indexed_input.pos = indexed_input.pos.add(lexeme_len);

                (indexed_input, Some(value))
            }
        }
    }
}

macro_rules! imp_pos {
    ($($type:ty),*) => {$(
        impl Pos for $type {
            fn add(self, inc: usize) -> Self {
                #[cfg(debug_assertions)]
                use std::convert::TryFrom;
                #[cfg(debug_assertions)]
                if let Ok(remaining) = usize::try_from(<$type>::MAX - self) {
                    assert!(
                        remaining >= inc,
                        concat!("tried to add {} to {}, but this would overflow ", stringify!($type)),
                        inc,
                        self
                    );
                }

                self.wrapping_add(inc as $type)
            }
        }
    )*};
}

imp_pos!(u8, u16, u32, u64, u128, usize);
