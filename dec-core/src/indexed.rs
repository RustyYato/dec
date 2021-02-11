use crate::{
    error::{CaptureInput, Error, PResult, ParseError},
    InputEq, InputSplit, ParseOnce, ParseTag, Tag,
};

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

    fn advance(mut self, at: usize) -> core::result::Result<Self, Self> {
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

    pub fn pos(&self) -> P { self.pos }

    pub fn inner(&self) -> &I { &self.inner }
}

impl<I: InputSplit, P> Indexed<I, P> {
    pub fn len(&self) -> usize { self.inner.len() }

    pub fn is_empty(&self) -> bool { self.inner.is_empty() }
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

pub fn fix<I: InputSplit, P: Pos, Q: ParseOnce<I, F, T>, T, E: ParseError<Indexed<I, P>>, F: ParseError<I>>(
    parser: Q,
) -> impl ParseOnce<Indexed<I, P>, E, T, Output = Q::Output> {
    move |input: Indexed<I, P>| {
        let len = input.inner.len();
        let pos = input.pos;
        match parser.parse_once(input.inner) {
            Err(Error::Failure(fail)) => Err(Error::Failure(fail)),
            Err(Error::Error(err)) => {
                let (input, kind) = err.into_input_kind();
                let end = input.len();
                let pos = pos.add(len - end);
                Err(Error::Error(E::from_input_kind(Indexed { inner: input, pos }, kind)))
            }
            Ok((input, value)) => {
                let lexeme_len = len - input.len();
                let pos = pos.add(lexeme_len);

                Ok((Indexed { inner: input, pos }, value))
            }
        }
    }
}

impl<I: InputSplit, P: Pos, T: Tag<I>> ParseTag<T> for Indexed<I, P> {
    type Output = T::Output;

    fn parse_tag(self, tag: &T) -> PResult<Self, Self::Output, CaptureInput<Self>, core::convert::Infallible> {
        fix(move |input| tag.parse_tag(input)).parse_once(self)
    }
}

macro_rules! imp_pos {
    ($($type:ty),*) => {$(
        impl Pos for $type {
            #[inline]
            fn add(self, inc: usize) -> Self {
                #[cfg(debug_assertions)]
                use core::convert::TryFrom;
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
