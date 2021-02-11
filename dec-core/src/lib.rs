#![deny(unsafe_code)]
#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc as std;

use crate::{
    error::{DefaultError, PResult},
    iter::Iter,
};

#[cfg(feature = "bitvec")]
#[allow(unsafe_code)]
mod bits;

#[forbid(unsafe_code)]
mod context;
#[forbid(unsafe_code)]
pub mod error;
#[forbid(unsafe_code)]
mod ext;
#[forbid(unsafe_code)]
pub mod indexed;
#[forbid(unsafe_code)]
mod iter;
#[forbid(unsafe_code)]
mod iter_input;
#[allow(unsafe_code)]
mod tag;

pub use context::{AppendError, Context};
use error::CaptureInput;
pub use ext::{Mut, Ref};
pub use iter_input::IterInput;
pub use tag::{AnyOf, NoneOf};

impl<T: ?Sized> ParseExt for T {}
pub trait ParseExt {
    fn with_context(self, ctx: &'static str) -> Context<Self>
    where
        Self: Sized,
    {
        Context(ctx, self)
    }

    fn append_error(self, kind: crate::error::ErrorKind) -> AppendError<Self>
    where
        Self: Sized,
    {
        AppendError(kind, self)
    }

    fn by_mut(&mut self) -> crate::ext::Mut<Self> { crate::ext::Mut(self) }

    fn by_ref(&self) -> crate::ext::Ref<Self> { crate::ext::Ref(self) }

    fn piter<I, E, F>(self, input: I) -> Iter<Self, I, E, F>
    where
        Self: Sized,
    {
        Iter::new(self, input)
    }

    #[cfg(feature = "nightly")]
    fn boxed_once<'a, I, E>(self) -> crate::ext::Own<dyn 'a + ParseOnce<I, E, Output = Self::Output>>
    where
        Self: 'a + ParseOnce<I, E> + Sized,
        E: ParseError<I>,
    {
        crate::ext::Own(Box::new(self) as _)
    }

    #[cfg(feature = "nightly")]
    fn boxed_mut<'a, I, E>(self) -> crate::ext::Own<dyn 'a + ParseMut<I, E, Output = Self::Output>>
    where
        Self: 'a + ParseMut<I, E> + Sized,
        E: ParseError<I>,
    {
        crate::ext::Own(Box::new(self) as _)
    }

    #[cfg(feature = "nightly")]
    fn boxed<'a, I, E>(self) -> crate::ext::Own<dyn 'a + Parse<I, E, Output = Self::Output>>
    where
        Self: 'a + Parse<I, E> + Sized,
        E: ParseError<I>,
    {
        crate::ext::Own(Box::new(self) as _)
    }
}

pub trait ParseOnce<I, E = DefaultError<I>, F = core::convert::Infallible> {
    type Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, F>;
}

pub trait ParseMut<I, E = DefaultError<I>, F = core::convert::Infallible>: ParseOnce<I, E, F> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, F>;
}

pub trait Parse<I, E = DefaultError<I>, F = core::convert::Infallible>: ParseMut<I, E, F> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, F>;
}

pub trait ParseTag<T>: Sized {
    type Output;

    fn parse_tag(self, tag: &T) -> PResult<Self, Self::Output, CaptureInput<Self>, core::convert::Infallible>;
}

pub trait Tag<I> {
    type Output;

    fn parse_tag(&self, input: I) -> PResult<I, Self::Output, CaptureInput<I>, core::convert::Infallible>;
}

impl<I: ParseTag<T>, T> Tag<I> for T {
    type Output = I::Output;

    fn parse_tag(&self, input: I) -> PResult<I, Self::Output, CaptureInput<I>, core::convert::Infallible> {
        input.parse_tag(self)
    }
}

pub trait InputEq {
    fn eq(&self, other: &Self) -> bool;
}

pub trait InputSplit: Sized {
    fn len(&self) -> usize;

    fn is_empty(&self) -> bool { self.len() == 0 }

    fn cut(self, at: usize) -> Self;

    fn advance(self, at: usize) -> core::result::Result<Self, Self>;
}

impl<T> InputSplit for &[T] {
    #[inline]
    fn len(&self) -> usize { (**self).len() }

    #[inline]
    fn cut(self, at: usize) -> Self { &self[..at] }

    #[inline]
    fn advance(self, at: usize) -> core::result::Result<Self, Self> {
        match self.get(at..) {
            Some(x) => Ok(x),
            None => Err(self),
        }
    }
}

impl InputSplit for &str {
    #[inline]
    fn len(&self) -> usize { (**self).len() }

    #[inline]
    fn cut(self, at: usize) -> Self { &self[..at] }

    #[inline]
    fn advance(self, at: usize) -> core::result::Result<Self, Self> {
        match self.get(at..) {
            Some(x) => Ok(x),
            None => Err(self),
        }
    }
}

impl<T> InputSplit for &mut [T] {
    #[inline]
    fn len(&self) -> usize { (**self).len() }

    #[inline]
    fn cut(self, at: usize) -> Self { &mut self[..at] }

    #[inline]
    fn advance(self, at: usize) -> core::result::Result<Self, Self> {
        if self.len() >= at {
            Ok(&mut self[at..])
        } else {
            Err(self)
        }
    }
}

impl InputSplit for &mut str {
    #[inline]
    fn len(&self) -> usize { (**self).len() }

    #[inline]
    fn cut(self, at: usize) -> Self { &mut self[..at] }

    #[inline]
    fn advance(self, at: usize) -> core::result::Result<Self, Self> {
        if self.len() >= at && self.is_char_boundary(at) {
            Ok(&mut self[at..])
        } else {
            Err(self)
        }
    }
}

impl<T> InputEq for &[T] {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let s: &[T] = self;
        let o: &[T] = other;

        core::ptr::eq(s, o)
    }
}

impl InputEq for &str {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let s: &str = self;
        let o: &str = other;

        core::ptr::eq(s, o)
    }
}

impl<T> InputEq for &mut [T] {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let s: &[T] = self;
        let o: &[T] = other;

        core::ptr::eq(s, o)
    }
}

impl InputEq for &mut str {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let s: &str = self;
        let o: &str = other;

        core::ptr::eq(s, o)
    }
}
