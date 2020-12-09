use crate::{
    error::{DefaultError, PResult, ParseError},
    iter::Iter,
};

impl<T: ?Sized> ParserRef for T {}
pub trait ParserRef {
    fn with_context(self, ctx: &'static str) -> crate::combinator::Context<Self>
    where
        Self: Sized,
    {
        crate::combinator::Context(ctx, self)
    }

    fn append_error(self, kind: crate::error::ErrorKind) -> crate::combinator::AppendError<Self>
    where
        Self: Sized,
    {
        crate::combinator::AppendError(kind, self)
    }

    fn by_mut(&mut self) -> crate::ext::Mut<Self> { crate::ext::Mut(self) }

    fn by_ref(&self) -> crate::ext::Ref<Self> { crate::ext::Ref(self) }

    fn piter<I, E>(self, input: I) -> Iter<Self, I, E>
    where
        Self: Sized,
    {
        Iter::new(self, input)
    }

    #[cfg(any(doc, feature = "nightly"))]
    fn boxed_once<'a, I, E>(self) -> crate::ext::Own<dyn 'a + ParseOnce<I, E, Output = Self::Output>>
    where
        Self: 'a + ParseOnce<I, E> + Sized,
        E: ParseError<I>,
    {
        crate::ext::Own(Box::new(self) as _)
    }

    #[cfg(any(doc, feature = "nightly"))]
    fn boxed_mut<'a, I, E>(self) -> crate::ext::Own<dyn 'a + ParseMut<I, E, Output = Self::Output>>
    where
        Self: 'a + ParseMut<I, E> + Sized,
        E: ParseError<I>,
    {
        crate::ext::Own(Box::new(self) as _)
    }

    #[cfg(any(doc, feature = "nightly"))]
    fn boxed<'a, I, E>(self) -> crate::ext::Own<dyn 'a + Parse<I, E, Output = Self::Output>>
    where
        Self: 'a + Parse<I, E> + Sized,
        E: ParseError<I>,
    {
        crate::ext::Own(Box::new(self) as _)
    }
}

pub trait ParseOnce<I, E: ParseError<I> = DefaultError<I>> {
    type Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E>;
}

pub trait ParseMut<I, E: ParseError<I> = DefaultError<I>>: ParseOnce<I, E> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E>;
}

pub trait Parse<I, E: ParseError<I> = DefaultError<I>>: ParseMut<I, E> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E>;
}

pub trait Compare<I> {
    type Output;

    fn compare(&self, input: I) -> (I, CompareResult<Self::Output>);
}

pub trait InputEq {
    fn eq(&self, other: &Self) -> bool;
}

pub trait InputSplit: Sized {
    fn len(&self) -> usize;

    fn cut(self, at: usize) -> Self;

    fn advance(self, at: usize) -> std::result::Result<Self, Self>;
}

impl<T> InputSplit for &[T] {
    fn len(&self) -> usize { (**self).len() }

    fn cut(self, at: usize) -> Self { &self[..at] }

    fn advance(self, at: usize) -> std::result::Result<Self, Self> {
        match self.get(at..) {
            Some(x) => Ok(x),
            None => Err(self),
        }
    }
}

impl InputSplit for &str {
    fn len(&self) -> usize { (**self).len() }

    fn cut(self, at: usize) -> Self { &self[..at] }

    fn advance(self, at: usize) -> std::result::Result<Self, Self> {
        match self.get(at..) {
            Some(x) => Ok(x),
            None => Err(self),
        }
    }
}

impl<T> InputSplit for &mut [T] {
    fn len(&self) -> usize { (**self).len() }

    fn cut(self, at: usize) -> Self { &mut self[..at] }

    fn advance(self, at: usize) -> std::result::Result<Self, Self> {
        if self.len() >= at {
            Ok(&mut self[at..])
        } else {
            Err(self)
        }
    }
}

impl InputSplit for &mut str {
    fn len(&self) -> usize { (**self).len() }

    fn cut(self, at: usize) -> Self { &mut self[..at] }

    fn advance(self, at: usize) -> std::result::Result<Self, Self> {
        if self.len() >= at && self.is_char_boundary(at) {
            Ok(&mut self[at..])
        } else {
            Err(self)
        }
    }
}

impl<T> InputEq for &[T] {
    fn eq(&self, other: &Self) -> bool {
        let s: &[T] = self;
        let o: &[T] = other;

        std::ptr::eq(s, o)
    }
}

impl InputEq for &str {
    fn eq(&self, other: &Self) -> bool {
        let s: &str = self;
        let o: &str = other;

        std::ptr::eq(s, o)
    }
}

impl<T> InputEq for &mut [T] {
    fn eq(&self, other: &Self) -> bool {
        let s: &[T] = self;
        let o: &[T] = other;

        std::ptr::eq(s, o)
    }
}

impl InputEq for &mut str {
    fn eq(&self, other: &Self) -> bool {
        let s: &str = self;
        let o: &str = other;

        std::ptr::eq(s, o)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CompareResult<T> {
    Ok(T),
    Error,
    Incomplete,
}

impl<T> CompareResult<T> {
    pub fn map<O>(self, f: impl FnOnce(T) -> O) -> CompareResult<O> {
        match self {
            CompareResult::Error => CompareResult::Error,
            CompareResult::Incomplete => CompareResult::Incomplete,
            CompareResult::Ok(output) => CompareResult::Ok(f(output)),
        }
    }

    pub fn try_map_output<O>(self, f: impl FnOnce(T) -> Option<O>) -> CompareResult<O> {
        match self {
            CompareResult::Error => CompareResult::Error,
            CompareResult::Incomplete => CompareResult::Incomplete,
            CompareResult::Ok(output) => match f(output) {
                None => CompareResult::Error,
                Some(output) => CompareResult::Ok(output),
            },
        }
    }
}
