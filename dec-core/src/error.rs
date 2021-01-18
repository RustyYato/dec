pub type DefaultError<I> = (I, ErrorKind);
pub type PResult<I, T, E = DefaultError<I>, F = E> = core::result::Result<(I, T), Error<E, F>>;

#[cfg(feature = "alloc")]
pub mod verbose;

#[derive(Debug, PartialEq, Eq)]
pub struct CaptureInput<T>(pub T);

#[derive(Debug, PartialEq, Eq)]
pub enum Error<E, F = E> {
    Error(E),
    Failure(F),
}

impl<E, F> From<E> for Error<E, F> {
    fn from(err: E) -> Self { Self::Error(err) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum ErrorKind {
    Tag,
    Any,
    Skip,
    Not,
    Verify,
    RangeStart,
    Pratt(PrattErrorKind),
    Custom(&'static str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum PrattErrorKind {
    FinishInfixOp,
    MergePrefixOp,
    MergeInfixOp,
    MergePostfixOp,
}

pub trait ParseError<I>: Sized {
    fn from_input_kind(input: I, kind: ErrorKind) -> Self;

    fn append(self, input: I, kind: ErrorKind) -> Self;

    #[allow(unused)]
    fn add_context(self, input: I, ctx: &'static str) -> Self { self }

    fn or(self, other: Self) -> Self;

    fn into_input(self) -> I;
}

impl<I> ParseError<I> for core::convert::Infallible {
    fn from_input_kind(_: I, kind: ErrorKind) -> Self {
        panic!("parse error: {:?}", kind);
    }

    fn append(self, _: I, _: ErrorKind) -> Self { self }

    fn or(self, _: Self) -> Self { self }

    fn into_input(self) -> I { match self {} }
}

impl<I> ParseError<I> for (I, ErrorKind) {
    fn from_input_kind(input: I, kind: ErrorKind) -> Self { (input, kind) }

    fn append(self, _: I, _: ErrorKind) -> Self { self }

    fn or(self, _: Self) -> Self { self }

    fn into_input(self) -> I { self.0 }
}

impl<I> ParseError<I> for CaptureInput<I> {
    fn from_input_kind(input: I, _: ErrorKind) -> Self { Self(input) }

    fn append(self, _: I, _: ErrorKind) -> Self { self }

    fn or(self, _: Self) -> Self { self }

    fn into_input(self) -> I { self.0 }
}
