pub type DefaultError<I> = (I, ErrorKind);
pub type PResult<I, T, E = DefaultError<I>> = std::result::Result<(I, T), Error<E>>;

#[derive(Debug, PartialEq, Eq)]
pub enum Error<E> {
    Error(E),
    Failure(E),
}

impl<E> From<E> for Error<E> {
    fn from(err: E) -> Self {
        Self::Error(err)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    Tag,
    Any,
    Skip,
    Not,
    Verify,
    Custom(&'static str),
}

pub trait ParseError<I>: Sized {
    fn from_input_kind(input: I, kind: ErrorKind) -> Self;

    fn append(self, input: I, kind: ErrorKind) -> Self;

    #[allow(unused)]
    fn add_context(self, input: I, ctx: &'static str) -> Self {
        self
    }

    fn or(self, other: Self) -> Self;
}

impl<I> ParseError<I> for () {
    fn from_input_kind(_: I, _: ErrorKind) -> Self {
        ()
    }

    fn append(self, _: I, _: ErrorKind) -> Self {
        self
    }

    fn or(self, _: Self) -> Self {
        self
    }
}

impl<I> ParseError<I> for std::convert::Infallible {
    fn from_input_kind(_: I, kind: ErrorKind) -> Self {
        panic!("parse error: {:?}", kind);
    }

    fn append(self, _: I, _: ErrorKind) -> Self {
        self
    }

    fn or(self, _: Self) -> Self {
        self
    }
}

impl<I> ParseError<I> for (I, ErrorKind) {
    fn from_input_kind(input: I, kind: ErrorKind) -> Self {
        (input, kind)
    }

    fn append(self, _: I, _: ErrorKind) -> Self {
        self
    }

    fn or(self, _: Self) -> Self {
        self
    }
}
