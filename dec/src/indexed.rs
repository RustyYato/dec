use crate::traits::{Compare, CompareResult, InputEq, InputSplit};

type DefaultPos = u32;

pub trait Spanned<P> {
    fn span(&self) -> Span<P>;
}

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span<P = DefaultPos> {
    start: P,
    end: P,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indexed<I, P = DefaultPos> {
    inner: I,
    pos: P,
}

use std::fmt;
impl<P: fmt::Debug> fmt::Debug for Span<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

impl<P: fmt::Display> fmt::Display for Span<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl<I: InputEq, P: PartialEq> InputEq for Indexed<I, P> {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos && self.inner.eq(&other.inner)
    }
}

impl<I: InputSplit, P: Pos> InputSplit for Indexed<I, P> {
    fn len(&self) -> usize {
        self.inner.len()
    }

    fn cut(mut self, at: usize) -> Self {
        self.inner = self.inner.cut(at);
        self
    }

    fn advance(mut self, at: usize) -> std::result::Result<Self, Self> {
        match self.inner.advance(at) {
            Ok(inner) => {
                self.inner = inner;
                self.pos.inc(at);
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
    fn inc(&mut self, inc: usize);
}

impl<I, P: Pos + Default> Indexed<I, P> {
    pub fn new(inner: I) -> Self {
        Self::with_pos(inner, P::default())
    }
}

impl<I, P: Pos> Indexed<I, P> {
    pub fn with_pos(inner: I, pos: P) -> Self {
        Self { inner, pos }
    }

    pub fn pos(&self) -> P {
        self.pos.clone()
    }

    pub fn inner(&self) -> &I {
        &self.inner
    }
}

impl<I: InputSplit, P: Pos> Spanned<P> for Indexed<I, P> {
    fn span(&self) -> Span<P> {
        let mut end = self.pos;
        end.inc(self.inner.len());

        Span {
            start: self.pos,
            end,
        }
    }
}

impl<I: Spanned<P>, T, P: Pos> Spanned<P> for (I, T) {
    fn span(&self) -> Span<P> {
        self.0.span()
    }
}

impl<I: InputSplit, P: Pos, T: Compare<I>> Compare<Indexed<I, P>> for T {
    type Output = T::Output;

    fn compare(
        &self,
        mut indexed_input: Indexed<I, P>,
    ) -> (Indexed<I, P>, CompareResult<Self::Output>) {
        let len = indexed_input.inner.len();
        let (input, result) = self.compare(indexed_input.inner);
        indexed_input.inner = input;

        match result {
            CompareResult::Incomplete => (indexed_input, CompareResult::Incomplete),
            CompareResult::Error => (indexed_input, CompareResult::Error),
            CompareResult::Ok(value) => {
                let lexeme_len = len - indexed_input.inner.len();
                indexed_input.pos.inc(lexeme_len);

                (indexed_input, CompareResult::Ok(value))
            }
        }
    }
}

impl Pos for u8 {
    fn inc(&mut self, inc: usize) {
        *self = (*self as usize + inc) as _;
    }
}

impl Pos for u16 {
    fn inc(&mut self, inc: usize) {
        *self = (*self as usize + inc) as _;
    }
}

impl Pos for u32 {
    fn inc(&mut self, inc: usize) {
        *self = (*self as usize + inc) as _;
    }
}

impl Pos for u64 {
    fn inc(&mut self, inc: usize) {
        *self = (*self as usize + inc) as _;
    }
}

impl Pos for u128 {
    fn inc(&mut self, inc: usize) {
        *self = (*self as usize + inc) as _;
    }
}

impl Pos for usize {
    fn inc(&mut self, inc: usize) {
        *self = (*self as usize + inc) as _;
    }
}
