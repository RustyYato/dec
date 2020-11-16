use crate::traits::{Compare, CompareResult, InputEq, InputSplit};

type DefaultPos = u32;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span<P = DefaultPos> {
    start: P,
    end: P,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indexed<I, P = DefaultPos> {
    input: I,
    pos: P,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<I, T, P = DefaultPos> {
    pub value: T,
    pub lexeme: Indexed<I, P>,
}

impl<I: InputEq, P: PartialEq> InputEq for Indexed<I, P> {
    fn eq(&self, other: &Self) -> bool {
        self.pos == other.pos && self.input.eq(&other.input)
    }
}

impl<I: InputSplit, P: Pos> InputSplit for Indexed<I, P> {
    fn len(&self) -> usize {
        self.input.len()
    }

    fn cut(mut self, at: usize) -> Self {
        self.input = self.input.cut(at);
        self
    }

    fn advance(mut self, at: usize) -> std::result::Result<Self, Self> {
        match self.input.advance(at) {
            Ok(input) => {
                self.input = input;
                self.pos.inc(at);
                Ok(self)
            }
            Err(input) => {
                self.input = input;
                Err(self)
            }
        }
    }
}

pub trait Pos: Copy {
    fn inc(&mut self, inc: usize);
}

impl<I, P: Pos + Default> Indexed<I, P> {
    pub fn new(input: I) -> Self {
        Self::with_pos(input, P::default())
    }
}

impl<I, P: Pos> Indexed<I, P> {
    pub fn with_pos(input: I, pos: P) -> Self {
        Self { input, pos }
    }

    pub fn pos(&self) -> P {
        self.pos.clone()
    }

    pub fn input(&self) -> &I {
        &self.input
    }

    pub fn span(&self) -> Span<P>
    where
        I: InputSplit,
    {
        let mut end = self.pos;
        end.inc(self.input.len());

        Span {
            start: self.pos,
            end,
        }
    }
}

impl<I: InputSplit + Clone, P: Pos, T: Compare<I>> Compare<Indexed<I, P>> for T {
    type Output = Spanned<I, T::Output, P>;

    fn compare(
        &self,
        indexed_input: Indexed<I, P>,
    ) -> (Indexed<I, P>, CompareResult<Self::Output>) {
        let len = indexed_input.input.len();
        let (input, result) = self.compare(indexed_input.input.clone());

        match result {
            CompareResult::Incomplete => (indexed_input, CompareResult::Incomplete),
            CompareResult::Error => (indexed_input, CompareResult::Error),
            CompareResult::Ok(value) => {
                let lexeme_len = len - input.len();
                let lexeme = indexed_input.cut(lexeme_len);
                (
                    Indexed {
                        input,
                        pos: lexeme.span().end,
                    },
                    CompareResult::Ok(Spanned { lexeme, value }),
                )
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
