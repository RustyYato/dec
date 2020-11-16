use crate::traits::{Compare, CompareResult, InputSplit};

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
    pub span: Span<P>,
    pub lexeme: I,
}

pub trait Pos: Clone {
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
}

impl<I: InputSplit + Clone, P: Pos, T: Compare<I>> Compare<Indexed<I, P>> for T {
    type Output = Spanned<I, T::Output, P>;

    fn compare(
        &self,
        mut indexed_input: Indexed<I, P>,
    ) -> (Indexed<I, P>, CompareResult<Self::Output>) {
        let start = indexed_input.pos.clone();
        let len = indexed_input.input.len();

        let (input, result) = self.compare(indexed_input.input.clone());

        match result {
            CompareResult::Incomplete => (indexed_input, CompareResult::Incomplete),
            CompareResult::Error => (indexed_input, CompareResult::Error),
            CompareResult::Ok(value) => {
                let lexeme_len = len - indexed_input.input.len();
                indexed_input.pos.inc(lexeme_len);
                let end = indexed_input.pos.clone();

                let lexeme = indexed_input.input.cut(lexeme_len);

                indexed_input.input = input;
                (
                    indexed_input,
                    CompareResult::Ok(Spanned {
                        span: Span { start, end },
                        lexeme,
                        value,
                    }),
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
