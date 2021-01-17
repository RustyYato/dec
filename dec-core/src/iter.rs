use crate::{
    error::{Error, PResult, ParseError},
    ParseMut,
};

enum IterState<I, E> {
    Input(I),
    Error(Error<E>),
    Empty,
}

pub struct Iter<P, I, E> {
    parser: P,
    state: IterState<I, E>,
}

impl<I, E> IterState<I, E> {
    fn take(&mut self) -> Option<I> {
        match core::mem::replace(self, IterState::Empty) {
            IterState::Input(input) => Some(input),
            IterState::Empty => unreachable!(),
            IterState::Error(err) => {
                *self = IterState::Error(err);
                None
            }
        }
    }

    fn into(self) -> PResult<I, (), E> {
        match self {
            IterState::Input(input) => Ok((input, ())),
            IterState::Error(err) => Err(err),
            IterState::Empty => unreachable!(),
        }
    }
}

impl<P, I, E> Iter<P, I, E> {
    pub(crate) fn new(parser: P, input: I) -> Self {
        Self {
            parser,
            state: IterState::Input(input),
        }
    }

    pub fn finish(self) -> PResult<I, (), E> { self.state.into() }
}

impl<P: ParseMut<I, E>, I, E: ParseError<I>> core::iter::FusedIterator for &mut Iter<P, I, E> {}
impl<P: ParseMut<I, E>, I, E: ParseError<I>> Iterator for &mut Iter<P, I, E> {
    type Item = P::Output;

    fn next(&mut self) -> Option<Self::Item> {
        match self.parser.parse_mut(self.state.take()?) {
            Ok((i, x)) => {
                self.state = IterState::Input(i);
                Some(x)
            }
            Err(err) => {
                self.state = IterState::Error(err);
                None
            }
        }
    }

    // as long as the parser and the folding function don't panic
    // everything is good, otherwise the input is lost forever
    //
    // fn fold<B, F>(mut self, mut init: B, mut f: F) -> B
    // where
    //     Self: Sized,
    //     F: FnMut(B, Self::Item) -> B,
    // {
    //     let mut input = match self.state.take() {
    //         Some(input) => input,
    //         None => return init,
    //     };

    //     loop {
    //         match self.parser.parse_mut(input) {
    //             Ok((i, x)) => {
    //                 input = i;
    //                 init = f(init, x);
    //             }
    //             Err(err) => {
    //                 self.state = IterState::Error(err);
    //                 break init;
    //             }
    //         }
    //     }
    // }
}
