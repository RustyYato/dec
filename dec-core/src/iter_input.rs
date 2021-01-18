use core::{borrow::Borrow, iter::Peekable};

use crate::{
    error::{CaptureInput, PResult},
    ParseTag,
};

pub struct IterInput<I: Iterator> {
    iter: Peekable<I>,
}

impl<I: Iterator> IterInput<I> {
    pub fn new(input: I) -> Self { Self { iter: input.peekable() } }
}

impl<I: Iterator, T: Borrow<I::Item>> ParseTag<T> for IterInput<I>
where
    I::Item: PartialEq,
{
    type Output = I::Item;

    fn parse_tag(mut self, tag: &T) -> PResult<Self, Self::Output, CaptureInput<Self>> {
        if self.iter.peek() == Some(tag.borrow()) {
            let next = self.iter.next().unwrap();
            Ok((self, next))
        } else {
            Err(CaptureInput(self).into())
        }
    }
}
