use crate::{
    error::{CaptureInput, PResult},
    InputEq, InputSplit, ParseTag,
};

use bitvec::{array::BitArray, order::BitOrder, slice::BitSlice, store::BitStore, view::BitView};

impl<O: BitOrder, T: BitStore> InputEq for &BitSlice<O, T> {
    fn eq(&self, other: &Self) -> bool { core::ptr::eq(self, other) }
}

impl<O: BitOrder, T: BitStore> InputSplit for &BitSlice<O, T> {
    fn len(&self) -> usize { BitSlice::len(self) }

    fn is_empty(&self) -> bool { BitSlice::is_empty(self) }

    fn advance(self, at: usize) -> core::result::Result<Self, Self> {
        match self.get(at..) {
            Some(value) => Ok(value),
            None => Err(self),
        }
    }

    fn cut(self, at: usize) -> Self { &self[..at] }
}

impl<'a, O1: BitOrder, T1: BitStore, O2: BitOrder, T2: BitStore> ParseTag<&BitSlice<O2, T2>> for &'a BitSlice<O1, T1> {
    type Output = &'a BitSlice<O1, T1>;

    fn parse_tag(
        self,
        &tag: &&BitSlice<O2, T2>,
    ) -> PResult<Self, Self::Output, CaptureInput<Self>, core::convert::Infallible> {
        if self.starts_with(tag) {
            let (tag, input) = unsafe { self.split_at_unchecked(self.len()) };
            Ok((input, tag))
        } else {
            Err(CaptureInput(self).into())
        }
    }
}

impl<'a, O1: BitOrder, T1: BitStore, O2: BitOrder, T2: BitView> ParseTag<BitArray<O2, T2>> for &'a BitSlice<O1, T1> {
    type Output = &'a BitSlice<O1, T1>;

    fn parse_tag(
        self,
        tag: &BitArray<O2, T2>,
    ) -> PResult<Self, Self::Output, CaptureInput<Self>, core::convert::Infallible> {
        if self.starts_with(tag) {
            let (tag, input) = unsafe { self.split_at_unchecked(self.len()) };
            Ok((input, tag))
        } else {
            Err(CaptureInput(self).into())
        }
    }
}

impl<'a, O1: BitOrder, T1: BitStore, O2: BitOrder, T2: BitView> ParseTag<&BitArray<O2, T2>> for &'a BitSlice<O1, T1> {
    type Output = &'a BitSlice<O1, T1>;

    fn parse_tag(
        self,
        &tag: &&BitArray<O2, T2>,
    ) -> PResult<Self, Self::Output, CaptureInput<Self>, core::convert::Infallible> {
        if self.starts_with(tag) {
            let (tag, input) = unsafe { self.split_at_unchecked(self.len()) };
            Ok((input, tag))
        } else {
            Err(CaptureInput(self).into())
        }
    }
}
