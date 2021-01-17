use crate::{Compare, InputEq, InputSplit};

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

impl<'a, O1: BitOrder, T1: BitStore, O2: BitOrder, T2: BitStore> Compare<&'a BitSlice<O1, T1>> for &BitSlice<O2, T2> {
    type Output = &'a BitSlice<O1, T1>;

    fn compare(&self, input: &'a BitSlice<O1, T1>) -> (&'a BitSlice<O1, T1>, Option<Self::Output>) {
        if input.starts_with(self) {
            let (tag, input) = unsafe { input.split_at_unchecked(self.len()) };
            (input, Some(tag))
        } else {
            (input, None)
        }
    }
}

impl<'a, O1: BitOrder, T1: BitStore, O2: BitOrder, T2: BitView> Compare<&'a BitSlice<O1, T1>> for &BitArray<O2, T2> {
    type Output = &'a BitSlice<O1, T1>;

    fn compare(&self, input: &'a BitSlice<O1, T1>) -> (&'a BitSlice<O1, T1>, Option<Self::Output>) {
        if input.starts_with(self) {
            let (tag, input) = unsafe { input.split_at_unchecked(self.len()) };
            (input, Some(tag))
        } else {
            (input, None)
        }
    }
}
