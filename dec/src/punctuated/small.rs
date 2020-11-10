use smallvec::{Array, SmallVec};
use std::fmt;
use std::{cmp::Ordering, hash, ops::RangeBounds, option, slice};

use super::Pair;

pub struct SmallPunctuated<V, A: Array> {
    inner: SmallVec<A>,
    last: Option<V>,
}

impl<V, P, A: Array<Item = (V, P)>> Default for SmallPunctuated<V, A> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
            last: Default::default(),
        }
    }
}

impl<V: fmt::Debug, P: fmt::Debug, A: Array<Item = (V, P)>> fmt::Debug for SmallPunctuated<V, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SmallPunctuated")
            .field("inner", &self.inner.as_slice())
            .field("last", &self.last)
            .finish()
    }
}

impl<V: Clone, P: Clone, A: Array<Item = (V, P)>> Clone for SmallPunctuated<V, A> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            last: self.last.clone(),
        }
    }

    fn clone_from(&mut self, other: &Self) {
        self.inner.clone_from(&other.inner);
        self.last.clone_from(&other.last);
    }
}

impl<V: Eq, P: Eq, A: Array<Item = (V, P)>> Eq for SmallPunctuated<V, A> {}
impl<V: PartialEq, P: PartialEq, A: Array<Item = (V, P)>> PartialEq for SmallPunctuated<V, A> {
    fn eq(&self, other: &Self) -> bool {
        self.last == other.last && self.inner == other.inner
    }
}

impl<V: PartialOrd, P: PartialOrd, A: Array<Item = (V, P)>> PartialOrd for SmallPunctuated<V, A> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(
            self.inner
                .partial_cmp(&other.inner)?
                .then(self.last.partial_cmp(&self.last)?),
        )
    }
}

impl<V: Ord, P: Ord, A: Array<Item = (V, P)>> Ord for SmallPunctuated<V, A> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.inner.cmp(&other.inner).then(self.last.cmp(&self.last))
    }
}

impl<V: hash::Hash, P: hash::Hash, A: Array<Item = (V, P)>> hash::Hash for SmallPunctuated<V, A> {
    fn hash<H: hash::Hasher>(&self, other: &mut H) {
        self.inner.hash(other);
        self.last.hash(other);
    }
}

impl<V, P, Arr> SmallPunctuated<V, Arr>
where
    Arr: Array<Item = (V, P)>,
{
    pub fn parse<A, B>(
        punct: A,
        value: B,
    ) -> crate::separated::SeparatedFoldRange<
        std::ops::RangeFull,
        impl Fn() -> Self + Copy,
        A,
        B,
        impl Fn(Self, P) -> Self + Copy,
        impl Fn(Self, V) -> Self + Copy,
    > {
        Self::parse_range(.., punct, value)
    }

    pub fn parse_range<R: RangeBounds<usize>, A, B>(
        range: R,
        punct: A,
        value: B,
    ) -> crate::separated::SeparatedFoldRange<
        R,
        impl Fn() -> Self + Copy,
        A,
        B,
        impl Fn(Self, P) -> Self + Copy,
        impl Fn(Self, V) -> Self + Copy,
    > {
        fn push_value<V, P, A: Array<Item = (V, P)>>(
            mut this: SmallPunctuated<V, A>,
            value: V,
        ) -> SmallPunctuated<V, A> {
            this.push_value(value);
            this
        }

        fn push_punct<V, P, A: Array<Item = (V, P)>>(
            mut this: SmallPunctuated<V, A>,
            punct: P,
        ) -> SmallPunctuated<V, A> {
            this.push_punct(punct);
            this
        }

        crate::separated::SeparatedFoldRange {
            range,
            mk_acc: Self::new,
            item: value,
            sep: punct,
            item_func: push_value,
            sep_func: push_punct,
        }
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn trailing_punct(&self) -> bool {
        self.last.is_none() && !self.inner.is_empty()
    }

    pub fn clear(&mut self) {
        self.inner.clear();
        self.last = None;
    }

    pub fn reserve(&mut self, additional: usize) {
        if self.last.is_none() && additional < 2 {
            return;
        }

        self.inner
            .reserve(additional - usize::from(self.last.is_none()))
    }

    pub fn push_value(&mut self, value: V) {
        assert!(
            self.last.is_none(),
            "The sequence must be empty or end in a punctuation"
        );
        self.last = Some(value);
    }

    pub fn push_punct(&mut self, punct: P) {
        let value = self
            .last
            .take()
            .expect("The sequence must have a value in it");
        self.inner.push((value, punct));
    }

    pub fn push(&mut self, value: V)
    where
        P: Default,
    {
        if let Some(last) = self.last.as_mut() {
            let value = std::mem::replace(last, value);
            self.inner.push((value, P::default()))
        } else {
            self.last = Some(value);
        }
    }

    pub fn insert(&mut self, index: usize, value: V)
    where
        P: Default,
    {
        if index == self.len() {
            self.push(value);
        } else {
            self.inner.insert(index, (value, Default::default()));
        }
    }

    pub fn pop(&mut self) -> Option<Pair<V, P>> {
        match self.last.take() {
            Some(value) => Some(Pair::End(value)),
            None => {
                let (value, punct) = self.inner.pop()?;
                Some(Pair::Item(value, punct))
            }
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len() + usize::from(self.last.is_some())
    }

    pub fn first<'a>(&'a self) -> Option<&V>
    where
        P: 'a,
    {
        self.inner.first().map(|(x, _)| x).or(self.last.as_ref())
    }

    pub fn first_mut<'a>(&'a mut self) -> Option<&mut V>
    where
        P: 'a,
    {
        self.inner
            .first_mut()
            .map(|(x, _)| x)
            .or(self.last.as_mut())
    }

    pub fn last<'a>(&'a self) -> Option<&V>
    where
        P: 'a,
    {
        self.last.as_ref().or(self.inner.last().map(|(x, _)| x))
    }

    pub fn last_mut<'a>(&'a mut self) -> Option<&mut V>
    where
        P: 'a,
    {
        self.last.as_mut().or(self.inner.last_mut().map(|(x, _)| x))
    }

    pub fn into_pairs(self) -> IntoPairs<V, Arr> {
        IntoPairs {
            inner: self.inner.into_iter(),
            last: self.last,
        }
    }

    pub fn pairs_mut(&mut self) -> PairsMut<V, P> {
        PairsMut {
            inner: self.inner.iter_mut(),
            last: self.last.iter_mut(),
        }
    }

    pub fn pairs(&self) -> Pairs<V, P> {
        Pairs {
            inner: self.inner.iter(),
            last: self.last.iter(),
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<V, P> {
        IterMut {
            inner: self.pairs_mut(),
        }
    }

    pub fn iter(&self) -> Iter<V, P> {
        Iter {
            inner: self.pairs(),
        }
    }
}

pub struct IntoPairs<V, A: Array> {
    inner: smallvec::IntoIter<A>,
    last: Option<V>,
}

pub struct PairsMut<'a, V, P> {
    inner: slice::IterMut<'a, (V, P)>,
    last: option::IterMut<'a, V>,
}

pub struct Pairs<'a, V, P> {
    inner: slice::Iter<'a, (V, P)>,
    last: option::Iter<'a, V>,
}

pub struct IntoIter<V, A: Array> {
    inner: IntoPairs<V, A>,
}

pub struct IterMut<'a, V, P> {
    inner: PairsMut<'a, V, P>,
}

pub struct Iter<'a, V, P> {
    inner: Pairs<'a, V, P>,
}

impl<V, P, Arr: Array<Item = (V, P)>> IntoIterator for SmallPunctuated<V, Arr> {
    type IntoIter = IntoIter<V, Arr>;
    type Item = V;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.into_pairs(),
        }
    }
}

impl<'a, V, P: 'a, A: Array<Item = (V, P)>> IntoIterator for &'a mut SmallPunctuated<V, A> {
    type IntoIter = IterMut<'a, V, P>;
    type Item = &'a mut V;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<'a, V, P: 'a, A: Array<Item = (V, P)>> IntoIterator for &'a SmallPunctuated<V, A> {
    type IntoIter = Iter<'a, V, P>;
    type Item = &'a V;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<V: Clone, P: Clone, A: Clone + Array<Item = (V, P)>> Clone for IntoPairs<V, A> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            last: self.last.clone(),
        }
    }
}

impl<V: Clone, P: Clone, A: Clone + Array<Item = (V, P)>> Clone for IntoIter<V, A> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<V, P> Clone for Pairs<'_, V, P> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            last: self.last.clone(),
        }
    }
}

impl<V, P> Clone for Iter<'_, V, P> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<V, P, A: Array<Item = (V, P)>> Iterator for IntoPairs<V, A> {
    type Item = Pair<V, P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(Pair::from)
            .or_else(|| self.last.take().map(Pair::from))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (len, _) = self.inner.size_hint();
        (len + 1, Some(len + 1))
    }
}

impl<V, P, A: Array<Item = (V, P)>> DoubleEndedIterator for IntoPairs<V, A> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.last
            .take()
            .map(Pair::from)
            .or_else(|| self.inner.next_back().map(Pair::from))
    }
}

impl<'a, V, P> Iterator for PairsMut<'a, V, P> {
    type Item = Pair<&'a mut V, &'a mut P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(Pair::from_mut)
            .or_else(|| self.last.next().map(Pair::from))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (len, _) = self.inner.size_hint();
        (len + 1, Some(len + 1))
    }
}

impl<V, P> DoubleEndedIterator for PairsMut<'_, V, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.last
            .next_back()
            .map(Pair::from)
            .or_else(|| self.inner.next_back().map(Pair::from_mut))
    }
}

impl<'a, V, P> Iterator for Pairs<'a, V, P> {
    type Item = Pair<&'a V, &'a P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(Pair::from_ref)
            .or_else(|| self.last.next().map(Pair::from))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (len, _) = self.inner.size_hint();
        (len + 1, Some(len + 1))
    }
}

impl<V, P> DoubleEndedIterator for Pairs<'_, V, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.last
            .next_back()
            .map(Pair::from)
            .or_else(|| self.inner.next_back().map(Pair::from_ref))
    }
}

impl<V, P, A: Array<Item = (V, P)>> Iterator for IntoIter<V, A> {
    type Item = V;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(Pair::into_value)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<V, P, A: Array<Item = (V, P)>> DoubleEndedIterator for IntoIter<V, A> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back().map(Pair::into_value)
    }
}

impl<'a, V, P> Iterator for IterMut<'a, V, P> {
    type Item = &'a mut V;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(Pair::into_value)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<V, P> DoubleEndedIterator for IterMut<'_, V, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back().map(Pair::into_value)
    }
}

impl<'a, V, P> Iterator for Iter<'a, V, P> {
    type Item = &'a V;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(Pair::into_value)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<V, P> DoubleEndedIterator for Iter<'_, V, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back().map(Pair::into_value)
    }
}

impl<V, P, A: Array<Item = (V, P)>> std::iter::FromIterator<Pair<V, P>> for SmallPunctuated<V, A> {
    fn from_iter<I: IntoIterator<Item = Pair<V, P>>>(iter: I) -> Self {
        let mut this = Self::new();
        this.extend(iter.into_iter());
        this
    }
}

impl<V, P, A: Array<Item = (V, P)>> Extend<Pair<V, P>> for SmallPunctuated<V, A> {
    fn extend<I: IntoIterator<Item = Pair<V, P>>>(&mut self, iter: I) {
        assert!(self.last.is_none());
        let iter = iter.into_iter();
        self.reserve(iter.size_hint().0);
        let mut nomore = false;
        iter.for_each(|pair| {
            if nomore {
                panic!("SmallPunctuated extended with items after a Pair::End");
            }
            match pair {
                Pair::Item(a, b) => self.inner.push((a, b)),
                Pair::End(a) => {
                    self.last = Some(a);
                    nomore = true;
                }
            }
        })
    }
}

impl<V, P: Default, A: Array<Item = (V, P)>> std::iter::FromIterator<V> for SmallPunctuated<V, A> {
    fn from_iter<I: IntoIterator<Item = V>>(iter: I) -> Self {
        let mut this = Self::new();
        this.extend(iter.into_iter());
        this
    }
}

impl<V, P: Default, A: Array<Item = (V, P)>> Extend<V> for SmallPunctuated<V, A> {
    fn extend<I: IntoIterator<Item = V>>(&mut self, iter: I) {
        let iter = iter.into_iter();
        self.reserve(iter.size_hint().0);
        iter.for_each(|value| self.push(value))
    }
}
