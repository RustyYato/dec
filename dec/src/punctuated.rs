use std::{ops::RangeBounds, option, slice, vec};

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Punctuated<V, P> {
    inner: Vec<(V, P)>,
    last: Option<V>,
}

pub enum Pair<V, P> {
    End(V),
    Item(V, P),
}

impl<V, P> From<(V, P)> for Pair<V, P> {
    fn from((value, punct): (V, P)) -> Self {
        Self::Item(value, punct)
    }
}

impl<V, P> Pair<V, P> {
    pub fn from_ref(pair: &(V, P)) -> Pair<&V, &P> {
        let (value, punct) = pair;
        (value, punct).into()
    }

    pub fn from_mut(pair: &mut (V, P)) -> Pair<&mut V, &mut P> {
        let (value, punct) = pair;
        (value, punct).into()
    }

    pub fn into_value(self) -> V {
        match self {
            Self::End(value) | Self::Item(value, _) => value,
        }
    }
}

impl<V, P> From<V> for Pair<V, P> {
    fn from(value: V) -> Self {
        Self::End(value)
    }
}

impl<V, P> Punctuated<V, P> {
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
        fn push_value<V, P>(mut this: Punctuated<V, P>, value: V) -> Punctuated<V, P> {
            this.push_value(value);
            this
        }

        fn push_punct<V, P>(mut this: Punctuated<V, P>, punct: P) -> Punctuated<V, P> {
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
        Self {
            inner: Vec::new(),
            last: None,
        }
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

    pub fn first(&self) -> Option<&V> {
        self.inner.first().map(|(x, _)| x).or(self.last.as_ref())
    }

    pub fn first_mut(&mut self) -> Option<&mut V> {
        self.inner
            .first_mut()
            .map(|(x, _)| x)
            .or(self.last.as_mut())
    }

    pub fn last(&self) -> Option<&V> {
        self.last.as_ref().or(self.inner.last().map(|(x, _)| x))
    }

    pub fn last_mut(&mut self) -> Option<&mut V> {
        self.last.as_mut().or(self.inner.last_mut().map(|(x, _)| x))
    }

    pub fn into_pairs(self) -> IntoPairs<V, P> {
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

#[derive(Clone)]
pub struct IntoPairs<V, P> {
    inner: vec::IntoIter<(V, P)>,
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

#[derive(Clone)]
pub struct IntoIter<V, P> {
    inner: IntoPairs<V, P>,
}

pub struct IterMut<'a, V, P> {
    inner: PairsMut<'a, V, P>,
}

pub struct Iter<'a, V, P> {
    inner: Pairs<'a, V, P>,
}

impl<V, P> IntoIterator for Punctuated<V, P> {
    type IntoIter = IntoIter<V, P>;
    type Item = V;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.into_pairs(),
        }
    }
}

impl<'a, V, P> IntoIterator for &'a mut Punctuated<V, P> {
    type IntoIter = IterMut<'a, V, P>;
    type Item = &'a mut V;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<'a, V, P> IntoIterator for &'a Punctuated<V, P> {
    type IntoIter = Iter<'a, V, P>;
    type Item = &'a V;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
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

impl<V, P> Iterator for IntoPairs<V, P> {
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

impl<V, P> DoubleEndedIterator for IntoPairs<V, P> {
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

impl<V, P> Iterator for IntoIter<V, P> {
    type Item = V;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(Pair::into_value)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<V, P> DoubleEndedIterator for IntoIter<V, P> {
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

impl<V, P> std::iter::FromIterator<Pair<V, P>> for Punctuated<V, P> {
    fn from_iter<I: IntoIterator<Item = Pair<V, P>>>(iter: I) -> Self {
        let mut punctuated = Self::new();
        punctuated.extend(iter.into_iter());
        punctuated
    }
}

impl<V, P> Extend<Pair<V, P>> for Punctuated<V, P> {
    fn extend<I: IntoIterator<Item = Pair<V, P>>>(&mut self, iter: I) {
        assert!(self.last.is_none());
        let iter = iter.into_iter();
        self.reserve(iter.size_hint().0);
        let mut nomore = false;
        iter.for_each(|pair| {
            if nomore {
                panic!("Punctuated extended with items after a Pair::End");
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

impl<V, P: Default> std::iter::FromIterator<V> for Punctuated<V, P> {
    fn from_iter<I: IntoIterator<Item = V>>(iter: I) -> Self {
        let mut punctuated = Self::new();
        punctuated.extend(iter.into_iter());
        punctuated
    }
}

impl<V, P: Default> Extend<V> for Punctuated<V, P> {
    fn extend<I: IntoIterator<Item = V>>(&mut self, iter: I) {
        let iter = iter.into_iter();
        self.reserve(iter.size_hint().0);
        iter.for_each(|value| self.push(value))
    }
}
