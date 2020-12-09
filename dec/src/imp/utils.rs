use core::convert::Infallible;

pub fn absurd<T>(x: Infallible) -> T { match x {} }
pub fn unwrap_absurd<I, T>((i, x): (I, Result<T, Infallible>)) -> (I, T) { (i, x.unwrap_or_else(absurd)) }
pub fn ok<F: FnMut(A, B) -> C, A, B, C, E>(mut f: F) -> impl FnMut(A, B) -> Result<C, E> { move |a, b| Ok(f(a, b)) }
pub fn value<T>(value: T) -> impl FnOnce() -> T { move || value }

pub fn into_inner<T>(r: Result<T, T>) -> T {
    match r {
        Ok(x) | Err(x) => x,
    }
}

#[inline(always)]
pub fn step<A, O, I, F>(mut f: F, mut iter: I) -> impl FnMut(A, O) -> Result<A, A>
where
    F: FnMut(A, O) -> A,
    I: Iterator<Item = ()>,
{
    move |acc, out| {
        let acc = f(acc, out);
        match iter.next() {
            Some(()) => Ok(acc),
            None => Err(acc),
        }
    }
}

pub fn extend<C: Extend<V>, V>(mut collection: C, value: V) -> C {
    collection.extend(Some(value));
    collection
}

pub fn fst<A, B>(a: A, _: B) -> A { a }
