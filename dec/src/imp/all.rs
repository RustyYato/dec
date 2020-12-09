use crate::{error::*, traits::*};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct All<T>(pub T);

pub trait TupleAllOnce<I, E> {
    type Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E>;
}

pub trait TupleAllMut<I, E>: TupleAllOnce<I, E> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E>;
}

pub trait TupleAll<I, E>: TupleAllMut<I, E> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E>;
}

impl<T: TupleAllOnce<I, E>, I, E: ParseError<I>> ParseOnce<I, E> for All<T> {
    type Output = T::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> { self.0.parse_once(input) }
}

impl<T: TupleAllMut<I, E>, I, E: ParseError<I>> ParseMut<I, E> for All<T> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> { self.0.parse_mut(input) }
}

impl<T: TupleAll<I, E>, I, E: ParseError<I>> Parse<I, E> for All<T> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> { self.0.parse(input) }
}

macro_rules! impl_tuple_fold {
    () => {};
    ($i:ident $($ident:ident)*) => {
        impl<Input, Error: ParseError<Input>, $i $(, $ident)*> TupleAllOnce<Input, Error> for ($i, $($ident,)*)
        where
            $i: ParseOnce<Input, Error>
            $(, $ident: ParseOnce<Input, Error>)*
        {
            type Output = (
                <$i as ParseOnce<Input, Error>>::Output,
                $(<$ident as ParseOnce<Input, Error>>::Output,)*
            );

            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse_once(self, input: Input) -> PResult<Input, Self::Output, Error> {
                let ($i, $($ident,)*) = self;

                let (input, $i) = $i.parse_once(input)?;

                $(let (input, $ident) = $ident.parse_once(input)?;)*

                Ok((input, ($i, $($ident,)*)))
            }
        }
        impl<Input, Error: ParseError<Input>, $i $(, $ident)*> TupleAllMut<Input, Error> for ($i, $($ident,)*)
        where
            $i: ParseMut<Input, Error>
            $(, $ident: ParseMut<Input, Error>)*
        {
            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse_mut(&mut self, input: Input) -> PResult<Input, Self::Output, Error> {
                let ($i, $($ident,)*) = self;

                let (input, $i) = $i.parse_mut(input)?;

                $(let (input, $ident) = $ident.parse_mut(input)?;)*

                Ok((input, ($i, $($ident,)*)))
            }
        }
        impl<Input, Error: ParseError<Input>, $i $(, $ident)*> TupleAll<Input, Error> for ($i, $($ident,)*)
        where
            $i: Parse<Input, Error>
            $(, $ident: Parse<Input, Error>)*
        {
            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse(&self, input: Input) -> PResult<Input, Self::Output, Error> {
                let ($i, $($ident,)*) = self;

                let (input, $i) = $i.parse(input)?;

                $(let (input, $ident) = $ident.parse(input)?;)*

                Ok((input, ($i, $($ident,)*)))
            }
        }
        impl_tuple_fold!($($ident)*);
    };
}

#[cfg(not(feature = "big-tuples"))]
impl_tuple_fold!(
    A0 B0 C0 D0
);

#[cfg(feature = "big-tuples")]
impl_tuple_fold!(
    A0 B0 C0 D0 E0 F0 G0 H0
    A1 B1 C1 D1 E1 F1 G1 H1
    A2 B2 C2 D2 E2 F2 G2 H2
    A3 B3 C3 D3 E3 F3 G3 H3
);
