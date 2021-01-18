use dec_core::{error::PResult, Parse, ParseMut, ParseOnce};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct All<T>(pub T);

pub trait TupleAllOnce<I, E, Fail> {
    type Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail>;
}

pub trait TupleAllMut<I, E, Fail>: TupleAllOnce<I, E, Fail> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail>;
}

pub trait TupleAll<I, E, Fail>: TupleAllMut<I, E, Fail> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail>;
}

impl<T: TupleAllOnce<I, E, F>, I, E, F> ParseOnce<I, E, F> for All<T> {
    type Output = T::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, F> { self.0.parse_once(input) }
}

impl<T: TupleAllMut<I, E, F>, I, E, F> ParseMut<I, E, F> for All<T> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, F> { self.0.parse_mut(input) }
}

impl<T: TupleAll<I, E, F>, I, E, F> Parse<I, E, F> for All<T> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, F> { self.0.parse(input) }
}

macro_rules! impl_tuple_fold {
    () => {};
    ($i:ident $($ident:ident)*) => {
        impl<Input, Error, Fail, $i $(, $ident)*> TupleAllOnce<Input, Error, Fail> for ($i, $($ident,)*)
        where
            $i: ParseOnce<Input, Error, Fail>
            $(, $ident: ParseOnce<Input, Error, Fail>)*
        {
            type Output = (
                <$i as ParseOnce<Input, Error, Fail>>::Output,
                $(<$ident as ParseOnce<Input, Error, Fail>>::Output,)*
            );

            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse_once(self, input: Input) -> PResult<Input, Self::Output, Error, Fail> {
                let ($i, $($ident,)*) = self;

                let (input, $i) = $i.parse_once(input)?;

                $(let (input, $ident) = $ident.parse_once(input)?;)*

                Ok((input, ($i, $($ident,)*)))
            }
        }
        impl<Input, Error, Fail, $i $(, $ident)*> TupleAllMut<Input, Error, Fail> for ($i, $($ident,)*)
        where
            $i: ParseMut<Input, Error, Fail>
            $(, $ident: ParseMut<Input, Error, Fail>)*
        {
            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse_mut(&mut self, input: Input) -> PResult<Input, Self::Output, Error, Fail> {
                let ($i, $($ident,)*) = self;

                let (input, $i) = $i.parse_mut(input)?;

                $(let (input, $ident) = $ident.parse_mut(input)?;)*

                Ok((input, ($i, $($ident,)*)))
            }
        }
        impl<Input, Error, Fail, $i $(, $ident)*> TupleAll<Input, Error, Fail> for ($i, $($ident,)*)
        where
            $i: Parse<Input, Error, Fail>
            $(, $ident: Parse<Input, Error, Fail>)*
        {
            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse(&self, input: Input) -> PResult<Input, Self::Output, Error, Fail> {
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
    A0 B0 C0 D0 E0 F0 G0 H0
);

#[cfg(feature = "big-tuples")]
impl_tuple_fold!(
    A0 B0 C0 D0 E0 F0 G0 H0
    A1 B1 C1 D1 E1 F1 G1 H1
    A2 B2 C2 D2 E2 F2 G2 H2
    A3 B3 C3 D3 E3 F3 G3 H3
);
