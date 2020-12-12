use dec_core::{
    error::{Error, ErrorKind, PResult, ParseError},
    Parse, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Any<T>(pub T);

pub trait TupleAnyOnce<I, E> {
    type Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E>;
}

pub trait TupleAnyMut<I, E>: TupleAnyOnce<I, E> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E>;
}

pub trait TupleAny<I, E>: TupleAnyMut<I, E> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E>;
}

impl<T: TupleAnyOnce<I, E>, I, E: ParseError<I>> ParseOnce<I, E> for Any<T> {
    type Output = T::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> { self.0.parse_once(input) }
}

impl<T: TupleAnyMut<I, E>, I, E: ParseError<I>> ParseMut<I, E> for Any<T> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> { self.0.parse_mut(input) }
}

impl<T: TupleAny<I, E>, I, E: ParseError<I>> Parse<I, E> for Any<T> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> { self.0.parse(input) }
}

macro_rules! impl_tuple_fold {
    () => {};
    ($i:ident $($ident:ident)*) => {
        impl<Input: Clone, Error: ParseError<Input>, $i $(, $ident)*> TupleAnyOnce<Input, Error> for ($i, $($ident,)*)
        where
            $i: ParseOnce<Input, Error>
            $(, $ident: ParseOnce<Input, Error, Output = $i::Output>)*
        {
            type Output = <$i as ParseOnce<Input, Error>>::Output;

            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse_once(self, input: Input) -> PResult<Input, Self::Output, Error> {
                let ($i, $($ident,)*) = self;

                let err = match $i.parse_once(input.clone()) {
                    Err(self::Error::Error(e)) => e,
                    res => return res,
                };

                $(let err = match $ident.parse_once(input.clone()) {
                    Err(self::Error::Error(e)) => err.or(e),
                    res => return res
                };)*

                Err(self::Error::Error(err.append(input, ErrorKind::Any)))
            }
        }

        impl<Input: Clone, Error: ParseError<Input>, $i $(, $ident)*> TupleAnyMut<Input, Error> for ($i, $($ident,)*)
        where
            $i: ParseMut<Input, Error>
            $(, $ident: ParseMut<Input, Error, Output = $i::Output>)*
        {
            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse_mut(&mut self, input: Input) -> PResult<Input, Self::Output, Error> {
                let ($i, $($ident,)*) = self;

                let err = match $i.parse_mut(input.clone()) {
                    Err(self::Error::Error(e)) => e,
                    res => return res,
                };

                $(let err = match $ident.parse_mut(input.clone()) {
                    Err(self::Error::Error(e)) => err.or(e),
                    res => return res
                };)*

                Err(self::Error::Error(err.append(input, ErrorKind::Any)))
            }
        }

        impl<Input: Clone, Error: ParseError<Input>, $i $(, $ident)*> TupleAny<Input, Error> for ($i, $($ident,)*)
        where
            $i: Parse<Input, Error>
            $(, $ident: Parse<Input, Error, Output = $i::Output>)*
        {
            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse(&self, input: Input) -> PResult<Input, Self::Output, Error> {
                let ($i, $($ident,)*) = self;

                let err = match $i.parse(input.clone()) {
                    Err(self::Error::Error(e)) => e,
                    res => return res,
                };

                $(let err = match $ident.parse(input.clone()) {
                    Err(self::Error::Error(e)) => err.or(e),
                    res => return res
                };)*

                Err(self::Error::Error(err.append(input, ErrorKind::Any)))
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
