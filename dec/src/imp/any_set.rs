use dec_core::{
    error::{Error, PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AnySet<T>(pub T);

pub trait TupleAnySetOnce<I, E, Fail> {
    type Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail>;
}

pub trait TupleAnySetMut<I, E, Fail>: TupleAnySetOnce<I, E, Fail> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail>;
}

pub trait TupleAnySet<I, E, Fail>: TupleAnySetMut<I, E, Fail> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail>;
}

impl<T: TupleAnySetOnce<I, E, Fail>, I, E: ParseError<I>, Fail> ParseOnce<I, E, Fail> for AnySet<T> {
    type Output = T::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> { self.0.parse_once(input) }
}

impl<T: TupleAnySetMut<I, E, Fail>, I, E: ParseError<I>, Fail> ParseMut<I, E, Fail> for AnySet<T> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { self.0.parse_mut(input) }
}

impl<T: TupleAnySet<I, E, Fail>, I, E: ParseError<I>, Fail> Parse<I, E, Fail> for AnySet<T> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> { self.0.parse(input) }
}

macro_rules! impl_tuple_fold_imp {
    ($($ident:ident)*) => {
        impl<Input: Clone, Error: ParseError<Input>, Fail $(, $ident)*> TupleAnySetOnce<Input, Error, Fail> for ($($ident,)*)
        where
            $($ident: ParseMut<Input, Error, Fail>,)*
        {
            type Output = ($(Option<<$ident as ParseOnce<Input, Error, Fail>>::Output>,)*);

            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse_once(self, mut input: Input) -> PResult<Input, Self::Output, Error, Fail> {
                let ($($ident,)*) = self;

                $(
                    let mut $ident = Ok($ident);
                )*

                loop {
                    $(
                        match $ident {
                            Err(_) => (),
                            Ok(ref mut parser) => {
                                match parser.parse_mut(input.clone()) {
                                    Ok((next_input, value)) => {
                                        input = next_input;
                                        $ident = Err(value);
                                        continue
                                    },
                                    Err(self::Error::Error(_)) => (),
                                    Err(self::Error::Failure(err)) => return Err(self::Error::Failure(err)),
                                }
                            }
                        }
                    )*

                    break Ok((input, ($($ident.err(),)*)))
                }
            }
        }

        impl<Input: Clone, Error: ParseError<Input>, Fail $(, $ident)*> TupleAnySetMut<Input, Error, Fail> for ($($ident,)*)
        where
            $($ident: ParseMut<Input, Error, Fail>,)*
        {
            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse_mut(&mut self, mut input: Input) -> PResult<Input, Self::Output, Error, Fail> {
                let ($($ident,)*) = self;
                TupleAnySetOnce::parse_once(($($ident.by_mut(),)*), input)
            }
        }

        impl<Input: Clone, Error: ParseError<Input>, Fail $(, $ident)*> TupleAnySet<Input, Error, Fail> for ($($ident,)*)
        where
            $($ident: Parse<Input, Error, Fail>,)*
        {
            #[inline]
            #[allow(unused_mut, non_snake_case)]
            fn parse(&self, mut input: Input) -> PResult<Input, Self::Output, Error, Fail> {
                let ($($ident,)*) = self;
                TupleAnySetOnce::parse_once(($($ident.by_ref(),)*), input)
            }
        }
    };
}

macro_rules! impl_tuple_fold {
    () => {
        impl_tuple_fold_imp!{}
    };
    ($i:ident $($ident:ident)*) => {
        impl_tuple_fold!{$($ident)*}
        impl_tuple_fold_imp!{$i $($ident)*}
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
