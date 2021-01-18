use crate::{
    error::{PResult, ParseError},
    Parse, ParseExt, ParseMut, ParseOnce,
};

#[cfg(feature = "alloc")]
use std::boxed::Box;

cfg_match::cfg_match! {
    // TODO: remove FALSE when unsized_locals works again
    all(FALSE, feature = "nightly") => {
        impl<F: ?Sized + FnOnce(I) -> PResult<I, O, E, Fail>, O, I, E: ParseError<I>, Fail> ParseOnce<I, E, Fail> for F {
            type Output = O;

            fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self(input)
            }
        }

        impl<F: ?Sized + FnMut(I) -> PResult<I, O, E, Fail>, O, I, E: ParseError<I>, Fail> ParseMut<I, E, Fail> for F {
            fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self(input)
            }
        }

        impl<F: ?Sized + Fn(I) -> PResult<I, O, E, Fail>, O, I, E: ParseError<I>, Fail> Parse<I, E, Fail> for F {
            fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self(input)
            }
        }
    }
    _ => {
        impl<F: FnOnce(I) -> PResult<I, O, E, Fail>, O, I, E: ParseError<I>, Fail> ParseOnce<I, E, Fail> for F {
            type Output = O;

            fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self(input)
            }
        }

        impl<F: FnMut(I) -> PResult<I, O, E, Fail>, O, I, E: ParseError<I>, Fail> ParseMut<I, E, Fail> for F {
            fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self(input)
            }
        }

        impl<F: Fn(I) -> PResult<I, O, E, Fail>, O, I, E: ParseError<I>, Fail> Parse<I, E, Fail> for F {
            fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self(input)
            }
        }
    }
}

#[cfg(feature = "alloc")]
#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Own<T: ?Sized>(pub Box<T>);
#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Mut<'a, T: ?Sized>(pub &'a mut T);
#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ref<'a, T: ?Sized>(pub &'a T);

impl<T: ?Sized> Copy for Ref<'_, T> {}
impl<T: ?Sized> Clone for Ref<'_, T> {
    fn clone(&self) -> Self { *self }
}

#[cfg(feature = "alloc")]
cfg_match::cfg_match! {
    feature = "nightly" => {
        impl<T: ?Sized + ParseOnce<I, E, Fail>, I, E: ParseError<I>, Fail> ParseOnce<I, E, Fail> for Own<T> {
            type Output = T::Output;

            fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self.0.parse_once(input)
            }
        }

        impl<T: ?Sized + ParseMut<I, E, Fail>, I, E: ParseError<I>, Fail> ParseMut<I, E, Fail> for Own<T> {
            fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self.0.parse_mut(input)
            }
        }

        impl<T: ?Sized + Parse<I, E, Fail>, I, E: ParseError<I>, Fail> Parse<I, E, Fail> for Own<T> {
            fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self.0.parse(input)
            }
        }
    }
    _ => {
        impl<T: ParseOnce<I, E, Fail>, I, E: ParseError<I>, Fail> ParseOnce<I, E, Fail> for Own<T> {
            type Output = T::Output;

            fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self.0.parse_once(input)
            }
        }

        impl<T: ParseMut<I, E, Fail>, I, E: ParseError<I>, Fail> ParseMut<I, E, Fail> for Own<T> {
            fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self.0.parse_mut(input)
            }
        }

        impl<T: Parse<I, E, Fail>, I, E: ParseError<I>, Fail> Parse<I, E, Fail> for Own<T> {
            fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
                self.0.parse(input)
            }
        }
    }
}

impl<T: ?Sized + ParseMut<I, E, Fail>, I, E: ParseError<I>, Fail> ParseOnce<I, E, Fail> for Mut<'_, T> {
    type Output = T::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> { self.0.parse_mut(input) }
}

impl<T: ?Sized + ParseMut<I, E, Fail>, I, E: ParseError<I>, Fail> ParseMut<I, E, Fail> for Mut<'_, T> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { self.0.parse_mut(input) }
}

impl<T: ?Sized + Parse<I, E, Fail>, I, E: ParseError<I>, Fail> Parse<I, E, Fail> for Mut<'_, T> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> { self.0.parse(input) }
}

impl<T: ?Sized + Parse<I, E, Fail>, I, E: ParseError<I>, Fail> ParseOnce<I, E, Fail> for Ref<'_, T> {
    type Output = T::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> { self.0.parse(input) }
}

impl<T: ?Sized + Parse<I, E, Fail>, I, E: ParseError<I>, Fail> ParseMut<I, E, Fail> for Ref<'_, T> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> { self.0.parse(input) }
}

impl<T: ?Sized + Parse<I, E, Fail>, I, E: ParseError<I>, Fail> Parse<I, E, Fail> for Ref<'_, T> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> { self.0.parse(input) }
}

impl<T: ParseOnce<I, E, Fail>, I, E: ParseError<I>, Fail> ParseOnce<I, E, Fail> for Option<T> {
    type Output = Option<T::Output>;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E, Fail> {
        match self {
            None => Ok((input, None)),
            Some(inner) => match inner.parse_once(input) {
                Ok((input, value)) => Ok((input, Some(value))),
                Err(err) => Err(err),
            },
        }
    }
}

impl<T: ParseMut<I, E, Fail>, I, E: ParseError<I>, Fail> ParseMut<I, E, Fail> for Option<T> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, Fail> {
        self.as_mut().map(ParseExt::by_mut).parse_once(input)
    }
}

impl<T: Parse<I, E, Fail>, I, E: ParseError<I>, Fail> Parse<I, E, Fail> for Option<T> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E, Fail> {
        self.as_ref().map(ParseExt::by_ref).parse_once(input)
    }
}
