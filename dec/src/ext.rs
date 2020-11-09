use crate::error::*;
use crate::traits::*;

cfg_match::cfg_match! {
    feature = "nightly" => {
        impl<F: ?Sized + FnOnce(I) -> PResult<I, O, E>, O, I, E: ParseError<I>> ParseOnce<I, E> for F {
            type Output = O;

            fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
                self(input)
            }
        }

        impl<F: ?Sized + FnMut(I) -> PResult<I, O, E>, O, I, E: ParseError<I>> ParseMut<I, E> for F {
            fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
                self(input)
            }
        }

        impl<F: ?Sized + Fn(I) -> PResult<I, O, E>, O, I, E: ParseError<I>> Parse<I, E> for F {
            fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
                self(input)
            }
        }
    }
    _ => {
        impl<F: FnOnce(I) -> PResult<I, O, E>, O, I, E: ParseError<I>> ParseOnce<I, E> for F {
            type Output = O;

            fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
                self(input)
            }
        }

        impl<F: FnMut(I) -> PResult<I, O, E>, O, I, E: ParseError<I>> ParseMut<I, E> for F {
            fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
                self(input)
            }
        }

        impl<F: Fn(I) -> PResult<I, O, E>, O, I, E: ParseError<I>> Parse<I, E> for F {
            fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
                self(input)
            }
        }
    }
}

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
    fn clone(&self) -> Self {
        *self
    }
}

cfg_match::cfg_match! {
    feature = "nightly" => {
        impl<T: ?Sized + ParseOnce<I, E>, I, E: ParseError<I>> ParseOnce<I, E> for Own<T> {
            type Output = T::Output;

            fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
                self.0.parse_once(input)
            }
        }

        impl<T: ?Sized + ParseMut<I, E>, I, E: ParseError<I>> ParseMut<I, E> for Own<T> {
            fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
                self.0.parse_mut(input)
            }
        }

        impl<T: ?Sized + Parse<I, E>, I, E: ParseError<I>> Parse<I, E> for Own<T> {
            fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
                self.0.parse(input)
            }
        }
    }
    _ => {
        impl<T: ParseOnce<I, E>, I, E: ParseError<I>> ParseOnce<I, E> for Own<T> {
            type Output = T::Output;

            fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
                self.0.parse_once(input)
            }
        }

        impl<T: ParseMut<I, E>, I, E: ParseError<I>> ParseMut<I, E> for Own<T> {
            fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
                self.0.parse_mut(input)
            }
        }

        impl<T: Parse<I, E>, I, E: ParseError<I>> Parse<I, E> for Own<T> {
            fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
                self.0.parse(input)
            }
        }
    }
}

impl<T: ?Sized + ParseMut<I, E>, I, E: ParseError<I>> ParseOnce<I, E> for Mut<'_, T> {
    type Output = T::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        self.0.parse_mut(input)
    }
}

impl<T: ?Sized + ParseMut<I, E>, I, E: ParseError<I>> ParseMut<I, E> for Mut<'_, T> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        self.0.parse_mut(input)
    }
}

impl<T: ?Sized + Parse<I, E>, I, E: ParseError<I>> Parse<I, E> for Mut<'_, T> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        self.0.parse(input)
    }
}

impl<T: ?Sized + Parse<I, E>, I, E: ParseError<I>> ParseOnce<I, E> for Ref<'_, T> {
    type Output = T::Output;

    fn parse_once(self, input: I) -> PResult<I, Self::Output, E> {
        self.0.parse(input)
    }
}

impl<T: ?Sized + Parse<I, E>, I, E: ParseError<I>> ParseMut<I, E> for Ref<'_, T> {
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        self.0.parse(input)
    }
}

impl<T: ?Sized + Parse<I, E>, I, E: ParseError<I>> Parse<I, E> for Ref<'_, T> {
    fn parse(&self, input: I) -> PResult<I, Self::Output, E> {
        self.0.parse(input)
    }
}
