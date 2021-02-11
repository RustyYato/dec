use crate::{
    error::{CaptureInput, Error, ErrorKind, PResult, ParseError},
    Parse, ParseMut, ParseOnce, ParseTag, Tag,
};

#[cfg(feature = "alloc")]
use std::boxed::Box;

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AnyOf<T>(pub T);

#[must_use = "parsers are lazy and do nothing unless consumed"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NoneOf<T>(pub T);

trait Slice: Sized {
    unsafe fn split_at_unchecked(self, index: usize) -> (Self, Self);
}

trait NormSlice: Slice {
    type Item;

    unsafe fn split_first_unchecked(self) -> (Self::Item, Self);
}

trait StrSlice: Sized {
    type Bytes;

    #[allow(clippy::wrong_self_convention)]
    unsafe fn to_bytes(self) -> Self::Bytes;

    unsafe fn from_utf8_unchecked(bytes: Self::Bytes) -> Self;
}

impl<'a, T> Slice for &'a [T] {
    #[inline]
    unsafe fn split_at_unchecked(self, index: usize) -> (Self, Self) {
        let ptr = self.as_ptr();
        (
            core::slice::from_raw_parts(ptr, index),
            core::slice::from_raw_parts(ptr.add(index), self.len() - index),
        )
    }
}

impl<'a, T> Slice for &'a mut [T] {
    #[inline]
    unsafe fn split_at_unchecked(self, index: usize) -> (Self, Self) {
        let ptr = self.as_mut_ptr();
        (
            core::slice::from_raw_parts_mut(ptr, index),
            core::slice::from_raw_parts_mut(ptr.add(index), self.len() - index),
        )
    }
}

impl<'a, T> NormSlice for &'a [T] {
    type Item = &'a T;

    #[inline]
    unsafe fn split_first_unchecked(self) -> (Self::Item, Self) {
        let (first, rest) = self.split_at_unchecked(1);
        (&*(first as *const [T] as *const T), rest)
    }
}

impl<'a, T> NormSlice for &'a mut [T] {
    type Item = &'a mut T;

    #[inline]
    unsafe fn split_first_unchecked(self) -> (Self::Item, Self) {
        let (first, rest) = self.split_at_unchecked(1);
        (&mut *(first as *mut [T] as *mut T), rest)
    }
}

impl<'a> Slice for &'a str {
    #[inline]
    unsafe fn split_at_unchecked(self, index: usize) -> (Self, Self) {
        let ptr = self.as_ptr();
        (
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(ptr, index)),
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(ptr.add(index), self.len() - index)),
        )
    }
}

impl<'a> Slice for &'a mut str {
    #[inline]
    unsafe fn split_at_unchecked(self, index: usize) -> (Self, Self) {
        let ptr = self.as_mut_ptr();
        (
            core::str::from_utf8_unchecked_mut(core::slice::from_raw_parts_mut(ptr, index)),
            core::str::from_utf8_unchecked_mut(core::slice::from_raw_parts_mut(ptr.add(index), self.len() - index)),
        )
    }
}

impl<'a> StrSlice for &'a str {
    type Bytes = &'a [u8];

    #[inline]
    unsafe fn to_bytes(self) -> Self::Bytes { self.as_bytes() }

    #[inline]
    unsafe fn from_utf8_unchecked(bytes: Self::Bytes) -> Self { core::str::from_utf8_unchecked(bytes) }
}

impl<'a> StrSlice for &'a mut str {
    type Bytes = &'a mut [u8];

    #[inline]
    unsafe fn to_bytes(self) -> Self::Bytes { self.as_bytes_mut() }

    #[inline]
    unsafe fn from_utf8_unchecked(bytes: Self::Bytes) -> Self { core::str::from_utf8_unchecked_mut(bytes) }
}

macro_rules! tag_impl {
    (fn[$($generics:tt)*]($self:ident: $Self:ty) {
        $($body:tt)*
    }) => {};
    (fn[$($generics:tt)*]($self:ident: $Self:ty, $input:ident: $Input:ty => $Output:ty $(, $($rest:tt)*)?) {
        $($body:tt)*
    }) => {
        impl<$($generics)*, ErrorTy: ParseError<$Input>, FailureTy> ParseOnce<$Input, ErrorTy, FailureTy> for $Self {
            type Output = $Output;

            #[inline]
            fn parse_once($self, $input: $Input) -> PResult<$Input, Self::Output, ErrorTy, FailureTy> {
                $($body)*
            }
        }

        impl<$($generics)*, ErrorTy: ParseError<$Input>, FailureTy> ParseMut<$Input, ErrorTy, FailureTy> for $Self {
            #[inline]
            fn parse_mut(&mut self, input: $Input) -> PResult<$Input, Self::Output, ErrorTy, FailureTy> {
                self.parse_once(input)
            }
        }

        impl<$($generics)*, ErrorTy: ParseError<$Input>, FailureTy> Parse<$Input, ErrorTy, FailureTy> for $Self {
            #[inline]
            fn parse(&self, input: $Input) -> PResult<$Input, Self::Output, ErrorTy, FailureTy> {
                self.parse_once(input)
            }
        }

        impl<$($generics)*> Tag<$Input> for $Self {
            type Output = $Output;
            #[inline]
            fn parse_tag(&self, input: $Input) -> PResult<$Input, Self::Output, CaptureInput<$Input>, core::convert::Infallible> {
                self.parse_once(input)
            }
        }

        tag_impl! { fn[$($generics)*]($self: $Self $(, $($rest)*)?) { $($body)* } }
    };
    (NOCOPY fn[$($generics:tt)*]($self:ident: $Self:ty) {
        $($body:tt)*
    }) => {};
    (NOCOPY fn[$($generics:tt)*]($self:ident: $Self:ty, $input:ident: $Input:ty => $Output:ty $(, $($rest:tt)*)?) {
        $($body:tt)*
    }) => {
        impl<$($generics)*, ErrorTy: ParseError<$Input>, FailureTy> ParseOnce<$Input, ErrorTy, FailureTy> for $Self {
            type Output = $Output;

            #[inline]
            fn parse_once(self, input: $Input) -> PResult<$Input, Self::Output, ErrorTy, FailureTy> {
                self.parse(input)
            }
        }

        impl<$($generics)*, ErrorTy: ParseError<$Input>, FailureTy> ParseMut<$Input, ErrorTy, FailureTy> for $Self {
            #[inline]
            fn parse_mut(&mut self, input: $Input) -> PResult<$Input, Self::Output, ErrorTy, FailureTy> {
                self.parse(input)
            }
        }

        impl<$($generics)*, ErrorTy: ParseError<$Input>, FailureTy> Parse<$Input, ErrorTy, FailureTy> for $Self {
            #[inline]
            fn parse(&$self, $input: $Input) -> PResult<$Input, Self::Output, ErrorTy, FailureTy> {
                $($body)*
            }
        }

        impl<$($generics)*> Tag<$Input> for $Self {
            type Output = $Output;
            #[inline]
            fn parse_tag(&self, input: $Input) -> PResult<$Input, Self::Output, CaptureInput<$Input>, core::convert::Infallible> {
                self.parse(input)
            }
        }

        tag_impl! { NOCOPY fn[$($generics)*]($self: $Self $(, $($rest)*)?) { $($body)* } }
    };
}

macro_rules! tag {
    ($(fn[$($generics:tt)*]($self:ident: $Self:ty $(, $input:ident: $Input:ty => $Output:ty)+ $(,)?) {
        $($body:tt)*
    })*) => {$(
        tag_impl! { fn[$($generics)*]($self: $Self $(, $input: $Input => $Output)*) { $($body)* } }
    )*};

    (NOCOPY $(fn[$($generics:tt)*]($self:ident: $Self:ty $(, $input:ident: $Input:ty => $Output:ty)+ $(,)?) {
        $($body:tt)*
    })*) => {$(
        tag_impl! { NOCOPY fn[$($generics)*]($self: $Self $(, $input: $Input => $Output)*) { $($body)* } }
    )*};

    (NOPARSE $(fn[$($generics:tt)*]($self:ident: $Self:ty $(, $input:ident: $Input:ty => $Output:ty)+ $(,)?) {
        $($body:tt)*
    })*) => {$(
        tag_impl! { NOPARSE fn[$($generics)*]($self: $Self $(, $input: $Input => $Output)*) { $($body)* } }
    )*};
}

impl<'input, T: PartialEq> ParseTag<T> for &'input [T] {
    type Output = &'input T;

    #[inline]
    fn parse_tag(self, tag: &T) -> PResult<Self, Self::Output, CaptureInput<Self>, core::convert::Infallible> {
        if self.get(0) == Some(tag) {
            let (matched, input) = unsafe { NormSlice::split_first_unchecked(self) };
            Ok((input, matched))
        } else {
            Err(Error::Error(ParseError::from_input_kind(self, ErrorKind::Tag)))
        }
    }
}

impl<'input, T: PartialEq> ParseTag<T> for &'input mut [T] {
    type Output = &'input mut T;

    #[inline]
    fn parse_tag(self, tag: &T) -> PResult<Self, Self::Output, CaptureInput<Self>, core::convert::Infallible> {
        if self.get(0) == Some(tag) {
            let (matched, input) = unsafe { NormSlice::split_first_unchecked(self) };
            Ok((input, matched))
        } else {
            Err(Error::Error(ParseError::from_input_kind(self, ErrorKind::Tag)))
        }
    }
}

#[cfg(feature = "alloc")]
tag! {
    NOCOPY
    fn['input, T: PartialEq](self: Box<[T]>, input: &'input [T] => &'input [T], input: &'input mut [T] => &'input mut [T]) {
        if input.get(..self.len()) == Some(&*self) {
            let (matched, input) = unsafe {
                Slice::split_at_unchecked(input, self.len())
            };
            Ok((input, matched))
        } else {
            Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
        }
    }

    fn['input](self: Box<str>, input: &'input str => &'input str, input: &'input mut str => &'input mut str) {
        if input.get(..self.len()) == Some(&*self) {
            let (matched, input) = unsafe {
                Slice::split_at_unchecked(input, self.len())
            };
            Ok((input, matched))
        } else {
            Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
        }
    }

    fn['input](self: Box<str>, input: &'input [u8] => &'input str, input: &'input mut [u8] => &'input mut str) {
        if input.get(..self.len()) == Some(self.as_bytes()) {
            let (matched, input) = unsafe {
                Slice::split_at_unchecked(input, self.len())
            };
            Ok((input, unsafe { StrSlice::from_utf8_unchecked(matched) }))
        } else {
            Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
        }
    }
}

tag! {
    fn['input, T: PartialEq](self: &[T], input: &'input [T] => &'input [T], input: &'input mut [T] => &'input mut [T]) {
        if input.get(..self.len()) == Some(self) {
            let (matched, input) = unsafe {
                Slice::split_at_unchecked(input, self.len())
            };
            Ok((input, matched))
        } else {
            Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
        }
    }

    fn['input](self: &str, input: &'input str => &'input str, input: &'input mut str => &'input mut str) {
        if input.get(..self.len()) == Some(self) {
            let (matched, input) = unsafe {
                Slice::split_at_unchecked(input, self.len())
            };
            Ok((input, matched))
        } else {
            Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
        }
    }

    fn['input](self: &str, input: &'input [u8] => &'input str, input: &'input mut [u8] => &'input mut str) {
        if input.get(..self.len()) == Some(self.as_bytes()) {
            let (matched, input) = unsafe {
                Slice::split_at_unchecked(input, self.len())
            };
            Ok((input, unsafe { StrSlice::from_utf8_unchecked(matched) }))
        } else {
            Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
        }
    }

    fn['input](self: char, input: &'input str => &'input str, input: &'input mut str => &'input mut str) {
        let len = self.len_utf8();
        let mut bytes = [0; 4];
        self.encode_utf8(&mut bytes);

        if input.get(..len).map(str::as_bytes) == Some(unsafe { bytes.get_unchecked(..len) }) {
            let (matched, input) = unsafe {
                Slice::split_at_unchecked(input, len)
            };
            Ok((input, matched))
        } else {
            Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
        }
    }

    fn['input](self: char, input: &'input [u8] => &'input str, input: &'input mut [u8] => &'input mut str) {
        let len = self.len_utf8();
        let mut bytes = [0; 4];
        self.encode_utf8(&mut bytes);

        if input.get(..len) == Some(unsafe { bytes.get_unchecked(..len) }) {
            let (matched, input) = unsafe {
                Slice::split_at_unchecked(input, len)
            };
            Ok((input, unsafe { StrSlice::from_utf8_unchecked(matched) }))
        } else {
            Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
        }
    }

    fn['input](self: u8, input: &'input str => (u8, &'input str), input: &'input mut str => (u8, &'input mut str)) {
        if input.get(..1).map(str::as_bytes) == Some(&[self]) {
            let (matched, input) = unsafe {
                Slice::split_at_unchecked(input, 1)
            };
            Ok((input, (self, matched)))
        } else {
            Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
        }
    }

    fn['input, T: PartialEq](self: AnyOf<&[T]>, input: &'input [T] => &'input T, input: &'input mut [T] => &'input mut T) {
        if let Some(first) = input.get(0) {
            if self.0.contains(first) {
                let (matched, input) = unsafe {
                    NormSlice::split_first_unchecked(input)
                };
                return Ok((input, matched))
            }
        }

        Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
    }

    fn['input, T: PartialEq](self: NoneOf<&[T]>, input: &'input [T] => &'input T, input: &'input mut [T] => &'input mut T) {
        if let Some(first) = input.get(0) {
            if !self.0.contains(first) {
                let (matched, input) = unsafe {
                    NormSlice::split_first_unchecked(input)
                };
                return Ok((input, matched))
            }
        }

        Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
    }

    fn['input](self: AnyOf<&[char]>, input: &'input str => (char, &'input str), input: &'input mut str => (char, &'input mut str)) {
        if let Some(first) = input.chars().next() {
            if self.0.contains(&first) {
                let (matched, input) = unsafe {
                    Slice::split_at_unchecked(input, first.len_utf8())
                };
                return Ok((input, (first, matched)))
            }
        }

        Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
    }

    fn['input](self: NoneOf<&[char]>, input: &'input str => (char, &'input str), input: &'input mut str => (char, &'input mut str)) {
        if let Some(first) = input.chars().next() {
            if !self.0.contains(&first) {
                let (matched, input) = unsafe {
                    Slice::split_at_unchecked(input, first.len_utf8())
                };
                return Ok((input, (first, matched)))
            }
        }

        Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
    }

    fn['input](self: AnyOf<&str>, input: &'input str => (char, &'input str), input: &'input mut str => (char, &'input mut str)) {
        if let Some(first) = input.chars().next() {
            if self.0.contains(first) {
                let (matched, input) = unsafe {
                    Slice::split_at_unchecked(input, first.len_utf8())
                };
                return Ok((input, (first, matched)))
            }
        }

        Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
    }

    fn['input](self: NoneOf<&str>, input: &'input str => (char, &'input str), input: &'input mut str => (char, &'input mut str)) {
        if let Some(first) = input.chars().next() {
            if !self.0.contains(first) {
                let (matched, input) = unsafe {
                    Slice::split_at_unchecked(input, first.len_utf8())
                };
                return Ok((input, (first, matched)))
            }
        }

        Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
    }

    fn['input](self: AnyOf<&[u8]>, input: &'input str => (u8, &'input str), input: &'input mut str => (u8, &'input mut str)) {
        match input.get(..1).map(str::as_bytes) {
            Some(&[byte]) => {
                if self.0.contains(&byte) {
                    let (matched, input) = unsafe {
                        Slice::split_at_unchecked(input, 1)
                    };
                    return Ok((input, (byte, matched)))
                }
            }
            Some(_) => unsafe { core::hint::unreachable_unchecked() },
            None => ()
        }

        Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
    }

    fn['input](self: NoneOf<&[u8]>, input: &'input str => (u8, &'input str), input: &'input mut str => (u8, &'input mut str)) {
        match input.get(..1).map(str::as_bytes) {
            Some(&[byte]) => {
                if !self.0.contains(&byte) {
                    let (matched, input) = unsafe {
                        Slice::split_at_unchecked(input, 1)
                    };
                    return Ok((input, (byte, matched)))
                }
            }
            Some(_) => unsafe { core::hint::unreachable_unchecked() },
            None => ()
        }

        Err(Error::Error(ParseError::from_input_kind(input, ErrorKind::Tag)))
    }
}
