use crate::tag::*;
use crate::traits::*;

fn slice_contains<T: PartialEq>(slice: &[T], item: &T) -> bool {
    if slice.len() < 8 {
        slice.iter().any(|x| x == item)
    } else {
        slice.contains(item)
    }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for [T] {
    type Output = &'i [T];

    fn compare(&self, input: &'i [T]) -> (&'i [T], CompareResult<Self::Output>) {
        let min = self.len().min(input.len());

        if self.get(..min) == input.get(..min) {
            if self.len() > input.len() {
                (input, CompareResult::Incomplete)
            } else {
                let (output, input) = input.split_at(min);
                (input, CompareResult::Ok(output))
            }
        } else {
            (input, CompareResult::Error)
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for &[T] {
    type Output = &'i [T];

    fn compare(&self, input: &'i [T]) -> (&'i [T], CompareResult<Self::Output>) {
        (**self).compare(input)
    }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for T {
    type Output = &'i T;

    fn compare(&self, input: &'i [T]) -> (&'i [T], CompareResult<Self::Output>) {
        match input {
            [] => (input, CompareResult::Incomplete),
            [output, input @ ..] => {
                if output == self {
                    (input, CompareResult::Ok(output))
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for AnyOf<&[T]> {
    type Output = &'i T;

    fn compare(&self, input: &'i [T]) -> (&'i [T], CompareResult<Self::Output>) {
        match input {
            [] => (input, CompareResult::Incomplete),
            [output, input @ ..] => {
                if slice_contains(self.0, output) {
                    (input, CompareResult::Ok(output))
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for NoneOf<&[T]> {
    type Output = &'i T;

    fn compare(&self, input: &'i [T]) -> (&'i [T], CompareResult<Self::Output>) {
        match input {
            [] => (input, CompareResult::Incomplete),
            [output, input @ ..] => {
                if !slice_contains(self.0, output) {
                    (input, CompareResult::Ok(output))
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i> Compare<&'i str> for [u8] {
    type Output = &'i [u8];

    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        let min = self.len().min(input.len());

        if self.get(..min) == input.get(..min).map(str::as_bytes) {
            if self.len() > input.len() {
                (input, CompareResult::Incomplete)
            } else {
                let (output, input) = input.split_at(min);
                (input, CompareResult::Ok(output.as_bytes()))
            }
        } else {
            (input, CompareResult::Error)
        }
    }
}

impl<'i> Compare<&'i str> for str {
    type Output = &'i str;

    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        let min = self.len().min(input.len());

        if self.get(..min) == input.get(..min) {
            if self.len() > input.len() {
                (input, CompareResult::Incomplete)
            } else {
                let (output, input) = input.split_at(min);
                (input, CompareResult::Ok(output))
            }
        } else {
            (input, CompareResult::Error)
        }
    }
}

impl<'i> Compare<&'i [u8]> for str {
    type Output = &'i str;

    fn compare(&self, input: &'i [u8]) -> (&'i [u8], CompareResult<Self::Output>) {
        let (input, output) = self.as_bytes().compare(input);

        (
            input,
            output.map(|v| unsafe { std::str::from_utf8_unchecked(v) }),
        )
    }
}

impl<'i> Compare<&'i str> for &[u8] {
    type Output = &'i [u8];

    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        (**self).compare(input)
    }
}

impl<'i> Compare<&'i str> for &str {
    type Output = &'i str;

    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        (**self).compare(input)
    }
}

impl<'i> Compare<&'i [u8]> for &str {
    type Output = &'i str;

    fn compare(&self, input: &'i [u8]) -> (&'i [u8], CompareResult<Self::Output>) {
        (**self).compare(input)
    }
}

impl<'i> Compare<&'i str> for u8 {
    type Output = u8;

    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        if input.get(..1).map(str::as_bytes) == Some(&[*self]) {
            (
                unsafe { input.get_unchecked(1..) },
                CompareResult::Ok(*self),
            )
        } else {
            (input, CompareResult::Error)
        }
    }
}

impl<'i> Compare<&'i str> for AnyOf<&[u8]> {
    type Output = u8;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        match input.get(..1).map(str::as_bytes) {
            Some(&[b]) if slice_contains(self.0, &b) => {
                (unsafe { input.get_unchecked(1..) }, CompareResult::Ok(b))
            }
            _ => (input, CompareResult::Error),
        }
    }
}

impl<'i> Compare<&'i [u8]> for char {
    type Output = char;

    fn compare(&self, input: &'i [u8]) -> (&'i [u8], CompareResult<Self::Output>) {
        let s = *self;
        let mut buf = [0; 4];
        let buf = self.encode_utf8(&mut buf).as_bytes();
        let (input, output) = buf.compare(input);
        (input, output.map(move |_| s))
    }
}

impl<'i> Compare<&'i [u8]> for AnyOf<&str> {
    type Output = char;

    fn compare(&self, input: &'i [u8]) -> (&'i [u8], CompareResult<Self::Output>) {
        for c in self.0.chars() {
            let (i, c) = c.compare(input);

            match c {
                CompareResult::Ok(_) => return (i, c),
                _ => (),
            }
        }

        (input, CompareResult::Error)
    }
}

impl<'i> Compare<&'i str> for char {
    type Output = char;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        let old_input = input;
        let mut input = input.chars();
        let c = input.next();
        let input = input.as_str();
        match c {
            None => (old_input, CompareResult::Incomplete),
            Some(output) => {
                if output == *self {
                    (input, CompareResult::Ok(output))
                } else {
                    (old_input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i> Compare<&'i str> for AnyOf<&str> {
    type Output = char;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        if self.0.is_ascii() {
            let (input, output) = AnyOf(self.0.as_bytes()).compare(input);
            return (input, output.map(char::from));
        }

        let old_input = input;
        let mut input = input.chars();
        let c = input.next();
        let input = input.as_str();
        match c {
            None => (old_input, CompareResult::Incomplete),
            Some(output) => {
                if self.0.contains(output) {
                    (input, CompareResult::Ok(output))
                } else {
                    (old_input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i> Compare<&'i str> for AnyOf<&[char]> {
    type Output = char;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        let old_input = input;
        let mut input = input.chars();
        let c = input.next();
        let input = input.as_str();
        match c {
            None => (old_input, CompareResult::Incomplete),
            Some(output) => {
                if self.0.contains(&output) {
                    (input, CompareResult::Ok(output))
                } else {
                    (old_input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i> Compare<&'i str> for NoneOf<&[char]> {
    type Output = char;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, CompareResult<Self::Output>) {
        let old_input = input;
        let mut input = input.chars();
        let c = input.next();
        let input = input.as_str();
        match c {
            None => (old_input, CompareResult::Incomplete),
            Some(output) => {
                if !self.0.contains(&output) {
                    (input, CompareResult::Ok(output))
                } else {
                    (old_input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i mut [T]> for [T] {
    type Output = &'i mut [T];

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], CompareResult<Self::Output>) {
        let min = self.len().min(input.len());

        if self.get(..min) == input.get(..min) {
            if self.len() > input.len() {
                (input, CompareResult::Incomplete)
            } else {
                let (output, input) = input.split_at_mut(min);
                (input, CompareResult::Ok(output))
            }
        } else {
            (input, CompareResult::Error)
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i mut [T]> for &[T] {
    type Output = &'i mut [T];

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], CompareResult<Self::Output>) {
        (**self).compare(input)
    }
}

impl<'i, T: PartialEq> Compare<&'i mut [T]> for T {
    type Output = &'i mut T;

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], CompareResult<Self::Output>) {
        match input {
            [] => (input, CompareResult::Incomplete),
            [output, input @ ..] => {
                if output == self {
                    (input, CompareResult::Ok(output))
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i mut [T]> for AnyOf<&[T]> {
    type Output = &'i mut T;

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], CompareResult<Self::Output>) {
        match input {
            [] => (input, CompareResult::Incomplete),
            [output, input @ ..] => {
                if slice_contains(self.0, output) {
                    (input, CompareResult::Ok(output))
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i mut [T]> for NoneOf<&[T]> {
    type Output = &'i mut T;

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], CompareResult<Self::Output>) {
        match input {
            [] => (input, CompareResult::Incomplete),
            [output, input @ ..] => {
                if !slice_contains(self.0, output) {
                    (input, CompareResult::Ok(output))
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i> Compare<&'i mut str> for [u8] {
    type Output = &'i [u8];

    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        let min = self.len().min(input.len());

        if self.get(..min) == input.get(..min).map(str::as_bytes) {
            if self.len() > input.len() {
                (input, CompareResult::Incomplete)
            } else {
                let (output, input) = input.split_at_mut(min);
                (input, CompareResult::Ok(output.as_bytes()))
            }
        } else {
            (input, CompareResult::Error)
        }
    }
}

impl<'i> Compare<&'i mut str> for str {
    type Output = &'i mut str;

    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        let min = self.len().min(input.len());

        if self.get(..min) == input.get(..min) {
            if self.len() > input.len() {
                (input, CompareResult::Incomplete)
            } else {
                let (output, input) = input.split_at_mut(min);
                (input, CompareResult::Ok(output))
            }
        } else {
            (input, CompareResult::Error)
        }
    }
}

impl<'i> Compare<&'i mut [u8]> for str {
    type Output = &'i mut str;

    fn compare(&self, input: &'i mut [u8]) -> (&'i mut [u8], CompareResult<Self::Output>) {
        let (input, output) = self.as_bytes().compare(input);

        (
            input,
            output.map(|v| unsafe { std::str::from_utf8_unchecked_mut(v) }),
        )
    }
}

impl<'i> Compare<&'i mut str> for &[u8] {
    type Output = &'i [u8];

    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        (**self).compare(input)
    }
}

impl<'i> Compare<&'i mut str> for &str {
    type Output = &'i mut str;

    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        (**self).compare(input)
    }
}

impl<'i> Compare<&'i mut [u8]> for &str {
    type Output = &'i mut str;

    fn compare(&self, input: &'i mut [u8]) -> (&'i mut [u8], CompareResult<Self::Output>) {
        (**self).compare(input)
    }
}

impl<'i> Compare<&'i mut str> for u8 {
    type Output = u8;

    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        if input.get(..1).map(str::as_bytes) == Some(&[*self]) {
            (
                unsafe { input.get_unchecked_mut(1..) },
                CompareResult::Ok(*self),
            )
        } else {
            (input, CompareResult::Error)
        }
    }
}

impl<'i> Compare<&'i mut str> for AnyOf<&[u8]> {
    type Output = u8;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        match input.get(..1).map(str::as_bytes) {
            Some(&[b]) if slice_contains(self.0, &b) => (
                unsafe { input.get_unchecked_mut(1..) },
                CompareResult::Ok(b),
            ),
            _ => (input, CompareResult::Error),
        }
    }
}

impl<'i> Compare<&'i mut [u8]> for char {
    type Output = char;

    fn compare(&self, input: &'i mut [u8]) -> (&'i mut [u8], CompareResult<Self::Output>) {
        let s = *self;
        let mut buf = [0; 4];
        let buf = self.encode_utf8(&mut buf).as_bytes();
        let (input, output) = buf.compare(input);
        (input, output.map(move |_| s))
    }
}

impl<'i> Compare<&'i mut [u8]> for AnyOf<&str> {
    type Output = char;

    fn compare(&self, mut input: &'i mut [u8]) -> (&'i mut [u8], CompareResult<Self::Output>) {
        for c in self.0.chars() {
            let (i, c) = c.compare(input);

            match c {
                CompareResult::Ok(_) => return (i, c),
                _ => input = i,
            }
        }

        (input, CompareResult::Error)
    }
}

impl<'i> Compare<&'i mut str> for char {
    type Output = char;

    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        match input.chars().next() {
            None => (input, CompareResult::Incomplete),
            Some(output) => {
                if output == *self {
                    (
                        unsafe { input.get_unchecked_mut(output.len_utf8()..) },
                        CompareResult::Ok(output),
                    )
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i> Compare<&'i mut str> for AnyOf<&str> {
    type Output = char;

    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        if self.0.is_ascii() {
            let (input, output) = AnyOf(self.0.as_bytes()).compare(input);
            return (input, output.map(char::from));
        }

        match input.chars().next() {
            None => (input, CompareResult::Incomplete),
            Some(output) => {
                if self.0.contains(output) {
                    (
                        unsafe { input.get_unchecked_mut(output.len_utf8()..) },
                        CompareResult::Ok(output),
                    )
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i> Compare<&'i mut str> for AnyOf<&[char]> {
    type Output = char;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        match input.chars().next() {
            None => (input, CompareResult::Incomplete),
            Some(output) => {
                if self.0.contains(&output) {
                    (
                        unsafe { input.get_unchecked_mut(output.len_utf8()..) },
                        CompareResult::Ok(output),
                    )
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}

impl<'i> Compare<&'i mut str> for NoneOf<&[char]> {
    type Output = char;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, CompareResult<Self::Output>) {
        match input.chars().next() {
            None => (input, CompareResult::Incomplete),
            Some(output) => {
                if !self.0.contains(&output) {
                    (
                        unsafe { input.get_unchecked_mut(output.len_utf8()..) },
                        CompareResult::Ok(output),
                    )
                } else {
                    (input, CompareResult::Error)
                }
            }
        }
    }
}
