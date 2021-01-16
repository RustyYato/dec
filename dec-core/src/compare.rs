use crate::Compare;

#[must_use = "AnyOf does nothing on it's own, and must be used with Tag"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AnyOf<T>(pub T);

#[must_use = "NoneOf does nothing on it's own, and must be used with Tag"]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NoneOf<T>(pub T);

unsafe fn split_at_unchecked(input: &str, index: usize) -> (&str, Option<&str>) {
    let ptr = input.as_ptr();
    (
        &*(core::ptr::slice_from_raw_parts(ptr, input.len() - index) as *const str),
        Some(&*(core::ptr::slice_from_raw_parts(ptr, index) as *const str)),
    )
}

unsafe fn split_at_unchecked_mut(input: &mut str, index: usize) -> (&mut str, Option<&mut str>) {
    let ptr = input.as_mut_ptr();
    (
        &mut *(core::ptr::slice_from_raw_parts_mut(ptr, input.len() - index) as *mut str),
        Some(&mut *(core::ptr::slice_from_raw_parts_mut(ptr, index) as *mut str)),
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CharRef<T>(T);

impl<T: AsRef<str>> CharRef<T> {
    pub fn char(&self) -> char {
        match self.0.as_ref().chars().next() {
            Some(c) => c,
            None => unsafe { core::hint::unreachable_unchecked() },
        }
    }
}

impl<T> CharRef<T> {
    pub fn into_inner(self) -> T { self.0 }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for [T] {
    type Output = &'i [T];

    fn compare(&self, input: &'i [T]) -> (&'i [T], Option<Self::Output>) {
        if input.get(..self.len()) == Some(self) {
            let (output, input) = input.split_at(self.len());
            (input, Some(output))
        } else {
            (input, None)
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for &[T] {
    type Output = &'i [T];

    fn compare(&self, input: &'i [T]) -> (&'i [T], Option<Self::Output>) { (**self).compare(input) }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for T {
    type Output = &'i T;

    fn compare(&self, input: &'i [T]) -> (&'i [T], Option<Self::Output>) {
        match input {
            [output, input @ ..] if output == self => (input, Some(output)),
            _ => (input, None),
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for AnyOf<&[T]> {
    type Output = &'i T;

    fn compare(&self, input: &'i [T]) -> (&'i [T], Option<Self::Output>) {
        match input {
            [first, input @ ..] if self.0.contains(first) => (input, Some(first)),
            _ => (input, None),
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i [T]> for NoneOf<&[T]> {
    type Output = &'i T;

    fn compare(&self, input: &'i [T]) -> (&'i [T], Option<Self::Output>) {
        match input {
            [first, input @ ..] if !self.0.contains(first) => (input, Some(first)),
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i str> for str {
    type Output = &'i str;

    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) {
        if input.get(..self.len()) == Some(self) {
            let (output, input) = input.split_at(self.len());
            (input, Some(output))
        } else {
            (input, None)
        }
    }
}

impl<'i> Compare<&'i [u8]> for str {
    type Output = &'i str;

    fn compare(&self, input: &'i [u8]) -> (&'i [u8], Option<Self::Output>) {
        let (input, output) = self.as_bytes().compare(input);

        (input, output.map(|v| unsafe { std::str::from_utf8_unchecked(v) }))
    }
}

impl<'i> Compare<&'i str> for &str {
    type Output = &'i str;

    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) { (**self).compare(input) }
}

impl<'i> Compare<&'i [u8]> for &str {
    type Output = &'i str;

    fn compare(&self, input: &'i [u8]) -> (&'i [u8], Option<Self::Output>) { (**self).compare(input) }
}

impl<'i> Compare<&'i str> for u8 {
    type Output = &'i str;

    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) {
        if input.get(..1).map(str::as_bytes) == Some(&[*self]) {
            unsafe { split_at_unchecked(input, 1) }
        } else {
            (input, None)
        }
    }
}

impl<'i> Compare<&'i str> for AnyOf<&[u8]> {
    type Output = &'i str;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) {
        if !input.is_char_boundary(1) {
            return (input, None)
        }

        match input.get(..1).map(str::as_bytes) {
            Some(&[b]) if self.0.contains(&b) => unsafe { split_at_unchecked(input, 1) },
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i str> for NoneOf<&[u8]> {
    type Output = &'i str;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) {
        if !input.is_char_boundary(1) {
            return (input, None)
        }

        match input.get(..1).map(str::as_bytes) {
            Some(&[b]) if !self.0.contains(&b) => unsafe { split_at_unchecked(input, 1) },
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i [u8]> for char {
    type Output = &'i [u8];

    fn compare(&self, input: &'i [u8]) -> (&'i [u8], Option<Self::Output>) {
        let mut buf = [0; 4];
        let buf = self.encode_utf8(&mut buf).as_bytes();
        let (input, output) = buf.compare(input);
        (input, output)
    }
}

impl<'i> Compare<&'i str> for char {
    type Output = &'i str;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) {
        unsafe {
            let (a, b) = self.compare(input.as_bytes());
            (
                core::str::from_utf8_unchecked(a),
                b.map(|b| core::str::from_utf8_unchecked(b)),
            )
        }
    }
}

impl<'i> Compare<&'i str> for AnyOf<&str> {
    type Output = CharRef<&'i str>;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) {
        if self.0.is_ascii() {
            let (input, output) = AnyOf(self.0.as_bytes()).compare(input);
            return (input, output.map(CharRef))
        }

        match input.chars().next() {
            Some(output) if self.0.contains(output) => {
                let (a, b) = unsafe { split_at_unchecked(input, output.len_utf8()) };
                (a, b.map(CharRef))
            }
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i str> for NoneOf<&str> {
    type Output = CharRef<&'i str>;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) {
        if self.0.is_ascii() {
            let (input, output) = NoneOf(self.0.as_bytes()).compare(input);
            return (input, output.map(CharRef))
        }

        match input.chars().next() {
            Some(output) if !self.0.contains(output) => {
                let (a, b) = unsafe { split_at_unchecked(input, output.len_utf8()) };
                (a, b.map(CharRef))
            }
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i str> for AnyOf<&[char]> {
    type Output = CharRef<&'i str>;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) {
        match input.chars().next() {
            Some(output) if self.0.contains(&output) => {
                let (a, b) = unsafe { split_at_unchecked(input, output.len_utf8()) };
                (a, b.map(CharRef))
            }
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i str> for NoneOf<&[char]> {
    type Output = CharRef<&'i str>;

    #[inline]
    fn compare(&self, input: &'i str) -> (&'i str, Option<Self::Output>) {
        match input.chars().next() {
            Some(output) if !self.0.contains(&output) => {
                let (a, b) = unsafe { split_at_unchecked(input, output.len_utf8()) };
                (a, b.map(CharRef))
            }
            _ => (input, None),
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i mut [T]> for [T] {
    type Output = &'i mut [T];

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], Option<Self::Output>) {
        if input.get(..self.len()) == Some(self) {
            let (output, input) = input.split_at_mut(self.len());
            (input, Some(output))
        } else {
            (input, None)
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i mut [T]> for &[T] {
    type Output = &'i mut [T];

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], Option<Self::Output>) { (**self).compare(input) }
}

unsafe fn unlink<'a, 'b, T: ?Sized>(x: &'a mut T) -> &'b mut T { &mut *(x as *mut T) }

impl<'i, T: PartialEq> Compare<&'i mut [T]> for T {
    type Output = &'i mut T;

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], Option<Self::Output>) {
        match unsafe { unlink(input) } {
            [output, input @ ..] if output == self => (input, Some(output)),
            _ => (input, None),
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i mut [T]> for AnyOf<&[T]> {
    type Output = &'i mut T;

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], Option<Self::Output>) {
        match unsafe { unlink(input) } {
            [first, input @ ..] if self.0.contains(first) => (input, Some(first)),
            _ => (input, None),
        }
    }
}

impl<'i, T: PartialEq> Compare<&'i mut [T]> for NoneOf<&[T]> {
    type Output = &'i mut T;

    fn compare(&self, input: &'i mut [T]) -> (&'i mut [T], Option<Self::Output>) {
        match unsafe { unlink(input) } {
            [first, input @ ..] if !self.0.contains(first) => (input, Some(first)),
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i mut str> for str {
    type Output = &'i mut str;

    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) {
        if input.get(..self.len()) == Some(self) {
            let (output, input) = input.split_at_mut(self.len());
            (input, Some(output))
        } else {
            (input, None)
        }
    }
}

impl<'i> Compare<&'i mut [u8]> for str {
    type Output = &'i mut str;

    fn compare(&self, input: &'i mut [u8]) -> (&'i mut [u8], Option<Self::Output>) {
        let (input, output) = self.as_bytes().compare(input);

        (input, output.map(|v| unsafe { std::str::from_utf8_unchecked_mut(v) }))
    }
}

impl<'i> Compare<&'i mut str> for &str {
    type Output = &'i mut str;

    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) { (**self).compare(input) }
}

impl<'i> Compare<&'i mut [u8]> for &str {
    type Output = &'i mut str;

    fn compare(&self, input: &'i mut [u8]) -> (&'i mut [u8], Option<Self::Output>) { (**self).compare(input) }
}

impl<'i> Compare<&'i mut str> for u8 {
    type Output = &'i mut str;

    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) {
        if input.get(..1).map(str::as_bytes) == Some(&[*self]) {
            unsafe { split_at_unchecked_mut(input, 1) }
        } else {
            (input, None)
        }
    }
}

impl<'i> Compare<&'i mut str> for AnyOf<&[u8]> {
    type Output = &'i mut str;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) {
        if !input.is_char_boundary(1) {
            return (input, None)
        }
        match input.get(..1).map(str::as_bytes) {
            Some(&[b]) if self.0.contains(&b) => unsafe { split_at_unchecked_mut(input, 1) },
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i mut str> for NoneOf<&[u8]> {
    type Output = &'i mut str;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) {
        if !input.is_char_boundary(1) {
            return (input, None)
        }
        match input.get(..1).map(str::as_bytes) {
            Some(&[b]) if self.0.contains(&b) => unsafe { split_at_unchecked_mut(input, 1) },
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i mut [u8]> for char {
    type Output = &'i mut [u8];

    fn compare(&self, input: &'i mut [u8]) -> (&'i mut [u8], Option<Self::Output>) {
        let mut buf = [0; 4];
        let buf = self.encode_utf8(&mut buf).as_bytes();
        let (input, output) = buf.compare(input);
        (input, output)
    }
}

impl<'i> Compare<&'i mut str> for char {
    type Output = &'i mut str;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) {
        unsafe {
            let (a, b) = self.compare(input.as_bytes_mut());
            (
                core::str::from_utf8_unchecked_mut(a),
                b.map(|b| core::str::from_utf8_unchecked_mut(b)),
            )
        }
    }
}

impl<'i> Compare<&'i mut str> for AnyOf<&str> {
    type Output = CharRef<&'i mut str>;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) {
        if self.0.is_ascii() {
            let (input, output) = AnyOf(self.0.as_bytes()).compare(input);
            return (input, output.map(CharRef))
        }

        match input.chars().next() {
            Some(output) if self.0.contains(output) => {
                let (a, b) = unsafe { split_at_unchecked_mut(input, output.len_utf8()) };
                (a, b.map(CharRef))
            }
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i mut str> for NoneOf<&str> {
    type Output = CharRef<&'i mut str>;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) {
        if self.0.is_ascii() {
            let (input, output) = NoneOf(self.0.as_bytes()).compare(input);
            return (input, output.map(CharRef))
        }

        match input.chars().next() {
            Some(output) if !self.0.contains(output) => {
                let (a, b) = unsafe { split_at_unchecked_mut(input, output.len_utf8()) };
                (a, b.map(CharRef))
            }
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i mut str> for AnyOf<&[char]> {
    type Output = CharRef<&'i mut str>;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) {
        match input.chars().next() {
            Some(output) if self.0.contains(&output) => {
                let (a, b) = unsafe { split_at_unchecked_mut(input, output.len_utf8()) };
                (a, b.map(CharRef))
            }
            _ => (input, None),
        }
    }
}

impl<'i> Compare<&'i mut str> for NoneOf<&[char]> {
    type Output = CharRef<&'i mut str>;

    #[inline]
    fn compare(&self, input: &'i mut str) -> (&'i mut str, Option<Self::Output>) {
        match input.chars().next() {
            Some(output) if !self.0.contains(&output) => {
                let (a, b) = unsafe { split_at_unchecked_mut(input, output.len_utf8()) };
                (a, b.map(CharRef))
            }
            _ => (input, None),
        }
    }
}
