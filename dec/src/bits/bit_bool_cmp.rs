use super::*;

use crate::traits::{Compare, CompareResult};

impl<'a> Compare<Bits<&'a [u8]>> for bool {
    type Output = bool;

    fn compare(&self, mut input: Bits<&'a [u8]>) -> (Bits<&'a [u8]>, CompareResult<Self::Output>) {
        match input.bytes.get(0) {
            None => (),
            Some(&byte) => {
                let bit = byte & (1 << input.bit_index) != 0;

                if bit == *self {
                    input.bit_index += 1;
                    return (input, CompareResult::Ok(bit));
                }
            }
        }

        (input, CompareResult::Error)
    }
}

impl<'a> Compare<Bits<&'a mut [u8]>> for bool {
    type Output = bool;

    fn compare(
        &self,
        mut input: Bits<&'a mut [u8]>,
    ) -> (Bits<&'a mut [u8]>, CompareResult<Self::Output>) {
        match input.bytes.get(0) {
            None => (),
            Some(&byte) => {
                let bit = byte & (1 << input.bit_index) != 0;

                if bit == *self {
                    input.bit_index += 1;
                    return (input, CompareResult::Ok(bit));
                }
            }
        }

        (input, CompareResult::Error)
    }
}

fn cmp_bits_small<'a, 'b>(
    tag: &'b [bool],
    mut input: Bits<&'a [u8]>,
) -> (Bits<&'a [u8]>, CompareResult<&'b [bool]>) {
    let old_input = input;
    let mut s_bits = tag.iter();

    let byte = match input.bytes.get(0) {
        None => return (old_input, CompareResult::Error),
        Some(&byte) => byte,
    };

    for (i, &s_bit) in (input.bit_index..8).zip(s_bits.by_ref()) {
        let bit = byte & (1 << (7 - i)) != 0;
        input.bit_index += 1;

        if bit != s_bit {
            return (old_input, CompareResult::Error);
        }
    }

    if input.bit_index == 8 {
        input.bit_index = 0;
        input.bytes = &input.bytes[1..];
    }

    if !s_bits.as_slice().is_empty() {
        let byte = match input.bytes.get(1) {
            None => return (old_input, CompareResult::Incomplete),
            Some(&byte) => byte,
        };

        for (i, &s_bit) in s_bits.enumerate() {
            let bit = byte & (1 << (7 - i)) != 0;
            input.bit_index += 1;

            if bit != s_bit {
                return (old_input, CompareResult::Error);
            }
        }
    }

    (input, CompareResult::Ok(tag))
}

fn cmp_bits_large<'a, 'b>(
    tag: &'b [bool],
    mut input: Bits<&'a [u8]>,
) -> (Bits<&'a [u8]>, CompareResult<&'b [bool]>) {
    let old_input = input.clone();
    let mut s_bits = tag.iter();

    if input.bit_index != 0 {
        let byte = match input.bytes.get(0) {
            None => return (old_input, CompareResult::Error),
            Some(&byte) => byte,
        };

        for (i, &s_bit) in (input.bit_index..8).zip(s_bits.by_ref()) {
            let bit = byte & (1 << (7 - i)) != 0;
            input.bit_index += 1;

            if bit != s_bit {
                return (old_input, CompareResult::Error);
            }
        }

        input.bit_index = 0;

        input.bytes = &input.bytes[1..];
    }

    let mut s_bits = s_bits.as_slice().chunks_exact(8);
    let last_chunk = s_bits.remainder();
    let mut bytes = input.bytes.iter();

    for (i, &byte) in s_bits.by_ref().zip(bytes.by_ref()) {
        let mut val = 0;

        for (i, &b) in i.iter().enumerate() {
            val |= u8::from(b) << (7 - i);
        }

        if val != byte {
            return (old_input, CompareResult::Error);
        }
    }

    input.bytes = bytes.as_slice();

    for _ in s_bits {
        return (old_input, CompareResult::Incomplete);
    }

    if !last_chunk.is_empty() {
        let byte = match bytes.next() {
            None => return (old_input, CompareResult::Incomplete),
            Some(&byte) => byte,
        };

        for (i, &s_bit) in last_chunk.iter().enumerate() {
            let bit = byte & (1 << (7 - i)) != 0;
            input.bit_index += 1;

            if bit != s_bit {
                return (old_input, CompareResult::Error);
            }
        }
    }

    (input, CompareResult::Ok(tag))
}

impl<'a, 'i> Compare<Bits<&'a [u8]>> for &'i [bool] {
    type Output = &'i [bool];

    #[inline]
    fn compare(&self, input: Bits<&'a [u8]>) -> (Bits<&'a [u8]>, CompareResult<Self::Output>) {
        if self.len() == 0 {
            return (input, CompareResult::Ok(*self));
        } else if self.len() < 8 {
            cmp_bits_small(*self, input)
        } else {
            cmp_bits_large(*self, input)
        }
    }
}

impl<'a, 'i> Compare<Bits<&'a mut [u8]>> for &'i [bool] {
    type Output = &'i [bool];

    #[inline]
    fn compare(
        &self,
        input: Bits<&'a mut [u8]>,
    ) -> (Bits<&'a mut [u8]>, CompareResult<Self::Output>) {
        let (input, output) = fix(input, *self, |s, i| (&s).compare(i));
        (input, output)
    }
}

impl<'a> Compare<Bits<&'a [u8]>> for [bool; 1] {
    type Output = [bool; 1];

    fn compare(&self, input: Bits<&'a [u8]>) -> (Bits<&'a [u8]>, CompareResult<Self::Output>) {
        let (input, output) = self[0].compare(input);
        (input, output.map(|x| [x]))
    }
}

impl<'a> Compare<Bits<&'a mut [u8]>> for [bool; 1] {
    type Output = [bool; 1];

    fn compare(
        &self,
        input: Bits<&'a mut [u8]>,
    ) -> (Bits<&'a mut [u8]>, CompareResult<Self::Output>) {
        let (input, output) = self[0].compare(input);
        (input, output.map(|x| [x]))
    }
}

macro_rules! small_bool_array {
    ($($lit:literal)*) => {$(
        impl<'a> Compare<Bits<&'a [u8]>> for [bool; $lit] {
            type Output = [bool; $lit];

            fn compare(
                &self,
                input: Bits<&'a [u8]>,
            ) -> (Bits<&'a [u8]>, CompareResult<Self::Output>) {
                let (input, output) = cmp_bits_small(self, input);
                (input, output.map(|_| *self))
            }
        }

        impl<'a> Compare<Bits<&'a mut [u8]>> for [bool; $lit] {
            type Output = [bool; $lit];

            fn compare(
                &self,
                input: Bits<&'a mut [u8]>,
            ) -> (Bits<&'a mut [u8]>, CompareResult<Self::Output>) {
                let (input, output) = fix(input, &self[..], cmp_bits_small);
                (input, output.map(|_| *self))
            }
        }
    )*};
}

small_bool_array!(2 3 4 5 6 7 8);

macro_rules! large_bool_array {
    ($($lit:literal)*) => {$(
        impl<'a> Compare<Bits<&'a [u8]>> for [bool; $lit] {
            type Output = [bool; $lit];

            fn compare(
                &self,
                input: Bits<&'a [u8]>,
            ) -> (Bits<&'a [u8]>, CompareResult<Self::Output>) {
                let (input, output) = cmp_bits_large(self, input);
                (input, output.map(|_| *self))
            }
        }

        impl<'a> Compare<Bits<&'a mut [u8]>> for [bool; $lit] {
            type Output = [bool; $lit];

            fn compare(
                &self,
                input: Bits<&'a mut [u8]>,
            ) -> (Bits<&'a mut [u8]>, CompareResult<Self::Output>) {
                let (input, output) = fix(input, &self[..], cmp_bits_large);
                (input, output.map(|_| *self))
            }
        }
    )*};
}

large_bool_array!(9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32);

#[cfg(test)]
mod test {
    use super::*;
    use crate::{error::*, prelude::*, tag::Tag};

    #[test]
    fn bool_array_small() {
        let input_array = &[0b01011010][..];

        assert_eq!(
            ParseOnce::<_, ()>::parse_once(
                Tag([false, true]),
                Bits {
                    bytes: input_array,
                    bit_index: 0
                }
            ),
            Ok((
                Bits {
                    bytes: input_array,
                    bit_index: 2
                },
                [false, true]
            ))
        );

        assert_eq!(
            ParseOnce::<_, ()>::parse_once(
                Tag([true, false]),
                Bits {
                    bytes: input_array,
                    bit_index: 2
                }
            ),
            Err(Error::Error(()))
        );

        assert_eq!(
            ParseOnce::<_, ()>::parse_once(
                Tag([false, true]),
                Bits {
                    bytes: input_array,
                    bit_index: 7
                }
            ),
            Err(Error::Error(()))
        );

        assert_eq!(
            ParseOnce::<_, ()>::parse_once(
                Tag([false, true, true]),
                Bits {
                    bytes: input_array,
                    bit_index: 6
                }
            ),
            Err(Error::Error(()))
        );

        assert_eq!(
            ParseOnce::<_, ()>::parse_once(
                Tag([true, false]),
                Bits {
                    bytes: input_array,
                    bit_index: 4
                }
            ),
            Ok((
                Bits {
                    bytes: input_array,
                    bit_index: 6
                },
                [true, false]
            ))
        );

        assert_eq!(
            ParseOnce::<_, ()>::parse_once(
                Tag([true, false]),
                Bits {
                    bytes: input_array,
                    bit_index: 6
                }
            ),
            Ok((
                Bits {
                    bytes: &[][..],
                    bit_index: 0
                },
                [true, false]
            ))
        );
    }

    #[test]
    fn bool_array_large() {
        let input_array = &[0b01011010, 0b00001111, 0b01011010][..];

        // parse 2 bytes of data
        let tag = [
            true, false, true, false, false, false, false, false, true, true, true, true, false,
            true, false, true,
        ];

        assert_eq!(
            ParseOnce::<_, ()>::parse_once(
                Tag(tag),
                Bits {
                    bytes: input_array,
                    bit_index: 4
                }
            ),
            Ok((
                Bits {
                    bytes: &[0b01011010][..],
                    bit_index: 4
                },
                tag
            ))
        );
    }

    #[test]
    fn bool_array_large_mut() {
        let input_array = &mut [0b01011010, 0b00001111, 0b01011010][..];

        // parse 2 bytes of data

        let tag = [
            true, false, true, false, false, false, false, false, true, true, true, true, false,
            true, false, true,
        ];

        assert_eq!(
            ParseOnce::<_, ()>::parse_once(
                Tag(tag),
                Bits {
                    bytes: input_array,
                    bit_index: 4
                }
            ),
            Ok((
                Bits {
                    bytes: &mut [0b01011010][..],
                    bit_index: 4
                },
                tag
            ))
        );
    }
}
