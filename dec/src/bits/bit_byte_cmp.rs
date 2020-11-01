use super::*;

use crate::traits::{Compare, CompareResult};

impl<'a, 'i> Compare<Bits<&'a [u8]>> for &'i [u8] {
    type Output = &'i [u8];

    fn compare(&self, mut input: Bits<&'a [u8]>) -> (Bits<&'a [u8]>, CompareResult<Self::Output>) {
        if input.bit_index == 0 {
            let (bytes, output) = self.compare(input.bytes);
            input.bytes = bytes;
            (input, output.map(|_| *self))
        } else {
            let old_input = input;
            let mut s_bytes = self.iter();
            let bit_index = input.bit_index;

            for (ib, &sb) in input.bytes.windows(2).zip(s_bytes.by_ref()) {
                let (ib0, ib1) = match ib {
                    &[ib0, ib1] => (ib0, ib1),
                    _ => unreachable!(),
                };

                let ib = (ib0 << bit_index) | (ib1 >> (8 - bit_index));

                if ib != sb {
                    return (old_input, CompareResult::Error);
                }
            }

            for _ in s_bytes {
                return (old_input, CompareResult::Incomplete);
            }

            input.bytes = &input.bytes[self.len()..];

            (input, CompareResult::Ok(*self))
        }
    }
}

impl<'a, 'i> Compare<Bits<&'a mut [u8]>> for &'i [u8] {
    type Output = &'i [u8];

    fn compare(
        &self,
        input: Bits<&'a mut [u8]>,
    ) -> (Bits<&'a mut [u8]>, CompareResult<Self::Output>) {
        fix(input, *self, |s, i| (&s).compare(i))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::{prelude::*, tag::Tag};

    #[test]
    fn cmp_bytes() {
        let input_array = &[0b01011010, 0b00001111, 0b01011010][..];

        assert_eq!(
            ParseOnce::<_, ()>::parse_once(
                Tag(&[0b1010_0000, 0b1111_0101][..]),
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
                &[0b1010_0000, 0b1111_0101][..]
            ))
        );
    }
}
