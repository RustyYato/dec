use crate::traits::InputSplit;

mod bit_bool_cmp;
mod bit_byte_cmp;

#[must_use = "bits is just a data structure and does nothing on it's own"]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bits<B> {
    bytes: B,
    bit_index: u8,
}

impl<'a> From<&'a [u8]> for Bits<&'a [u8]> {
    fn from(bytes: &'a [u8]) -> Self { Self { bytes, bit_index: 0 } }
}

impl<'a> From<&'a mut [u8]> for Bits<&'a mut [u8]> {
    fn from(bytes: &'a mut [u8]) -> Self { Self { bytes, bit_index: 0 } }
}

impl<'a> Bits<&'a [u8]> {
    pub fn from_ref(bytes: &'a [u8], bit_index: u8) -> Self {
        assert!(!bytes.is_empty() || bit_index == 0);
        assert!(bit_index < 8);

        Self { bytes, bit_index }
    }
}

impl<'a> Bits<&'a mut [u8]> {
    pub fn from_mut(bytes: &'a mut [u8], bit_index: u8) -> Self {
        assert!(!bytes.is_empty() || bit_index == 0);
        assert!(bit_index < 8);

        Self { bytes, bit_index }
    }
}

impl<B> Bits<B> {
    pub fn bytes(&self) -> &B { &self.bytes }

    pub fn bit_index(&self) -> u8 { self.bit_index }

    pub fn into_bytes_index(self) -> (B, u8) { (self.bytes, self.bit_index) }
}

impl<B: InputSplit> InputSplit for Bits<B> {
    fn len(&self) -> usize {
        self.bytes.len().checked_mul(8).expect("bit length too large") - usize::from(self.bit_index)
    }

    fn cut(mut self, at: usize) -> Self {
        let at = at
            .checked_add(usize::from(self.bit_index))
            .expect("bit index too large");
        let len = self.bytes.len();
        self.bytes = self.bytes.cut((at / 8 + 1).min(len));
        self
    }

    fn advance(mut self, at: usize) -> std::result::Result<Self, Self> {
        let at = at
            .checked_add(usize::from(self.bit_index))
            .expect("bit index too large");
        let (at_bit, at_byte) = (at % 8, (at + 7) / 8);

        let len = self.bytes.len();
        match self.bytes.advance((at_byte).min(len)) {
            Ok(bytes) => {
                self.bytes = bytes;
                self.bit_index = at_bit as u8;
                Ok(self)
            }
            Err(bytes) => {
                self.bytes = bytes;
                Err(self)
            }
        }
    }
}

fn fix<T, U, F>(mut input: Bits<&mut [u8]>, value: T, f: F) -> (Bits<&mut [u8]>, Option<U>)
where
    F: FnOnce(T, Bits<&[u8]>) -> (Bits<&[u8]>, Option<U>),
{
    let (i, output) = f(value, Bits {
        bytes: &*input.bytes,
        bit_index: input.bit_index,
    });

    input.bit_index = i.bit_index;
    let len = input.bytes.len() - i.bytes.len();
    input.bytes = &mut input.bytes[len..];

    (input, output)
}
