use std_core::ops::{Bound, RangeBounds};

#[derive(Debug, PartialEq, Eq)]
pub struct Ranged {
    prefix: usize,
    tail: Option<usize>,
}

pub struct Prefix(usize);
pub struct Tail(Option<usize>);

fn validate_range(start: Bound<usize>, end: Bound<usize>) -> bool {
    match (start, end) {
        (Bound::Included(x), Bound::Included(y)) => x <= y,

        | (Bound::Included(x), Bound::Excluded(y))
        | (Bound::Excluded(x), Bound::Excluded(y))
        | (Bound::Excluded(x), Bound::Included(y)) => x < y,

        (Bound::Unbounded, _) | (_, Bound::Unbounded) => true,
    }
}

fn copied(x: Bound<&usize>) -> Bound<usize> {
    match x {
        Bound::Included(&x) => Bound::Included(x),
        Bound::Excluded(&x) => Bound::Excluded(x),
        Bound::Unbounded => Bound::Unbounded,
    }
}

impl Ranged {
    pub fn new<R: RangeBounds<usize>>(range: R) -> Option<Self> {
        let start = copied(range.start_bound());
        let end = copied(range.end_bound());

        assert!(validate_range(start, end), "malformed range");

        // fast track no-ops
        match end {
            Bound::Excluded(1) | Bound::Excluded(0) | Bound::Included(0) => return None,
            _ => (),
        }

        let prefix = match start {
            // validate_range will ensure that x != usize::MAX
            // if start is excluded
            Bound::Excluded(x) => x.wrapping_add(1),
            Bound::Included(x) => x,
            Bound::Unbounded => 0,
        };

        let tail = match end {
            // validate_range will ensure that x > prefix
            // if end is excluded
            Bound::Excluded(x) => Some(x.wrapping_sub(1).wrapping_sub(prefix)),
            // validate_range will ensure that x >= prefix
            // if end is included
            Bound::Included(x) => Some(x.wrapping_sub(prefix)),
            Bound::Unbounded => None,
        };

        Some(Self { prefix, tail })
    }

    pub fn split(self) -> (Prefix, Tail) { (Prefix(self.prefix), Tail(self.tail)) }
}

impl Iterator for Prefix {
    type Item = ();

    fn next(&mut self) -> Option<()> {
        self.0 = self.0.checked_sub(1)?;
        Some(())
    }
}

impl Iterator for Tail {
    type Item = ();

    fn next(&mut self) -> Option<()> {
        if let Some(tail) = self.0.as_mut() {
            *tail = tail.checked_sub(1)?;
        }
        Some(())
    }
}

#[test]
fn range() {
    assert_eq!(Ranged::new(..), Some(Ranged { prefix: 0, tail: None }));
    assert_eq!(Ranged::new(1..), Some(Ranged { prefix: 1, tail: None }));
    assert_eq!(
        Ranged::new(..3),
        Some(Ranged {
            prefix: 0,
            tail: Some(2)
        })
    );
    assert_eq!(
        Ranged::new(..=3),
        Some(Ranged {
            prefix: 0,
            tail: Some(3)
        })
    );
    assert_eq!(
        Ranged::new(1..3),
        Some(Ranged {
            prefix: 1,
            tail: Some(1)
        })
    );
    assert_eq!(
        Ranged::new(1..=3),
        Some(Ranged {
            prefix: 1,
            tail: Some(2)
        })
    );
}
