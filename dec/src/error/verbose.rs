use super::{ErrorKind, ParseError};
use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub struct VerboseError<I> {
    pub errors: VecDeque<VerboseErrorItem<I>>,
}

#[derive(Debug, PartialEq)]
pub enum VerboseErrorItem<I> {
    Alt {
        parent: Option<usize>,
    },
    Item {
        parent: Option<usize>,
        input: I,
        kind: VerboseErrorKind,
    },
}

#[derive(Debug, PartialEq)]
/// error context for `VerboseError`
pub enum VerboseErrorKind {
    /// static string added by the `context` function
    Context(&'static str),
    /// indicates which character was expected by the `char` function
    Char(char),
    /// error kind given by various nom parsers
    Kind(ErrorKind),
}

impl<I> VerboseError<I> {
    pub fn map_input<J, F: FnMut(I) -> J>(self, mut f: F) -> VerboseError<J> {
        VerboseError {
            errors: self
                .errors
                .into_iter()
                .map(|x| x.map_input(&mut f))
                .collect(),
        }
    }
}

impl<I> VerboseErrorItem<I> {
    pub fn parent(&self) -> Option<usize> {
        match *self {
            VerboseErrorItem::Alt { parent } | VerboseErrorItem::Item { parent, .. } => parent,
        }
    }

    pub fn map_input<J, F: FnOnce(I) -> J>(self, f: F) -> VerboseErrorItem<J> {
        match self {
            Self::Alt { parent } => VerboseErrorItem::Alt { parent },
            Self::Item {
                parent,
                input,
                kind,
            } => VerboseErrorItem::Item {
                parent,
                kind,
                input: f(input),
            },
        }
    }
}

impl<I: std::fmt::Debug> ParseError<I> for VerboseError<I> {
    fn from_input_kind(input: I, kind: ErrorKind) -> Self {
        let mut errors = VecDeque::with_capacity(1);
        errors.push_back(VerboseErrorItem::Item {
            parent: None,
            input,
            kind: VerboseErrorKind::Kind(kind),
        });
        VerboseError { errors }
    }

    // fn from_char(input: I, c: char) -> Self {
    //     let mut errors = VecDeque::with_capacity(1);
    //     errors.push_back(VerboseErrorItem::Item {
    //         parent: None,
    //         input,
    //         kind: VerboseErrorKind::Char(c),
    //     });
    //     VerboseError { errors }
    // }

    fn append(mut self, input: I, kind: ErrorKind) -> Self {
        let len = self.errors.len() - 1;
        self.errors.push_back(VerboseErrorItem::Item {
            parent: Some(len),
            input,
            kind: VerboseErrorKind::Kind(kind),
        });
        self
    }

    fn add_context(mut self: Self, input: I, ctx: &'static str) -> Self {
        self.errors.iter_mut().for_each(|err| match err {
            VerboseErrorItem::Alt { parent } | VerboseErrorItem::Item { parent, .. } => {
                match parent {
                    Some(parent) => *parent += 1,
                    None => *parent = Some(0),
                }
            }
        });

        self.errors.push_front(VerboseErrorItem::Item {
            input,
            parent: None,
            kind: VerboseErrorKind::Context(ctx),
        });

        self
    }

    fn or(mut self, mut other: Self) -> Self {
        let update_parent = |parent_offset| {
            move |err: &mut VerboseErrorItem<I>| match err {
                VerboseErrorItem::Item { parent, .. } | VerboseErrorItem::Alt { parent, .. } => {
                    if let Some(parent) = parent {
                        *parent += parent_offset;
                    } else {
                        *parent = Some(0)
                    }
                }
            }
        };

        if let Some(VerboseErrorItem::Alt { parent: None }) = self.errors.front() {
        } else {
            self.errors.iter_mut().for_each(update_parent(1));

            self.errors
                .push_front(VerboseErrorItem::Alt { parent: None });
        }

        let parent_offset = self.errors.len();

        other
            .errors
            .iter_mut()
            .for_each(update_parent(parent_offset));

        self.errors.extend(other.errors);

        self
    }
}
