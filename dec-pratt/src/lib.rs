#![allow(clippy::type_complexity)]

use dec_core::{
    error::{CaptureInput, Error::Error, ErrorKind, PResult, ParseError, PrattErrorKind},
    ParseMut, ParseOnce,
};

use core::convert::Infallible;

#[cfg(test)]
mod test;

#[allow(clippy::len_without_is_empty)]
pub trait Stack {
    type Item;

    fn len(&self) -> usize;
    fn push(&mut self, value: Self::Item);
    fn pop(&mut self) -> Self::Item;
}

impl<S: ?Sized + Stack> Stack for &mut S {
    type Item = S::Item;

    fn len(&self) -> usize { S::len(self) }
    fn push(&mut self, value: Self::Item) { S::push(self, value) }
    fn pop(&mut self) -> Self::Item { S::pop(self) }
}

impl<S: ?Sized + Stack> Stack for Box<S> {
    type Item = S::Item;

    fn len(&self) -> usize { S::len(self) }
    fn push(&mut self, value: Self::Item) { S::push(self, value) }
    fn pop(&mut self) -> Self::Item { S::pop(self) }
}

impl<T> Stack for Vec<T> {
    type Item = T;

    fn len(&self) -> usize { self.len() }
    fn push(&mut self, value: Self::Item) { self.push(value); }
    fn pop(&mut self) -> Self::Item { self.pop().unwrap() }
}

#[cfg(feature = "smallvec")]
impl<A: smallvec::Array> Stack for smallvec::SmallVec<A> {
    type Item = A::Item;

    fn len(&self) -> usize { self.len() }
    fn push(&mut self, value: Self::Item) { self.push(value); }
    fn pop(&mut self) -> Self::Item { self.pop().unwrap() }
}

#[cfg(feature = "arrayvec")]
impl<A: arrayvec::Array> Stack for arrayvec::ArrayVec<A> {
    type Item = A::Item;

    fn len(&self) -> usize { self.len() }
    fn push(&mut self, value: Self::Item) { self.push(value); }
    fn pop(&mut self) -> Self::Item { self.pop().unwrap() }
}

#[cfg(feature = "generic-vec")]
impl<T, S: generic_vec::raw::Storage<T>> Stack for generic_vec::GenericVec<T, S> {
    type Item = T;

    fn len(&self) -> usize { self.len() }
    fn push(&mut self, value: Self::Item) { self.push(value); }
    fn pop(&mut self) -> Self::Item { self.pop() }
}

pub struct RecursePratt<P>(pub P);

pub struct StackPratt<P, S> {
    pub pratt: P,
    pub stack: S,
}

#[derive(Debug)]
pub enum Args<N, H, V> {
    Normal(N),
    Recurse(H, V),
}

pub enum Hook<V, H, B> {
    Complete(V),
    Recurse(B, H),
}

#[derive(Debug, Clone, Copy)]
pub struct Operator<Left, Op, Right> {
    pub left: Left,
    pub op: Op,
    pub right: Right,
}

use private::StackItem as RawSI;
mod private {
    pub struct StackItem<Bp, V, PO, IO, HBV, HFI, HMPr, HMI, HMPo> {
        pub(crate) min_bp: Bp,
        pub(crate) recurse: super::Recurse<V, PO, IO, HBV, HFI, HMPr, HMI, HMPo>,
    }
}

pub enum StackItem<Psi, E = ()> {
    Raw(Psi),
    Ext(E),
}

#[allow(type_alias_bounds)]
pub type PrattStackItem<I, E: ParseError<I>, F, P: Pratt<I, E, F>> = private::StackItem<
    <P as Pratt<I, E, F>>::BindingPower,
    <P as Pratt<I, E, F>>::Value,
    <P as Pratt<I, E, F>>::PrefixOp,
    <P as Pratt<I, E, F>>::InfixOp,
    <P as Pratt<I, E, F>>::HookBuildValue,
    <P as Pratt<I, E, F>>::HookFinishInfix,
    <P as Pratt<I, E, F>>::HookMergePrefix,
    <P as Pratt<I, E, F>>::HookMergeInfix,
    <P as Pratt<I, E, F>>::HookMergePostfix,
>;

enum Recurse<V, PO, IO, HBV, HFI, HMPr, HMI, HMPo> {
    PrefixOp(PO),
    InfixOp(IO, V),
    Value(HBV),
    HookFinishInfix(HFI, V),
    HookMergePrefix(HMPr),
    HookMergeInfix(HMI),
    HookMergePostfix(HMPo),
}

pub type PrefixOp<P, I, E, F = core::convert::Infallible> =
    Operator<(), <P as Pratt<I, E, F>>::PrefixOp, <P as Pratt<I, E, F>>::Value>;
pub type InfixOp<P, I, E, F = core::convert::Infallible> =
    Operator<<P as Pratt<I, E, F>>::Value, <P as Pratt<I, E, F>>::InfixOp, <P as Pratt<I, E, F>>::Value>;
pub type PostfixOp<P, I, E, F = core::convert::Infallible> =
    Operator<<P as Pratt<I, E, F>>::Value, <P as Pratt<I, E, F>>::PostfixOp, ()>;

impl<N, V> Args<N, Infallible, V> {
    pub fn always_normal(self) -> N {
        match self {
            Args::Normal(normal) => normal,
            Args::Recurse(inf, _) => match inf {},
        }
    }
}

impl<Op, Right> Operator<(), Op, Right> {
    pub fn prefix_op(op: Op, right: Right) -> Self { Self { left: (), op, right } }
}

impl<Left, Op, Right> Operator<Left, Op, Right> {
    pub fn infix_op(left: Left, op: Op, right: Right) -> Self { Self { left, op, right } }
}

impl<Left, Op> Operator<Left, Op, ()> {
    pub fn postfix_op(left: Left, op: Op) -> Self { Self { left, op, right: () } }
}

#[allow(unused)]
pub trait Pratt<I, E: ParseError<I>, F = core::convert::Infallible> {
    type BindingPower: Default + Copy + Ord;
    type Value;

    type PrefixOp;
    type InfixOp;
    type PostfixOp;
    type FastInfixOp;

    type HookBuildValue;
    type HookFinishInfix;
    type HookMergePrefix;
    type HookMergeInfix;
    type HookMergePostfix;

    fn value(
        &mut self,
        args: Args<(), Self::HookBuildValue, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookBuildValue, Self::BindingPower>, E, F>;

    fn prefix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<(), Self::PrefixOp, Self::BindingPower>, CaptureInput<I>, F> {
        Err(Error(CaptureInput(input)))
    }

    fn infix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<Self::BindingPower, Self::FastInfixOp, Self::BindingPower>, CaptureInput<I>, F> {
        Err(Error(CaptureInput(input)))
    }

    fn postfix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<Self::BindingPower, Self::PostfixOp, ()>, CaptureInput<I>, F> {
        Err(Error(CaptureInput(input)))
    }

    fn finish_infix_op(
        &mut self,
        args: Args<Self::FastInfixOp, Self::HookFinishInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::InfixOp, Self::HookFinishInfix, Self::BindingPower>, E, F> {
        Err(Error(E::from_input_kind(
            input,
            ErrorKind::Pratt(PrattErrorKind::FinishInfixOp),
        )))
    }

    fn merge_prefix_op(
        &mut self,
        args: Args<PrefixOp<Self, I, E, F>, Self::HookMergePrefix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePrefix, Self::BindingPower>, E, F> {
        Err(Error(E::from_input_kind(
            input,
            ErrorKind::Pratt(PrattErrorKind::MergePrefixOp),
        )))
    }

    fn merge_infix_op(
        &mut self,
        args: Args<InfixOp<Self, I, E, F>, Self::HookMergeInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergeInfix, Self::BindingPower>, E, F> {
        Err(Error(E::from_input_kind(
            input,
            ErrorKind::Pratt(PrattErrorKind::MergeInfixOp),
        )))
    }

    fn merge_postfix_op(
        &mut self,
        args: Args<PostfixOp<Self, I, E, F>, Self::HookMergePostfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePostfix, Self::BindingPower>, E, F> {
        Err(Error(E::from_input_kind(
            input,
            ErrorKind::Pratt(PrattErrorKind::MergePostfixOp),
        )))
    }
}

impl<I, E: ParseError<I>, F, P: Pratt<I, E, F> + ?Sized> Pratt<I, E, F> for &mut P {
    type BindingPower = P::BindingPower;
    type Value = P::Value;

    type PrefixOp = P::PrefixOp;
    type InfixOp = P::InfixOp;
    type PostfixOp = P::PostfixOp;
    type FastInfixOp = P::FastInfixOp;

    type HookBuildValue = P::HookBuildValue;
    type HookFinishInfix = P::HookFinishInfix;
    type HookMergePrefix = P::HookMergePrefix;
    type HookMergeInfix = P::HookMergeInfix;
    type HookMergePostfix = P::HookMergePostfix;

    fn value(
        &mut self,
        args: Args<(), Self::HookBuildValue, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookBuildValue, Self::BindingPower>, E, F> {
        P::value(self, args, input)
    }

    fn prefix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<(), Self::PrefixOp, Self::BindingPower>, CaptureInput<I>, F> {
        P::prefix_op(self, input)
    }

    fn infix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<Self::BindingPower, Self::FastInfixOp, Self::BindingPower>, CaptureInput<I>, F> {
        P::infix_op(self, input)
    }

    fn postfix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<Self::BindingPower, Self::PostfixOp, ()>, CaptureInput<I>, F> {
        P::postfix_op(self, input)
    }

    fn finish_infix_op(
        &mut self,
        args: Args<Self::FastInfixOp, Self::HookFinishInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::InfixOp, Self::HookFinishInfix, Self::BindingPower>, E, F> {
        P::finish_infix_op(self, args, input)
    }

    fn merge_prefix_op(
        &mut self,
        args: Args<PrefixOp<Self, I, E, F>, Self::HookMergePrefix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePrefix, Self::BindingPower>, E, F> {
        P::merge_prefix_op(self, args, input)
    }

    fn merge_infix_op(
        &mut self,
        args: Args<InfixOp<Self, I, E, F>, Self::HookMergeInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergeInfix, Self::BindingPower>, E, F> {
        P::merge_infix_op(self, args, input)
    }

    fn merge_postfix_op(
        &mut self,
        args: Args<PostfixOp<Self, I, E, F>, Self::HookMergePostfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePostfix, Self::BindingPower>, E, F> {
        P::merge_postfix_op(self, args, input)
    }
}

impl<I, E: ParseError<I>, F, P: Pratt<I, E, F> + ?Sized> Pratt<I, E, F> for Box<P> {
    type BindingPower = P::BindingPower;
    type Value = P::Value;

    type PrefixOp = P::PrefixOp;
    type InfixOp = P::InfixOp;
    type PostfixOp = P::PostfixOp;
    type FastInfixOp = P::FastInfixOp;

    type HookBuildValue = P::HookBuildValue;
    type HookFinishInfix = P::HookFinishInfix;
    type HookMergePrefix = P::HookMergePrefix;
    type HookMergeInfix = P::HookMergeInfix;
    type HookMergePostfix = P::HookMergePostfix;

    fn value(
        &mut self,
        args: Args<(), Self::HookBuildValue, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookBuildValue, Self::BindingPower>, E, F> {
        P::value(self, args, input)
    }

    fn prefix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<(), Self::PrefixOp, Self::BindingPower>, CaptureInput<I>, F> {
        P::prefix_op(self, input)
    }

    fn infix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<Self::BindingPower, Self::FastInfixOp, Self::BindingPower>, CaptureInput<I>, F> {
        P::infix_op(self, input)
    }

    fn postfix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<Self::BindingPower, Self::PostfixOp, ()>, CaptureInput<I>, F> {
        P::postfix_op(self, input)
    }

    fn finish_infix_op(
        &mut self,
        args: Args<Self::FastInfixOp, Self::HookFinishInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::InfixOp, Self::HookFinishInfix, Self::BindingPower>, E, F> {
        P::finish_infix_op(self, args, input)
    }

    fn merge_prefix_op(
        &mut self,
        args: Args<PrefixOp<Self, I, E, F>, Self::HookMergePrefix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePrefix, Self::BindingPower>, E, F> {
        P::merge_prefix_op(self, args, input)
    }

    fn merge_infix_op(
        &mut self,
        args: Args<InfixOp<Self, I, E, F>, Self::HookMergeInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergeInfix, Self::BindingPower>, E, F> {
        P::merge_infix_op(self, args, input)
    }

    fn merge_postfix_op(
        &mut self,
        args: Args<PostfixOp<Self, I, E, F>, Self::HookMergePostfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePostfix, Self::BindingPower>, E, F> {
        P::merge_postfix_op(self, args, input)
    }
}

fn unwrap<I, E, F, T, C, P, A, Func>(
    pratt: &mut P,
    mut result: Hook<T, C, P::BindingPower>,
    mut input: I,
    mut f: Func,
) -> PResult<I, T, E, F>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E, F>,
    Func: FnMut(&mut P, Args<A, C, P::Value>, I) -> PResult<I, Hook<T, C, P::BindingPower>, E, F>,
{
    loop {
        match result {
            Hook::Complete(value) => break Ok((input, value)),
            Hook::Recurse(bp, hook) => {
                let (next_input, value) = recurse_pratt(pratt, input, bp)?;
                let (next_input, value) = f(pratt, Args::Recurse(hook, value), next_input)?;
                input = next_input;
                result = value;
            }
        }
    }
}

fn recurse_pratt<I, E, F, P>(pratt: &mut P, input: I, min_bp: P::BindingPower) -> PResult<I, P::Value, E, F>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E, F>,
{
    let (mut input, mut lhs) = match pratt.prefix_op(input.clone()) {
        Err(_) => {
            let (input, lhs) = pratt.value(Args::Normal(()), input)?;
            unwrap(pratt, lhs, input, P::value)?
        }
        Ok((input, prefix)) => {
            let (input, value) = recurse_pratt(pratt, input, prefix.right)?;
            let (input, lhs) = pratt.merge_prefix_op(Args::Normal(Operator::prefix_op(prefix.op, value)), input)?;
            unwrap(pratt, lhs, input, P::merge_prefix_op)?
        }
    };

    loop {
        match pratt.postfix_op(input.clone()) {
            Err(_) => (),
            Ok((next_input, postfix)) => {
                if postfix.left < min_bp {
                    break
                }
                let (next_input, value) =
                    pratt.merge_postfix_op(Args::Normal(Operator::postfix_op(lhs, postfix.op)), next_input)?;
                let (next_input, value) = unwrap(pratt, value, next_input, P::merge_postfix_op)?;
                input = next_input;
                lhs = value;
                continue
            }
        }

        match pratt.infix_op(input.clone()) {
            Err(_) => (),
            Ok((next_input, infix)) => {
                if infix.left < min_bp {
                    break
                }
                let (next_input, inf_op) = pratt.finish_infix_op(Args::Normal(infix.op), next_input)?;
                let (next_input, inf_op) = unwrap(pratt, inf_op, next_input, P::finish_infix_op)?;
                let (next_input, rhs) = recurse_pratt(pratt, next_input, infix.right)?;
                let (next_input, value) =
                    pratt.merge_infix_op(Args::Normal(Operator::infix_op(lhs, inf_op, rhs)), next_input)?;
                let (next_input, value) = unwrap(pratt, value, next_input, P::merge_infix_op)?;
                input = next_input;
                lhs = value;
                continue
            }
        }

        break
    }

    Ok((input, lhs))
}

impl<I, E, F, P> ParseOnce<I, E, F> for RecursePratt<P>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E, F>,
{
    type Output = P::Value;

    fn parse_once(mut self, input: I) -> PResult<I, Self::Output, E, F> { self.parse_mut(input) }
}

impl<I, E, F, P> ParseMut<I, E, F> for RecursePratt<P>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E, F>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E, F> {
        let Self(pratt) = self;
        recurse_pratt(pratt, input, Default::default())
    }
}

impl<I, E, F, P, S, Ext> ParseOnce<I, E, F> for StackPratt<P, S>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E, F>,
    S: Stack<Item = StackItem<PrattStackItem<I, E, F, P>, Ext>>,
{
    type Output = P::Value;

    fn parse_once(mut self, input: I) -> PResult<I, Self::Output, E, F> { self.parse_mut(input) }
}

impl<I, E, F, P, S, Ext> ParseMut<I, E, F> for StackPratt<P, S>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E, F>,
    S: Stack<Item = StackItem<PrattStackItem<I, E, F, P>, Ext>>,
{
    fn parse_mut(&mut self, mut input: I) -> PResult<I, Self::Output, E, F> {
        let Self { stack, pratt } = self;
        let bottom = stack.len();
        let mut min_bp = P::BindingPower::default();

        'recurse: loop {
            macro_rules! merge {
                ($hook:expr, $lhs:ident <- $value:expr, $f:expr, $g:expr $(,)?) => {{
                    let (next_input, value) = $f(pratt, Args::Recurse($hook, $value), input)?;
                    input = next_input;
                    match value {
                        Hook::Complete(value) => $lhs = value,
                        Hook::Recurse(bp, hook) => {
                            stack.push(StackItem::Raw(RawSI {
                                min_bp,
                                recurse: $g(hook),
                            }));
                            min_bp = bp;
                            continue 'recurse
                        }
                    }
                }};
            }

            let mut lhs = match pratt.prefix_op(input.clone()) {
                Ok((next_input, prefix)) => {
                    stack.push(StackItem::Raw(RawSI {
                        min_bp,
                        recurse: Recurse::PrefixOp(prefix.op),
                    }));
                    input = next_input;
                    min_bp = prefix.right;
                    continue
                }
                Err(_) => {
                    let (next_input, value) = pratt.value(Args::Normal(()), input)?;
                    input = next_input;
                    match value {
                        Hook::Complete(value) => value,
                        Hook::Recurse(bp, hook) => {
                            stack.push(StackItem::Raw(RawSI {
                                min_bp,
                                recurse: Recurse::Value(hook),
                            }));
                            min_bp = bp;
                            continue
                        }
                    }
                }
            };

            loop {
                let value = 'main_loop: loop {
                    match pratt.postfix_op(input.clone()) {
                        Ok((next_input, postfix)) => {
                            if postfix.left < min_bp {
                                break 'main_loop lhs
                            } else {
                                let (next_input, value) = pratt.merge_postfix_op(
                                    Args::Normal(Operator::postfix_op(lhs, postfix.op)),
                                    next_input,
                                )?;
                                input = next_input;
                                match value {
                                    Hook::Complete(value) => {
                                        lhs = value;
                                        continue 'main_loop
                                    }
                                    Hook::Recurse(bp, hook) => {
                                        stack.push(StackItem::Raw(RawSI {
                                            min_bp,
                                            recurse: Recurse::HookMergePostfix(hook),
                                        }));
                                        min_bp = bp;
                                        continue 'recurse
                                    }
                                }
                            }
                        }
                        Err(_) => match pratt.infix_op(input.clone()) {
                            Ok((next_input, infix)) if infix.left >= min_bp => {
                                let (next_input, inf_op) = pratt.finish_infix_op(Args::Normal(infix.op), next_input)?;

                                let old_min_bp = min_bp;
                                let recurse = match inf_op {
                                    Hook::Complete(inf_op) => {
                                        min_bp = infix.right;
                                        Recurse::InfixOp(inf_op, lhs)
                                    }
                                    Hook::Recurse(bp, hook) => {
                                        min_bp = bp;
                                        Recurse::HookFinishInfix(hook, lhs)
                                    }
                                };

                                stack.push(StackItem::Raw(RawSI {
                                    min_bp: old_min_bp,
                                    recurse,
                                }));

                                input = next_input;
                                continue 'recurse
                            }
                            _ => break 'main_loop lhs,
                        },
                    }
                };

                if stack.len() == bottom {
                    return Ok((input, value))
                }

                let RawSI {
                    min_bp: old_min_bp,
                    recurse,
                } = match stack.pop() {
                    StackItem::Raw(raw) => raw,
                    StackItem::Ext(_) => panic!("Encountered an external stack item while processing expressions!"),
                };

                min_bp = old_min_bp;

                match recurse {
                    Recurse::Value(hook) => {
                        merge!(hook, lhs <- value, P::value, Recurse::Value);
                    }
                    Recurse::HookMergePrefix(hook) => {
                        merge!(hook, lhs <- value, P::merge_prefix_op, Recurse::HookMergePrefix)
                    }
                    Recurse::HookMergeInfix(hook) => {
                        merge!(hook, lhs <- value, P::merge_infix_op, Recurse::HookMergeInfix)
                    }
                    Recurse::HookMergePostfix(hook) => {
                        merge!(hook, lhs <- value, P::merge_postfix_op, Recurse::HookMergePostfix)
                    }
                    Recurse::PrefixOp(pre_op) => {
                        let (next_input, value) =
                            pratt.merge_prefix_op(Args::Normal(Operator::prefix_op(pre_op, value)), input)?;
                        input = next_input;
                        match value {
                            Hook::Complete(value) => lhs = value,
                            Hook::Recurse(bp, hook) => {
                                stack.push(StackItem::Raw(RawSI {
                                    min_bp,
                                    recurse: Recurse::HookMergePrefix(hook),
                                }));
                                min_bp = bp;
                                continue 'recurse
                            }
                        };
                    }
                    Recurse::InfixOp(inf_op, left) => {
                        let (next_input, value) =
                            pratt.merge_infix_op(Args::Normal(Operator::infix_op(left, inf_op, value)), input)?;
                        input = next_input;
                        match value {
                            Hook::Complete(value) => lhs = value,
                            Hook::Recurse(bp, hook) => {
                                stack.push(StackItem::Raw(RawSI {
                                    min_bp,
                                    recurse: Recurse::HookMergeInfix(hook),
                                }));
                                min_bp = bp;
                                continue 'recurse
                            }
                        };
                    }
                    Recurse::HookFinishInfix(hook, lhs) => {
                        let (next_input, inf_op) = pratt.finish_infix_op(Args::Recurse(hook, value), input)?;
                        input = next_input;

                        let old_min_bp = min_bp;
                        let recurse = match inf_op {
                            Hook::Complete(inf_op) => Recurse::InfixOp(inf_op, lhs),
                            Hook::Recurse(bp, hook) => {
                                min_bp = bp;
                                Recurse::HookFinishInfix(hook, lhs)
                            }
                        };

                        stack.push(StackItem::Raw(RawSI {
                            min_bp: old_min_bp,
                            recurse,
                        }));

                        continue 'recurse
                    }
                }
            }
        }
    }
}
