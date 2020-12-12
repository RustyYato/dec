use crate::{error::{ParseError, PrattErrorKind}, prelude::{Error::Error, ErrorKind, PResult}, traits::{ParseMut, ParseOnce}};

use core::convert::Infallible;

#[cfg(test)]
mod test;

pub trait Stack {
    type Item;

    fn clear(&mut self) { while let Some(_) = self.pop() {} }
    fn push(&mut self, value: Self::Item);
    fn pop(&mut self) -> Option<Self::Item>;
}

impl<S: ?Sized + Stack> Stack for &mut S {
    type Item = S::Item;

    fn clear(&mut self) { S::clear(self) }
    fn push(&mut self, value: Self::Item) { S::push(self, value) }
    fn pop(&mut self) -> Option<Self::Item> { S::pop(self) }
}

impl<S: ?Sized + Stack> Stack for Box<S> {
    type Item = S::Item;

    fn clear(&mut self) { S::clear(self) }
    fn push(&mut self, value: Self::Item) { S::push(self, value) }
    fn pop(&mut self) -> Option<Self::Item> { S::pop(self) }
}

impl<T> Stack for Vec<T> {
    type Item = T;

    fn push(&mut self, value: Self::Item) { self.push(value); }
    fn pop(&mut self) -> Option<Self::Item> { self.pop() }
    fn clear(&mut self) { self.clear() }
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

pub struct StackItem<I, E: ParseError<I>, P: Pratt<I, E>> {
    min_bp: P::BindingPower,
    recurse: Recurse<I, E, P>,
}

enum Recurse<I, E: ParseError<I>, P: Pratt<I, E>> {
    PrefixOp(P::PrefixOp),
    InfixOp(P::InfixOp, P::Value),
    Value(P::HookBuildValue),
    HookFinishInfix(P::HookFinishInfix, P::Value),
    HookMergePrefix(P::HookMergePrefix),
    HookMergeInfix(P::HookMergeInfix),
    HookMergePostfix(P::HookMergePostfix),
}

pub type PrefixOp<I, E, P> = Operator<(), <P as Pratt<I, E>>::PrefixOp, <P as Pratt<I, E>>::Value>;
pub type InfixOp<I, E, P> = Operator<<P as Pratt<I, E>>::Value, <P as Pratt<I, E>>::InfixOp, <P as Pratt<I, E>>::Value>;
pub type PostfixOp<I, E, P> = Operator<<P as Pratt<I, E>>::Value, <P as Pratt<I, E>>::PostfixOp, ()>;

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
pub trait Pratt<I, E: ParseError<I>> {
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
    ) -> PResult<I, Hook<Self::Value, Self::HookBuildValue, Self::BindingPower>, E>;

    fn prefix_op(&mut self, input: I) -> PResult<I, Operator<(), Self::PrefixOp, Self::BindingPower>, ()> {
        Err(Error(()))
    }

    fn infix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<Self::BindingPower, Self::FastInfixOp, Self::BindingPower>, ()> {
        Err(Error(()))
    }

    fn postfix_op(&mut self, input: I) -> PResult<I, Operator<Self::BindingPower, Self::PostfixOp, ()>, ()> {
        Err(Error(()))
    }

    fn finish_infix_op(
        &mut self,
        args: Args<Self::FastInfixOp, Self::HookFinishInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::InfixOp, Self::HookFinishInfix, Self::BindingPower>, E> {
        Err(Error(E::from_input_kind(input, ErrorKind::Pratt(PrattErrorKind::FinishInfixOp))))
    }

    fn merge_prefix_op(
        &mut self,
        args: Args<PrefixOp<I, E, Self>, Self::HookMergePrefix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePrefix, Self::BindingPower>, E> {
        Err(Error(E::from_input_kind(input, ErrorKind::Pratt(PrattErrorKind::MergePrefixOp))))
    }

    fn merge_infix_op(
        &mut self,
        args: Args<InfixOp<I, E, Self>, Self::HookMergeInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergeInfix, Self::BindingPower>, E> {
        Err(Error(E::from_input_kind(input, ErrorKind::Pratt(PrattErrorKind::MergeInfixOp))))
    }

    fn merge_postfix_op(
        &mut self,
        args: Args<PostfixOp<I, E, Self>, Self::HookMergePostfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePostfix, Self::BindingPower>, E> {
        Err(Error(E::from_input_kind(input, ErrorKind::Pratt(PrattErrorKind::MergePostfixOp))))
    }
}

impl<I, E: ParseError<I>, P: Pratt<I, E> + ?Sized> Pratt<I, E> for &mut P {
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
    ) -> PResult<I, Hook<Self::Value, Self::HookBuildValue, Self::BindingPower>, E> {
        P::value(self, args, input)
    }

    fn prefix_op(&mut self, input: I) -> PResult<I, Operator<(), Self::PrefixOp, Self::BindingPower>, ()> {
        P::prefix_op(self, input)
    }

    fn infix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<Self::BindingPower, Self::FastInfixOp, Self::BindingPower>, ()> {
        P::infix_op(self, input)
    }

    fn postfix_op(&mut self, input: I) -> PResult<I, Operator<Self::BindingPower, Self::PostfixOp, ()>, ()> {
        P::postfix_op(self, input)
    }

    fn finish_infix_op(
        &mut self,
        args: Args<Self::FastInfixOp, Self::HookFinishInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::InfixOp, Self::HookFinishInfix, Self::BindingPower>, E> {
        P::finish_infix_op(self, args, input)
    }

    fn merge_prefix_op(
        &mut self,
        args: Args<PrefixOp<I, E, Self>, Self::HookMergePrefix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePrefix, Self::BindingPower>, E> {
        P::merge_prefix_op(self, args, input)
    }

    fn merge_infix_op(
        &mut self,
        args: Args<InfixOp<I, E, Self>, Self::HookMergeInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergeInfix, Self::BindingPower>, E> {
        P::merge_infix_op(self, args, input)
    }

    fn merge_postfix_op(
        &mut self,
        args: Args<PostfixOp<I, E, Self>, Self::HookMergePostfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePostfix, Self::BindingPower>, E> {
        P::merge_postfix_op(self, args, input)
    }
}

impl<I, E: ParseError<I>, P: Pratt<I, E> + ?Sized> Pratt<I, E> for Box<P> {
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
    ) -> PResult<I, Hook<Self::Value, Self::HookBuildValue, Self::BindingPower>, E> {
        P::value(self, args, input)
    }

    fn prefix_op(&mut self, input: I) -> PResult<I, Operator<(), Self::PrefixOp, Self::BindingPower>, ()> {
        P::prefix_op(self, input)
    }

    fn infix_op(
        &mut self,
        input: I,
    ) -> PResult<I, Operator<Self::BindingPower, Self::FastInfixOp, Self::BindingPower>, ()> {
        P::infix_op(self, input)
    }

    fn postfix_op(&mut self, input: I) -> PResult<I, Operator<Self::BindingPower, Self::PostfixOp, ()>, ()> {
        P::postfix_op(self, input)
    }

    fn finish_infix_op(
        &mut self,
        args: Args<Self::FastInfixOp, Self::HookFinishInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::InfixOp, Self::HookFinishInfix, Self::BindingPower>, E> {
        P::finish_infix_op(self, args, input)
    }

    fn merge_prefix_op(
        &mut self,
        args: Args<PrefixOp<I, E, Self>, Self::HookMergePrefix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePrefix, Self::BindingPower>, E> {
        P::merge_prefix_op(self, args, input)
    }

    fn merge_infix_op(
        &mut self,
        args: Args<InfixOp<I, E, Self>, Self::HookMergeInfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergeInfix, Self::BindingPower>, E> {
        P::merge_infix_op(self, args, input)
    }

    fn merge_postfix_op(
        &mut self,
        args: Args<PostfixOp<I, E, Self>, Self::HookMergePostfix, Self::Value>,
        input: I,
    ) -> PResult<I, Hook<Self::Value, Self::HookMergePostfix, Self::BindingPower>, E> {
        P::merge_postfix_op(self, args, input)
    }
}

fn unwrap<I, E, T, C, P, A, F>(
    pratt: &mut P,
    mut result: Hook<T, C, P::BindingPower>,
    mut input: I,
    mut f: F,
) -> PResult<I, T, E>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E>,
    F: FnMut(&mut P, Args<A, C, P::Value>, I) -> PResult<I, Hook<T, C, P::BindingPower>, E>,
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

fn recurse_pratt<I, E, P>(pratt: &mut P, input: I, min_bp: P::BindingPower) -> PResult<I, P::Value, E>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E>,
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

impl<I, E, P> ParseOnce<I, E> for RecursePratt<P>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E>,
{
    type Output = P::Value;

    fn parse_once(mut self, input: I) -> PResult<I, Self::Output, E> { self.parse_mut(input) }
}

impl<I, E, P> ParseMut<I, E> for RecursePratt<P>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E>,
{
    fn parse_mut(&mut self, input: I) -> PResult<I, Self::Output, E> {
        let Self(pratt) = self;
        recurse_pratt(pratt, input, Default::default())
    }
}

impl<I, E, P, S> ParseOnce<I, E> for StackPratt<P, S>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E>,
    S: Stack<Item = StackItem<I, E, P>>,
{
    type Output = P::Value;

    fn parse_once(mut self, input: I) -> PResult<I, Self::Output, E> { self.parse_mut(input) }
}

impl<I, E, P, S> ParseMut<I, E> for StackPratt<P, S>
where
    I: Clone,
    E: ParseError<I>,
    P: Pratt<I, E>,
    S: Stack<Item = StackItem<I, E, P>>,
{
    fn parse_mut(&mut self, mut input: I) -> PResult<I, Self::Output, E> {
        let Self { stack, pratt } = self;
        stack.clear();
        let mut min_bp = P::BindingPower::default();

        'recurse: loop {
            macro_rules! merge {
                ($hook:expr, $lhs:ident <- $value:expr, $f:expr, $g:expr $(,)?) => {{
                    let (next_input, value) = $f(pratt, Args::Recurse($hook, $value), input)?;
                    input = next_input;
                    match value {
                        Hook::Complete(value) => $lhs = value,
                        Hook::Recurse(bp, hook) => {
                            stack.push(StackItem {
                                min_bp,
                                recurse: $g(hook),
                            });
                            min_bp = bp;
                            continue 'recurse
                        }
                    }
                }};
            }

            let mut lhs = match pratt.prefix_op(input.clone()) {
                Ok((next_input, prefix)) => {
                    stack.push(StackItem {
                        min_bp,
                        recurse: Recurse::PrefixOp(prefix.op),
                    });
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
                            stack.push(StackItem {
                                min_bp,
                                recurse: Recurse::Value(hook),
                            });
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
                                        stack.push(StackItem {
                                            min_bp,
                                            recurse: Recurse::HookMergePostfix(hook),
                                        });
                                        min_bp = bp;
                                        continue 'recurse
                                    }
                                }
                            }
                        }
                        Err(_) => match pratt.infix_op(input.clone()) {
                            Ok((next_input, infix)) if infix.left >= min_bp => {
                                let (next_input, inf_op) = pratt.finish_infix_op(Args::Normal(infix.op), next_input)?;

                                stack.push(StackItem {
                                    min_bp,
                                    recurse: match inf_op {
                                        Hook::Complete(inf_op) => {
                                            min_bp = infix.right;
                                            Recurse::InfixOp(inf_op, lhs)
                                        }
                                        Hook::Recurse(bp, hook) => {
                                            min_bp = bp;
                                            Recurse::HookFinishInfix(hook, lhs)
                                        }
                                    },
                                });

                                input = next_input;
                                continue 'recurse
                            }
                            _ => break 'main_loop lhs,
                        },
                    }
                };

                let StackItem {
                    min_bp: old_min_bp,
                    recurse,
                } = match stack.pop() {
                    Some(stack_item) => stack_item,
                    None => return Ok((input, value)),
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
                                stack.push(StackItem {
                                    min_bp,
                                    recurse: Recurse::HookMergePrefix(hook),
                                });
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
                                stack.push(StackItem {
                                    min_bp,
                                    recurse: Recurse::HookMergeInfix(hook),
                                });
                                min_bp = bp;
                                continue 'recurse
                            }
                        };
                    }
                    Recurse::HookFinishInfix(hook, lhs) => {
                        let (next_input, inf_op) = pratt.finish_infix_op(Args::Recurse(hook, value), input)?;
                        input = next_input;
                        stack.push(StackItem {
                            min_bp,
                            recurse: match inf_op {
                                Hook::Complete(inf_op) => Recurse::InfixOp(inf_op, lhs),
                                Hook::Recurse(bp, hook) => {
                                    min_bp = bp;
                                    Recurse::HookFinishInfix(hook, lhs)
                                }
                            },
                        });
                        continue 'recurse
                    }
                }
            }
        }
    }
}
