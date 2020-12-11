use super::*;
use crate::{branch::Any, map::Value, prelude::*, tag::Tag};

#[derive(Debug, PartialEq, Eq)]
enum PrefixOp {
    Pos,
    Neg,
    Tilda,
}

#[derive(Debug, PartialEq, Eq)]
enum Simple {
    Equal,
    Add,
    Sub,
    Mul,
    Div,
    Dot,
}

#[derive(Debug, PartialEq, Eq)]
enum InfixOp<T = ()> {
    Ternary(T),
    Simple(Simple),
}

#[derive(Debug, PartialEq, Eq)]
enum PostfixOp<I = ()> {
    Index(I),
    Bang,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Prefix<'a> {
    pre_op: PrefixOp,
    right: Expr<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Infix<'a> {
    left: Expr<'a>,
    inf_op: InfixOp<Expr<'a>>,
    right: Expr<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Postfix<'a> {
    left: Expr<'a>,
    post_op: PostfixOp<Expr<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
enum Expr<'a> {
    Ident(&'a str),
    Integer(u64),
    Prefix(Box<Prefix<'a>>),
    Infix(Box<Infix<'a>>),
    Postfix(Box<Postfix<'a>>),
}

#[derive(Debug)]
struct Ternary;
#[derive(Debug)]
struct Grouped;
#[derive(Debug)]
struct Index<'i>(Expr<'i>);

struct ExprParser;

impl<'i, E: ParseError<&'i str>> Pratt<&'i str, E> for ExprParser {
    type BindingPower = u8;
    type Value = Expr<'i>;

    type PrefixOp = PrefixOp;
    type InfixOp = InfixOp<Expr<'i>>;
    type PostfixOp = PostfixOp;
    type FastInfixOp = InfixOp;

    type HookBuildValue = Grouped;
    type HookFinishInfix = Ternary;
    type HookMergePrefix = Infallible;
    type HookMergeInfix = Infallible;
    type HookMergePostfix = Index<'i>;

    fn value(
        &mut self,
        args: Args<(), Self::HookBuildValue, Self::Value>,
        input: &'i str,
    ) -> PResult<&'i str, Hook<Self::Value, Self::HookBuildValue, Self::BindingPower>, E> {
        let input = input.trim_start();

        match args {
            Args::Normal(()) => (),
            Args::Recurse(Grouped, expr) => {
                let (input, _) = Tag(')').with_context("value (paren group end)").parse_once(input)?;
                return Ok((input.trim_start(), Hook::Complete(expr)))
            }
        }

        if input.starts_with('(') {
            let input = input.split_at(1).1.trim_start();

            return Ok((input, Hook::Recurse(0, Grouped)))
        }

        let pos = input
            .char_indices()
            .find_map(|(pos, c)| if c.is_numeric() { None } else { Some(pos) })
            .unwrap_or(input.len());
        match pos {
            0 => (),
            pos => {
                let (num, input) = input.split_at(pos);
                return Ok((input.trim_start(), Hook::Complete(Expr::Integer(num.parse().unwrap()))))
            }
        }

        let pos = input
            .char_indices()
            .find_map(|(pos, c)| if c.is_alphanumeric() { None } else { Some(pos) })
            .unwrap_or(input.len());
        match pos {
            0 => (),
            pos => {
                let (num, input) = input.split_at(pos);
                return Ok((input.trim_start(), Hook::Complete(Expr::Ident(num))))
            }
        }

        Err(Error::Error(
            E::from_input_kind(input, ErrorKind::Custom("Invalid value")).add_context(input, "value"),
        ))
    }

    fn prefix_op(&mut self, input: &'i str) -> PResult<&'i str, Operator<(), Self::PrefixOp, Self::BindingPower>, ()> {
        let (input, prefix) = Any((
            Value(Operator::prefix_op(PrefixOp::Pos, 11), Tag('+')),
            Value(Operator::prefix_op(PrefixOp::Neg, 11), Tag('-')),
            Value(Operator::prefix_op(PrefixOp::Tilda, 1), Tag('~')),
            //
        ))
        .with_context("prefix op")
        .parse_once(input)?;
        Ok((input.trim_start(), prefix))
    }

    fn infix_op(
        &mut self,
        input: &'i str,
    ) -> PResult<&'i str, Operator<Self::BindingPower, Self::FastInfixOp, Self::BindingPower>, ()> {
        use self::{InfixOp::Simple, Simple::*};

        Any((
            Value(Operator::infix_op(4, Simple(Equal), 3), Tag('=')),
            Value(Operator::infix_op(6, InfixOp::Ternary(()), 3), Tag('?')),
            Value(Operator::infix_op(7, Simple(Add), 7), Tag('+')),
            Value(Operator::infix_op(7, Simple(Sub), 7), Tag('-')),
            Value(Operator::infix_op(9, Simple(Mul), 9), Tag('*')),
            Value(Operator::infix_op(9, Simple(Div), 9), Tag('/')),
            Value(Operator::infix_op(16, Simple(Dot), 15), Tag('.')),
            //
        ))
        .with_context("infix op")
        .parse_once(input)
    }

    fn postfix_op(
        &mut self,
        input: &'i str,
    ) -> PResult<&'i str, Operator<Self::BindingPower, Self::PostfixOp, ()>, ()> {
        Any((
            Value(Operator::postfix_op(13, PostfixOp::Bang), Tag('!')),
            Value(Operator::postfix_op(13, PostfixOp::Index(())), Tag('[')),
            //
        ))
        .with_context("postfix op")
        .parse_once(input)
    }

    fn finish_infix_op(
        &mut self,
        args: Args<Self::FastInfixOp, Self::HookFinishInfix, Self::Value>,
        input: &'i str,
    ) -> PResult<&'i str, Hook<Self::InfixOp, Self::HookFinishInfix, Self::BindingPower>, E> {
        let input = input.trim_start();

        let inf_op = match args {
            Args::Normal(inf_op) => inf_op,
            Args::Recurse(Ternary, value) => {
                let (input, _) = Tag(':').with_context("finish infix op (ternary)").parse_once(input)?;
                return Ok((input.trim_start(), Hook::Complete(InfixOp::Ternary(value))))
            }
        };

        let hook = match inf_op {
            InfixOp::Simple(simple) => Hook::Complete(InfixOp::Simple(simple)),
            InfixOp::Ternary(()) => Hook::Recurse(0, Ternary),
        };

        Ok((input.trim_start(), hook))
    }

    fn merge_prefix_op(
        &mut self,
        args: Args<super::PrefixOp<&'i str, E, Self>, Self::HookMergePrefix, Self::Value>,
        input: &'i str,
    ) -> PResult<&'i str, Hook<Self::Value, Self::HookMergePrefix, Self::BindingPower>, E> {
        let Operator {
            left: (),
            op: pre_op,
            right,
        } = args.always_normal();
        Ok((input, Hook::Complete(Expr::Prefix(Box::new(Prefix { pre_op, right })))))
    }

    fn merge_infix_op(
        &mut self,
        args: Args<super::InfixOp<&'i str, E, Self>, Self::HookMergeInfix, Self::Value>,
        input: &'i str,
    ) -> PResult<&'i str, Hook<Self::Value, Self::HookMergeInfix, Self::BindingPower>, E> {
        let Operator {
            left,
            op: inf_op,
            right,
        } = args.always_normal();
        Ok((
            input,
            Hook::Complete(Expr::Infix(Box::new(Infix { left, inf_op, right }))),
        ))
    }

    fn merge_postfix_op(
        &mut self,
        args: Args<super::PostfixOp<&'i str, E, Self>, Self::HookMergePostfix, Self::Value>,
        input: &'i str,
    ) -> PResult<&'i str, Hook<Self::Value, Self::HookMergePostfix, Self::BindingPower>, E> {
        let input = input.trim_start();

        let (input, left, post_op) = match args {
            Args::Recurse(Index(left), index) => {
                let (input, _) = Tag(']').with_context("merge postfix op (index)").parse_once(input)?;
                (input, left, PostfixOp::Index(index))
            }
            Args::Normal(postfix) => match postfix.op {
                PostfixOp::Bang => (input, postfix.left, PostfixOp::Bang),
                PostfixOp::Index(()) => return Ok((input, Hook::Recurse(0, Index(postfix.left)))),
            },
        };

        Ok((
            input.trim_start(),
            Hook::Complete(Expr::Postfix(Box::new(Postfix { left, post_op }))),
        ))
    }
}

fn expr(input: &str) -> PResult<&str, Expr<'_>, crate::error::verbose::VerboseError<&str>> {
    StackPratt {
        pratt: ExprParser,
        stack: Vec::new(),
    }
    .parse_once(input.trim_start())
}

const INPUT: &str = " a [ b ] . - ( s * g ) ? 10 : 20";

#[test]
fn check_linear() {
    // assert_eq!(expr_rec(INPUT), expr(INPUT));
    panic!("{:#?}", expr(INPUT));
}
