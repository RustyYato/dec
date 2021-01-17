use criterion::{black_box, criterion_group, criterion_main, Criterion};

use dec::tag::tag;
use dec_core::{
    error::{CaptureInput, PResult},
    ParseOnce,
};

pub fn separated(input: &[u8]) -> PResult<&[u8], (u32, u32), CaptureInput<&[u8]>> {
    dec::seq::SeparatedFold {
        item: tag(b'a'),
        sep: tag(b'b'),
        item_func: |(i, s), _| (i + 1, s),
        sep_func: |(i, s), _| (i, s + 1),
        mk_acc: || (0, 0),
    }
    .parse_once(input)
}

pub fn manual(input: &[u8]) -> PResult<&[u8], (u32, u32), CaptureInput<&[u8]>> {
    if input.first() != Some(&b'a') {
        return Ok((input, (0, 0)))
    }

    let mut input = input[1..].iter();

    let mut i = 1;
    let mut s = 0;

    loop {
        if input.next() != Some(&b'b') {
            return Ok((input.as_slice(), (i, s)))
        }

        if input.next() != Some(&b'a') {
            return Ok((input.as_slice(), (i, s)))
        }

        i += 1;
        s += 1;
    }
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let input = b"abababababababababa";

    assert_eq!(separated(input), manual(input));

    c.bench_function("manual", |b| b.iter(|| manual(black_box(input))));
    c.bench_function("separated", |b| b.iter(|| separated(black_box(input))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
