#[macro_use]
extern crate criterion;

use criterion::Criterion;
use nom_edn::*;

fn deps_edn(c: &mut Criterion) {
    let edn = include_str!("../fixtures/deps.edn");

    c.bench_function("deps.edn", move |b| b.iter(|| edn!(&edn)));
}

fn unicode_char_found(c: &mut Criterion) {
    let chr = "\\u3F3A";

    c.bench_function("unicode char found", move |b| b.iter(|| edn!(&chr)));
}

fn char_unfound(c: &mut Criterion) {
    let chr = "\\u3Z3Z";

    c.bench_function("unicode char unfound", move |b| b.iter(|| edn!(&chr)));
}

criterion_group!(benches, deps_edn, unicode_char_found, char_unfound);
criterion_main!(benches);
