#[macro_use]
extern crate criterion;

use criterion::Criterion;
use nom_edn::*;

fn deps_edn(c: &mut Criterion) {
    let edn = include_str!("../fixtures/deps.edn");

    c.bench_function("deps.edn", move |b| b.iter(|| edn!(&edn)));
}

criterion_group!(benches, deps_edn);
criterion_main!(benches);
