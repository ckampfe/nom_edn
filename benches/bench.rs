#[macro_use]
extern crate criterion;

use criterion::Criterion;
use nom_edn;
use std::io::Read;

fn deps_edn(c: &mut Criterion) {
    let mut edn = std::fs::File::open("./fixtures/deps.edn").unwrap();

    let mut buf = Vec::new();

    edn.read_to_end(&mut buf).unwrap();

    c.bench_function("deps.edn", move |b| b.iter(|| nom_edn::parse_bytes(&buf)));
}

criterion_group!(benches, deps_edn);
criterion_main!(benches);
