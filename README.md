nom_edn
=======

A nom parser for [edn](https://github.com/edn-format/edn)

Warning: this is alpha-quality software.
There isn't a real public API right now,
and test coverage/quality need to improve.
That said, it is probably complete enough that you could
build a Lisp interpreter with it.

Here's a test as an example:

```rust
#[test]
fn parses_nested_maps_values() {
    let map_str = "{:a [1 2 4.01]}";
    let map_res = edn_map(map_str.as_bytes());
    assert_eq!(
        map_res,
        Ok((
            vec!().as_slice(),
            Map(hashmap!(
                Keyword("a".to_string()),
                Vector(vec!(Integer(1), Integer(2), Float(4.01)))
            ))
        ))
    );
}
```

If your data could be any edn form, use `edn_any`.
The above test is a unit test specifically for exercising map parsing,
while `edn_any` will try to parse all valid edn (at least, all valid edn that is built so far).

What's built so far:

- [x] nil
- [x] booleans
- [x] strings
- [x] chars
- [x] symbols
- [x] keywords
- [x] integers
- [x] floats
- [x] lists
- [x] vectors
- [x] maps
- [x] sets
- [ ] builtin tagged elements (inst, uuid, etc)
- [ ] comments (;)
- [x] discard
- [ ] commas as whitespace
- [ ] a real public API
- [ ] real Rust trait implementations of `Hash`/`Eq` for `Edn` (stub impls right now)
- [ ] user-defined tagged elements
- [ ] tests to show UTF-8 compliance
- [ ] tests of streaming
- [ ] better tests around namespaced keywords/symbols
- [ ] real-world tests of a few real (large) source files
- [ ] get rid of as many `.unwrap()`s as possible
- [x] optional hashbrown for hashmaps/hashsets instead of stdlib
- [ ] benchmarking for fun
