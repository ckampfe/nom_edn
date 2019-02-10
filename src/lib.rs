use nom::*;
use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq)]
pub enum Edn<'a> {
    Nil,
    Bool(bool),
    String(&'a str),
    // Character(char), TODO
    Symbol(String),
    Keyword(String),
    Integer(isize),
    // Float(f64), TODO
    List(Vec<Edn<'a>>),
    Vector(Vec<Edn<'a>>),
    Map(HashMap<Edn<'a>, Edn<'a>>),
    Set(HashSet<Edn<'a>>),
    // TODO: Tagged
    // TODO: handle comments ;;
    // TODO: handle reader macro `#_`
}

impl<'a> std::hash::Hash for Edn<'a> {
    fn hash<H>(&self, _state: &mut H) {
        // unimplemented!()
    }
}

impl<'a> Eq for Edn<'a> {
    // fn cmp(self) -> bool {
    //     unimplemented!()
    // }
}

named!(edn_nil<crate::Edn>, do_parse!(tag!("nil") >> (Edn::Nil)));

const TRUEBYTES: &[u8] = b"true";
const FALSEBYTES: &[u8] = b"false";

named!(
    edn_bool<crate::Edn>,
    do_parse!(
        res: map!(alt!(tag!("true") | tag!("false")), |value| match value {
            _ if value == TRUEBYTES => Edn::Bool(true),
            _ if value == FALSEBYTES => Edn::Bool(false),
            _ => panic!("nonbool matched, definitely an error."),
        }) >> (res)
    )
);

named!(
    edn_int<crate::Edn>,
    do_parse!(
        i: map!(
            pair!(
                opt!(alt!(tag!("+") | tag!("-"))), // maybe sign?
                digit1
            ),
            |(sign, digits)| {
                let i = if let Some(s) = sign {
                    let nstr = std::str::from_utf8(digits).unwrap();
                    let n = nstr.parse::<isize>().unwrap();
                    if s == b"-" {
                        -n
                    } else {
                        n
                    }
                } else {
                    let nstr = std::str::from_utf8(digits).unwrap();
                    nstr.parse::<isize>().unwrap()
                };

                Edn::Integer(i)
            }
        ) >> (i)
    )
);

named!(
    edn_string<crate::Edn>,
    do_parse!(
        tag!("\"")
            >> s: map!(escaped!(none_of!("\"\\"), '\\', one_of!("\"ntr\\")), |s| {
                Edn::String(std::str::from_utf8(s).unwrap())
            })
            >> tag!("\"")
            >> (s)
    )
);

named!(
    edn_keyword<crate::Edn>,
    do_parse!(
        tag!(":")
            >> namespace: opt!(pair!(take_while1!(matches_identifier), tag!("/")))
            >> kws: take_while1!(matches_identifier)
            >> (if let Some((ns, slash)) = namespace {
                Edn::Keyword(std::string::String::from_utf8(vec![ns, slash, kws].concat()).unwrap())
            } else {
                Edn::Keyword(std::string::String::from_utf8(kws.to_vec()).unwrap())
            })
    )
);

named!(
    edn_symbol<crate::Edn>,
    do_parse!(
        peek!(not!(tag!(":")))
            >> namespace: opt!(pair!(take_while1!(matches_identifier), tag!("/")))
            >> sym: take_while1!(matches_identifier)
            >> (if let Some((ns, slash)) = namespace {
                Edn::Symbol(std::string::String::from_utf8(vec![ns, slash, sym].concat()).unwrap())
            } else {
                Edn::Symbol(std::string::String::from_utf8(sym.to_vec()).unwrap())
            })
    )
);

named!(
    edn_list<crate::Edn>,
    do_parse!(
        tag!("(")
            >> elements: opt!(many1!(ws!(edn_any)))
            >> tag!(")")
            >> (Edn::List(elements.unwrap_or_else(Vec::new)))
    )
);

named!(
    edn_vector<crate::Edn>,
    do_parse!(
        alt!(tag!("[") | ws!(tag!("[")))
            >> elements: opt!(many1!(alt!(ws!(edn_any) | edn_any)))
            >> alt!(tag!("]") | ws!(tag!("]")))
            >> (Edn::Vector(elements.unwrap_or_else(Vec::new)))
    )
);

named!(
    edn_map<crate::Edn>,
    do_parse!(
        alt!(tag!("{") | ws!(tag!("{")))
            >> map: opt!(fold_many1!(
                pair!(alt!(ws!(edn_any) | edn_any), alt!(ws!(edn_any) | edn_any)),
                HashMap::new(),
                |mut acc: HashMap<_, _>, (k, v)| {
                    acc.insert(k, v);
                    acc
                }
            ))
            >> alt!(tag!("}") | ws!(tag!("}")))
            >> (Edn::Map(map.unwrap_or_else(HashMap::new)))
    )
);

named!(
    edn_set<crate::Edn>,
    do_parse!(
        alt!(tag!("#{") | ws!(tag!("#{")))
            >> set: opt!(fold_many1!(
                alt!(ws!(edn_any) | edn_any),
                HashSet::new(),
                |mut acc: HashSet<_>, v| {
                    acc.insert(v);
                    acc
                }
            ))
            >> alt!(tag!("}") | ws!(tag!("}")))
            >> (Edn::Set(set.unwrap_or_else(HashSet::new)))
    )
);

named!(
    edn_any<crate::Edn>,
    alt!(
        edn_nil
            | edn_list
            | edn_map
            | edn_vector
            | edn_set
            | edn_bool
            | edn_int
            | edn_keyword
            | edn_string
            | edn_symbol // | edn_float
                         // | edn_char
    )
);

fn matches_identifier(c: u8) -> bool {
    is_alphanumeric(c) || c == b'-' || c == b'_' || c == b'.'
}

#[cfg(test)]
mod tests {
    use super::Edn::*;
    use super::*;
    use std::io::prelude::*;

    macro_rules! hashmap {
        () => {
            std::collections::HashMap::new()
        };
        ( $($x:expr, $y:expr),* ) => {
            {
                let mut hm = std::collections::HashMap::new();

                $(
                    hm.insert($x, $y);
                )*

                    hm
            }
        };
    }

    macro_rules! hashset {
        () => {
            std::collections::HashSet::new()
        };
        ( $($x:expr),* ) => {
            {
                let mut hs = std::collections::HashSet::new();

                $(
                    hs.insert($x);
                )*

                    hs
            }
        };
    }

    #[test]
    fn parses_nil() {
        let nil = "nil";
        let res = edn_nil(nil.as_bytes());
        assert_eq!(res, Ok((vec!().as_slice(), Edn::Nil)));
    }

    #[test]
    fn parses_bools() {
        let truestr = "true";
        let falsestr = "false";
        let trueres = edn_bool(truestr.as_bytes());
        let falseres = edn_bool(falsestr.as_bytes());
        assert_eq!(trueres, Ok((vec!().as_slice(), Bool(true))));
        assert_eq!(falseres, Ok((vec!().as_slice(), Bool(false))));
    }

    #[test]
    fn parses_keywords() {
        let keystr = ":a-kw ";
        let res = edn_keyword(keystr.as_bytes());
        assert_eq!(res, Ok((vec!(32).as_slice(), Keyword("a-kw".to_string()))));
    }

    #[test]
    fn parses_namespaced_keywords() {
        let keystr = ":org.clojure/clojure ";
        let res = edn_keyword(keystr.as_bytes());
        assert_eq!(
            res,
            Ok((
                vec!(32).as_slice(),
                Keyword("org.clojure/clojure".to_string())
            ))
        );
    }

    #[test]
    fn parses_ints() {
        let intstr = "1 ";
        let res = edn_int(intstr.as_bytes());
        assert_eq!(res, Ok((vec!(32).as_slice(), Integer(1))));
    }

    #[test]
    fn parses_symbols() {
        let symstr = "a-sym ";
        let res = edn_symbol(symstr.as_bytes());
        assert_eq!(res, Ok((vec!(32).as_slice(), Symbol("a-sym".to_string()))));
    }

    #[test]
    fn parses_namedspaced_symbols() {
        let symstr = "org.clojure/clojure ";
        let res = edn_symbol(symstr.as_bytes());
        assert_eq!(
            res,
            Ok((
                vec!(32).as_slice(),
                Symbol("org.clojure/clojure".to_string())
            ))
        );
    }

    #[test]
    fn parses_strings() {
        let strstr = "\"hello\"";
        let res = edn_string(strstr.as_bytes());
        assert_eq!(res, Ok((vec!().as_slice(), String("hello"))));
    }

    #[test]
    fn parses_strings_with_escapes() {
        let mut embedded_str = std::fs::File::open("./fixtures/embedded_str").unwrap();
        // let embedded_str = "\"hel\"lo\"";
        let mut buf = Vec::new();
        embedded_str.read_to_end(&mut buf).unwrap();
        let embedded_res = edn_string(&mut buf);
        assert_eq!(embedded_res, Ok((vec!(10).as_bytes(), String("hel\\\"lo"))));
    }

    #[test]
    fn parses_homogenous_lists() {
        let list_str = "(:a :b :c)";
        let list_res = edn_list(list_str.as_bytes());
        assert_eq!(
            list_res,
            Ok((
                vec!().as_slice(),
                List(vec!(
                    Keyword("a".to_string()),
                    Keyword("b".to_string()),
                    Keyword("c".to_string())
                ))
            ))
        );
    }

    #[test]
    fn parses_heterogenous_lists() {
        let list_str = "(:a b true false some-sym :c)";
        let list_res = edn_list(list_str.as_bytes());
        assert_eq!(
            list_res,
            Ok((
                vec!().as_slice(),
                List(vec!(
                    Keyword("a".to_string()),
                    Symbol("b".to_string()),
                    Bool(true),
                    Bool(false),
                    Symbol("some-sym".to_string()),
                    Keyword("c".to_string())
                ))
            ))
        );
    }

    #[test]
    fn parses_heterogenous_nested_lists() {
        let list_str = "(:a b (1 2 5 :e) true false [[] 232 ()] some-sym :c)";
        let list_res = edn_list(list_str.as_bytes());
        assert_eq!(
            list_res,
            Ok((
                vec!().as_slice(),
                List(vec!(
                    Keyword("a".to_string()),
                    Symbol("b".to_string()),
                    List(vec!(
                        Integer(1),
                        Integer(2),
                        Integer(5),
                        Keyword("e".to_string())
                    )),
                    Bool(true),
                    Bool(false),
                    Vector(vec!(Vector(vec!()), Integer(232), List(vec!()))),
                    Symbol("some-sym".to_string()),
                    Keyword("c".to_string())
                ))
            ))
        );
    }

    #[test]
    fn parses_homogenous_vectors() {
        let vector_str = "[:a :b :c]";
        let vector_res = edn_vector(vector_str.as_bytes());
        assert_eq!(
            vector_res,
            Ok((
                vec!().as_slice(),
                Vector(vec!(
                    Keyword("a".to_string()),
                    Keyword("b".to_string()),
                    Keyword("c".to_string())
                ))
            ))
        );
    }

    #[test]
    fn parses_heterogenous_vectors() {
        let vector_str = "[:a b 1 true false some-sym 44444 :c]";
        let vector_res = edn_vector(vector_str.as_bytes());
        assert_eq!(
            vector_res,
            Ok((
                vec!().as_slice(),
                Vector(vec!(
                    Keyword("a".to_string()),
                    Symbol("b".to_string()),
                    Integer(1),
                    Bool(true),
                    Bool(false),
                    Symbol("some-sym".to_string()),
                    Integer(44444),
                    Keyword("c".to_string())
                ))
            ))
        );
    }

    #[test]
    fn parses_empty_vector() {
        let vector_str = "[]";
        let vector_res = edn_vector(vector_str.as_bytes());
        assert_eq!(vector_res, Ok((vec!().as_slice(), Vector(vec!()))));
    }

    #[test]
    fn parses_heterogenous_nested_vectors() {
        let vector_str = "[:a b true false [[] 232 ()] some-sym :c]";
        let vector_res = edn_vector(vector_str.as_bytes());
        assert_eq!(
            vector_res,
            Ok((
                vec!().as_slice(),
                Vector(vec!(
                    Keyword("a".to_string()),
                    Symbol("b".to_string()),
                    Bool(true),
                    Bool(false),
                    Vector(vec!(Vector(vec!()), Integer(232), List(vec!()))),
                    Symbol("some-sym".to_string()),
                    Keyword("c".to_string())
                ))
            ))
        );
    }

    #[test]
    fn parses_maps() {
        let map_str = "{:a 1}";
        let map_res = edn_map(map_str.as_bytes());
        assert_eq!(
            map_res,
            Ok((
                vec!().as_slice(),
                Map(hashmap!(Keyword("a".to_string()), Integer(1)))
            ))
        );
    }

    #[test]
    fn parses_empty_maps() {
        let map_str = "{}";
        let map_res = edn_map(map_str.as_bytes());
        assert_eq!(map_res, Ok((vec!().as_slice(), Map(hashmap!()))));
    }

    #[test]
    fn parses_nested_maps_values() {
        let map_str = "{:a [1 2 3]}";
        let map_res = edn_map(map_str.as_bytes());
        assert_eq!(
            map_res,
            Ok((
                vec!().as_slice(),
                Map(hashmap!(
                    Keyword("a".to_string()),
                    Vector(vec!(Integer(1), Integer(2), Integer(3)))
                ))
            ))
        );
    }

    #[test]
    fn parses_nested_maps_keys() {
        let map_str = "{[1 2 3] :a\n {} :bcd {} :zzzzzzzz}"; // :bcd should be gone, as :zzzzzzzz overwrites
        let map_res = edn_map(map_str.as_bytes());
        assert_eq!(
            map_res,
            Ok((
                vec!().as_slice(),
                Map(hashmap!(
                    Vector(vec!(Integer(1), Integer(2), Integer(3))),
                    Keyword("a".to_string()),
                    Map(hashmap!()),
                    Keyword("bcd".to_string()),
                    Map(hashmap!()),
                    Keyword("zzzzzzzz".to_string())
                ))
            ))
        );
    }

    #[test]
    fn parses_sets() {
        let set_str = "#{:a 1}";
        let set_res = edn_set(set_str.as_bytes());
        assert_eq!(
            set_res,
            Ok((
                vec!().as_slice(),
                Set(hashset!(Keyword("a".to_string()), Integer(1)))
            ))
        );
    }

    #[test]
    fn parses_empty_sets() {
        let set_str = "#{}";
        let set_res = edn_set(set_str.as_bytes());
        assert_eq!(set_res, Ok((vec!().as_slice(), Set(hashset!()))));
    }

    #[test]
    fn parses_nested_sets_values() {
        let set_str = "#{:a [1 2 3]}";
        let set_res = edn_set(set_str.as_bytes());
        assert_eq!(
            set_res,
            Ok((
                vec!().as_slice(),
                Set(hashset!(
                    Keyword("a".to_string()),
                    Vector(vec!(Integer(1), Integer(2), Integer(3)))
                ))
            ))
        );
    }

    #[test]
    fn parses_nested_sets_keys() {
        let set_str = "#{[1 2 3] :a\n {} :bcd {} #{} [] (1 2 #{}) :zzzzzzzz}"; // only one nested empty map
        let set_res = edn_set(set_str.as_bytes());
        assert_eq!(
            set_res,
            Ok((
                vec!().as_slice(),
                Set(hashset!(
                    Vector(vec!(Integer(1), Integer(2), Integer(3))),
                    Keyword("a".to_string()),
                    Map(hashmap!()),
                    Keyword("bcd".to_string()),
                    Set(hashset!()),
                    Vector(vec!()),
                    List(vec!(Integer(1), Integer(2), Set(hashset!()))),
                    Keyword("zzzzzzzz".to_string())
                ))
            ))
        );
    }

    #[test]
    fn parses_a_real_one() {
        let mut edn = std::fs::File::open("./fixtures/deps.edn").unwrap();
        let mut buf = Vec::new();
        edn.read_to_end(&mut buf).unwrap();
        let start = std::time::Instant::now();
        let embedded_res = edn_any(&mut buf);
        let end = std::time::Instant::now();

        println!("{:?}", end - start);

        assert_eq!(
            embedded_res,
            Ok((
                vec!(10).as_slice(),
                Map(hashmap!(
                    Keyword("paths".to_string()),
                    Vector(vec!(String("resources"), String("src"))),
                    Keyword("deps".to_string()),
                    Map(hashmap!(
                        Symbol("org.clojure/clojure".to_string()),
                        Map(hashmap!(
                            Keyword("mvn/version".to_string()),
                            String("1.10.0")
                        )),
                        Symbol("instaparse".to_string()),
                        Map(hashmap!(
                            Keyword("mvn/version".to_string()),
                            String("1.4.9")
                        )),
                        Symbol("quil".to_string()),
                        Map(hashmap!(
                            Keyword("mvn/version".to_string()),
                            String("2.8.0"),
                            Keyword("exclusions".to_string()),
                            Vector(vec!(Symbol("com.lowagie/itext".to_string())))
                        ),),
                        Symbol("com.hypirion/clj-xchart".to_string()),
                        Map(hashmap!(
                            Keyword("mvn/version".to_string()),
                            String("0.2.0")
                        )),
                        Symbol("net.mikera/core.matrix".to_string()),
                        Map(hashmap!(
                            Keyword("mvn/version".to_string()),
                            String("0.62.0")
                        )),
                        Symbol("net.mikera/vectorz-clj".to_string()),
                        Map(hashmap!(
                            Keyword("mvn/version".to_string()),
                            String("0.48.0")
                        ))
                    )),
                    Keyword("aliases".to_string()),
                    Map(hashmap!(
                        Keyword("more-mem".to_string()),
                        Map(hashmap!(
                            Keyword("jvm-opts".to_string()),
                            Vector(vec!(String("-Xmx12G -Xms12G")))
                        ),),
                        Keyword("test".to_string()),
                        Map(hashmap!(
                            Keyword("extra-paths".to_string()),
                            Vector(vec!(String("test"))),
                            Keyword("extra-deps".to_string()),
                            Map(hashmap!(
                                Symbol("org.clojure/test.check".to_string()),
                                Map(hashmap!(
                                    Keyword("mvn/version".to_string()),
                                    String("RELEASE")
                                ))
                            ))
                        )),
                        Keyword("runner".to_string()),
                        Map(hashmap!(
                            Keyword("extra-deps".to_string()),
                            Map(hashmap!(
                                Symbol("com.cognitect/test-runner".to_string()),
                                Map(hashmap!(
                                    Keyword("git/url".to_string()),
                                    String("https://github.com/cognitect-labs/test-runner"),
                                    Keyword("sha".to_string()),
                                    String("76568540e7f40268ad2b646110f237a60295fa3c")
                                ),)
                            )),
                            Keyword("main-opts".to_string()),
                            Vector(vec!(
                                String("-m"),
                                String("cognitect.test-runner"),
                                String("-d"),
                                String("test")
                            ),)
                        ))
                    ),)
                ))
            ))
        )
    }
}
