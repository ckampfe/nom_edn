use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take_while1};
use nom::character::complete::{anychar, char, digit1, line_ending, none_of, one_of};
use nom::character::is_alphanumeric;
use nom::combinator::{complete, map, not, opt, peek, recognize, rest};
use nom::multi::{fold_many1, many0};
use nom::number::complete::{double, hex_u32};
use nom::sequence::{delimited, pair, preceded};
use nom::*;
use std::str::FromStr;

use std::collections::{HashMap, HashSet};

type EdnParseResult<'a> =
    Result<(&'a [u8], Option<Edn<'a>>), nom::Err<(&'a [u8], nom::error::ErrorKind)>>;

pub fn parse_bytes(bytes: &[u8]) -> EdnParseResult {
    edn_any(bytes)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Edn<'a> {
    Nil,
    Bool(bool),
    String(&'a str),
    Character(char),
    Symbol(String),
    Keyword(String),
    Integer(isize),
    Float(f64),
    Decimal(rust_decimal::Decimal),
    List(Vec<Edn<'a>>),
    Vector(Vec<Edn<'a>>),
    Map(HashMap<Edn<'a>, Edn<'a>>),
    Set(HashSet<Edn<'a>>),
    // Right now `Comment` is a marker value that follows the edn spec
    // by ignoring any subsequent data, but in the future we could
    // make a variant of it that captures the comment data itself.
    // There could be circumstances where one would want to
    // capture comment data.
    Comment,
    // TODO: handle tagged elements
}

impl<'a> std::hash::Hash for Edn<'a> {
    fn hash<H>(&self, _state: &mut H) {
        // unimplemented!()
    }
}

impl<'a> Eq for Edn<'a> {}

named!(pub space_or_comma, eat_separator!(&b" \t\r\n,"[..]));

macro_rules! ws_or_comma (
    ($i:expr, $($args:tt)*) => (
        {
            sep!($i, space_or_comma, $($args)*)
        }
    )
);

fn edn_discard_sequence(s: &[u8]) -> IResult<&[u8], Option<Edn>> {
    let (s, _) = preceded(tag("#_"), recognize(edn_any))(s)?;

    Ok((s, None))
}

fn edn_nil(s: &[u8]) -> IResult<&[u8], crate::Edn> {
    let (s, _) = tag("nil")(s)?;
    Ok((s, Edn::Nil))
}

fn edn_bool(s: &[u8]) -> IResult<&[u8], crate::Edn> {
    let (s, v) = map(alt((tag("true"), tag("false"))), |value| match value {
        _ if value == b"true" => Edn::Bool(true),
        _ if value == b"false" => Edn::Bool(false),
        _ => panic!("nonbool matched, definitely an error."),
    })(s)?;

    Ok((s, v))
}

fn edn_int(s: &[u8]) -> nom::IResult<&[u8], crate::Edn> {
    let (s, i) = map(
        pair(opt(alt((tag("+"), tag("-")))), digit1),
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
        },
    )(s)?;

    let (s, _) = not(tag("."))(s)?;

    Ok((s, i))
}

fn edn_float(s: &[u8]) -> nom::IResult<&[u8], crate::Edn> {
    let (s, f) = alt((
        map(
            pair(recognize(double), tag("M")),
            |(d, _): (&[u8], &[u8])| {
                Edn::Decimal(
                    rust_decimal::Decimal::from_str(unsafe { std::str::from_utf8_unchecked(d) })
                        .unwrap(),
                )
            },
        ),
        map(double, |d| Edn::Float(d)),
    ))(s)?;

    Ok((s, f))
}

fn edn_string(s: &[u8]) -> IResult<&[u8], crate::Edn> {
    let (s, _) = tag("\"")(s)?;
    let (s, string) = map(escaped(none_of("\"\\"), '\\', one_of("\"ntr\\")), |s| {
        Edn::String(std::str::from_utf8(s).unwrap())
    })(s)?;
    let (s, _) = tag("\"")(s)?;

    Ok((s, string))
}

fn edn_char(s: &[u8]) -> nom::IResult<&[u8], crate::Edn> {
    let (s, c) = preceded(
        tag("\\"),
        alt((
            map(preceded(char('u'), hex_u32), |c| {
                Edn::Character(unsafe { std::char::from_u32_unchecked(c) })
            }),
            map(tag("newline"), |_| Edn::Character('\n')),
            map(tag("return"), |_| Edn::Character('\r')),
            map(tag("space"), |_| Edn::Character(' ')),
            map(tag("tab"), |_| Edn::Character('\t')),
            map(anychar, |c| Edn::Character(c)),
        )),
    )(s)?;

    Ok((s, c))
}

fn edn_keyword(s: &[u8]) -> nom::IResult<&[u8], crate::Edn> {
    let (s, _) = tag(":")(s)?;

    let optional_namespace = opt(pair(take_while1(matches_identifier), tag("/")));
    let (s, namespace) = optional_namespace(s)?;
    let (s, sym) = take_while1(matches_identifier)(s)?;

    if let Some((ns, slash)) = namespace {
        Ok((
            s,
            Edn::Keyword(std::string::String::from_utf8(vec![ns, slash, sym].concat()).unwrap()),
        ))
    } else {
        Ok((
            s,
            Edn::Keyword(std::string::String::from_utf8(sym.to_vec()).unwrap()),
        ))
    }
}

fn edn_symbol(s: &[u8]) -> nom::IResult<&[u8], crate::Edn> {
    peek(not(tag(":")))(s)?;

    let optional_namespace = opt(pair(take_while1(matches_identifier), tag("/")));
    let (s, namespace) = optional_namespace(s)?;
    let (s, sym) = take_while1(matches_identifier)(s)?;

    if let Some((ns, slash)) = namespace {
        Ok((
            s,
            Edn::Symbol(std::string::String::from_utf8(vec![ns, slash, sym].concat()).unwrap()),
        ))
    } else {
        Ok((
            s,
            Edn::Symbol(std::string::String::from_utf8(sym.to_vec()).unwrap()),
        ))
    }
}

named!(
    edn_list<crate::Edn>,
    do_parse!(
        ws_or_comma!(tag!("("))
            >> elements: opt!(many0!(ws_or_comma!(edn_any)))
            >> ws_or_comma!(tag!(")"))
            >> (Edn::List(
                elements
                    .unwrap_or_else(Vec::new)
                    .into_iter()
                    .flatten()
                    .collect()
            ))
    )
);

// fn whitespace(s: &[u8]) -> IResult<&[u8], &[u8]> {
//     let (s, b) = alt((
//         tag(" "),
//         tag("\n"),
//         tag(","),
//         tag("\r\n"),
//     ))(s)?;
//
//     Ok((s, b))
// }
//
// fn edn_list(s: &[u8]) -> nom::IResult<&[u8], crate::Edn> {
//     let (s, _) = many0(space_or_comma)(s)?;
//     let (s, _) = tag("(")(s)?;
//     let (s, _) = many0(space_or_comma)(s)?;
//     let (s, elements) = opt(nom::multi::separated_list(
//         many0(space_or_comma),
//         edn_any,
//     ))(s)?;
//     let (s, _) = many0(space_or_comma)(s)?;
//     let (s, _) = tag(")")(s)?;
//     let (s, _) = many0(space_or_comma)(s)?;
//
//     Ok((
//         s,
//         Edn::List(
//             elements
//                 .unwrap_or_else(Vec::new)
//                 .into_iter()
//                 .flatten()
//                 .collect(),
//         ),
//     ))
// }

named!(
    edn_vector<crate::Edn>,
    do_parse!(
        ws_or_comma!(tag!("["))
            >> elements: many0!(ws_or_comma!(edn_any))
            >> ws_or_comma!(tag!("]"))
            >> (Edn::Vector(elements.into_iter().flatten().collect()))
    )
);

named!(
    edn_map<crate::Edn>,
    do_parse!(
        ws_or_comma!(tag!("{"))
            >> map: opt!(fold_many1!(
                pair!(ws_or_comma!(edn_any), ws_or_comma!(edn_any)),
                HashMap::new(),
                |mut acc: HashMap<_, _>, (k, v)| match (k, v) {
                    (Some(kk), Some(vv)) => {
                        acc.insert(kk, vv);
                        acc
                    }
                    _ => acc,
                }
            ))
            >> ws_or_comma!(tag!("}"))
            >> (Edn::Map(map.unwrap_or_else(HashMap::new)))
    )
);

fn edn_set(s: &[u8]) -> nom::IResult<&[u8], crate::Edn> {
    let (s, _) = tag("#{")(s)?;

    let (s, set) = opt(fold_many1(
        delimited(opt(space_or_comma), edn_any, opt(space_or_comma)),
        HashSet::new(),
        |mut acc: HashSet<_>, v| {
            if let Some(actual_v) = v {
                acc.insert(actual_v);
            }

            acc
        },
    ))(s)?;

    let (s, _) = tag("}")(s)?;

    Ok((s, Edn::Set(set.unwrap_or_else(HashSet::new))))
}

named!(
    edn_comment<crate::Edn>,
    do_parse!(
        n: preceded!(
            tag!(";"),
            value!(
                Edn::Comment,
                alt!(
                    complete!(alt!(
                        do_parse!(c: take_until!("\n") >> tag!("\n") >> (c))
                            | do_parse!(c: take_until!("\r\n") >> tag!("\r\n") >> (c))
                    )) | rest
                )
            )
        ) >> (n)
    )
);

// fn edn_comment(s: &[u8]) -> IResult<&[u8], crate::Edn> {
//     let (s, c) = preceded(
//         tag(";"),
//         nom::combinator::value(
//             Edn::Comment,
//             nom::sequence::terminated(not_line_ending, line_ending),
//         ),
//     )(s)?;
//
//     Ok((s, c))
// }

fn edn_any(s: &[u8]) -> IResult<&[u8], Option<crate::Edn>> {
    let (s, edn) = alt((
        edn_discard_sequence,
        map(edn_nil, |n| Some(n)),
        map(edn_list, |n| Some(n)),
        map(edn_map, |n| Some(n)),
        map(edn_vector, |n| Some(n)),
        map(edn_set, |n| Some(n)),
        map(edn_int, |n| Some(n)),
        map(edn_float, |n| Some(n)),
        map(edn_bool, |n| Some(n)),
        map(edn_keyword, |n| Some(n)),
        map(edn_string, |n| Some(n)),
        map(edn_symbol, |n| Some(n)),
        map(edn_char, |n| Some(n)),
        map(edn_comment, |_| None),
    ))(s)?;

    Ok((s, edn))
}

fn edn_all(s: &[u8]) -> IResult<&[u8], Vec<crate::Edn>> {
    let (s, edn) = many0(complete(edn_any))(s)?;
    let (s, _) = opt(line_ending)(s)?;
    Ok((s, edn.into_iter().flatten().collect()))
}

fn matches_identifier(c: u8) -> bool {
    is_alphanumeric(c) || c == b'-' || c == b'_' || c == b'.' || c == b'+' || c == b'&'
}

#[cfg(test)]
mod tests {
    use super::Edn::*;
    use super::*;
    use rust_decimal;
    use std::io::prelude::*;
    use std::str::FromStr;

    macro_rules! hashmap {
        () => {
            HashMap::new()
        };
        ( $($x:expr, $y:expr),* ) => {
            {
                let mut hm = HashMap::new();

                $(
                    hm.insert($x, $y);
                )*

                    hm
            }
        };
    }

    macro_rules! hashset {
        () => {
            HashSet::new()
        };
        ( $($x:expr),* ) => {
            {
                let mut hs = HashSet::new();

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
    fn parses_floats() {
        assert_eq!(
            edn_float("0.0".as_bytes()),
            Ok((vec!().as_slice(), Float(0.0)))
        );
        assert_eq!(
            edn_float("-0.0".as_bytes()),
            Ok((vec!().as_slice(), Float(-0.0)))
        );
        assert_eq!(
            edn_float("1.0".as_bytes()),
            Ok((vec!().as_slice(), Float(1.0)))
        );
        assert_eq!(
            edn_float("-1.0".as_bytes()),
            Ok((vec!().as_slice(), Float(-1.0)))
        );
        assert_eq!(
            edn_float("-1.2E5".as_bytes()),
            Ok((vec!().as_slice(), Float(-1.2E5)))
        );
        assert_eq!(
            edn_float("-120000".as_bytes()),
            Ok((vec!().as_slice(), Float(-1.2E5)))
        );
    }

    #[test]
    fn parses_decimals() {
        assert_eq!(
            edn_float("0.0M".as_bytes()),
            Ok((
                vec!().as_slice(),
                Decimal(rust_decimal::Decimal::from_str("0.0").unwrap())
            ))
        );

        assert_eq!(
            edn_float("1140141.1041040014014141M".as_bytes()),
            Ok((
                vec!().as_slice(),
                Decimal(rust_decimal::Decimal::from_str("1140141.1041040014014141").unwrap())
            ))
        );
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
    fn parses_with_unicode_literals() {
        let charstr1 = "\\u0065 ";
        let charstr2 = "\\u0177 ";
        let res1 = edn_char(charstr1.as_bytes());
        let res2 = edn_char(charstr2.as_bytes());
        assert_eq!(res1, Ok((vec!(32).as_slice(), Character('e'))));
        assert_eq!(res2, Ok((vec!(32).as_slice(), Character('ŷ'))));
    }

    #[test]
    fn parses_ascii_chars() {
        let charstr1 = b"\\a";
        let charstr2 = b"\\8";
        let res1 = edn_char(charstr1);
        let res2 = edn_char(charstr2);
        assert_eq!(res1, Ok((vec!().as_slice(), Character('a'))));
        assert_eq!(res2, Ok((vec!().as_slice(), Character('8'))));
    }

    #[test]
    fn parses_chars_with_special_sequence_literals() {
        let charstr_newline = "\\newline";
        let res1 = edn_char(charstr_newline.as_bytes());
        assert_eq!(res1, Ok((vec!().as_slice(), Character('\n'))));
        let charstr_return = "\\return";
        let res1 = edn_char(charstr_return.as_bytes());
        assert_eq!(res1, Ok((vec!().as_slice(), Character('\r'))));
        let charstr_space = "\\space";
        let res1 = edn_char(charstr_space.as_bytes());
        assert_eq!(res1, Ok((vec!().as_slice(), Character(' '))));
        let charstr_tab = "\\tab";
        let res1 = edn_char(charstr_tab.as_bytes());
        assert_eq!(res1, Ok((vec!().as_slice(), Character('\t'))));
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
    fn lists_must_have_whitespace() {
        let list_str = b"(\"hello\"abc)";
        let list_res = edn_vector(list_str);
        assert!(list_res.is_err());
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
    fn vectors_can_have_varargs() {
        let vector_str = b"[& args]";
        let vector_res = edn_vector(vector_str);
        assert_eq!(
            vector_res,
            Ok((
                vec![].as_bytes(),
                Vector(vec![Symbol("&".to_string()), Symbol("args".to_string())])
            ))
        );
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
    fn parses_sets_with_leading_whitespace() {
        let set_str = "#{,,,1 2 5}";
        let set_res = edn_set(set_str.as_bytes());
        assert_eq!(
            set_res,
            Ok((
                vec!().as_slice(),
                Set(hashset!(Integer(1), Integer(2), Integer(5)))
            ))
        );
    }

    #[test]
    fn parses_sets_with_trailing_whitespace() {
        let set_str = "#{1 2 5,, ,}";
        let set_res = edn_set(set_str.as_bytes());
        assert_eq!(
            set_res,
            Ok((
                vec!().as_slice(),
                Set(hashset!(Integer(1), Integer(2), Integer(5)))
            ))
        );
    }

    #[test]
    fn parses_sets_with_leading_and_trailing_whitespace() {
        let set_str = "#{ ,,      ,,   1 2 5,, ,}";
        let set_res = edn_set(set_str.as_bytes());
        assert_eq!(
            set_res,
            Ok((
                vec!().as_slice(),
                Set(hashset!(Integer(1), Integer(2), Integer(5)))
            ))
        );
    }

    #[test]
    fn parses_discard_sequence() {
        // only term is discarded results in
        // an empty, but valid result
        assert_eq!(edn_any(b"#_{:a :b :c :d}"), Ok((vec!().as_slice(), None)));

        assert_eq!(
            edn_any(b"[1 2 #_3 4]"),
            Ok((
                vec!().as_slice(),
                Some(Vector(vec!(Integer(1), Integer(2), Integer(4))))
            ))
        );

        // with weird nesting
        assert_eq!(
            edn_any(b"[1 2 #_[1 2 #_3] 4]"),
            Ok((
                vec!().as_slice(),
                Some(Vector(vec!(Integer(1), Integer(2), Integer(4))))
            ))
        );

        // with varied types
        assert_eq!(
            edn_map(b"{:a 1.01 #_:b #_38000 :c :d}"),
            Ok((
                vec!().as_slice(),
                Map(hashmap!(
                    Keyword("a".to_string()),
                    Float(1.01),
                    Keyword("c".to_string()),
                    Keyword("d".to_string())
                ))
            ))
        )
    }

    #[test]
    fn ignores_comments_in_various_positions() {
        // with trailing newline
        assert_eq!(
            edn_comment(b";; this is a comment and should not appear\n"),
            Ok((vec!().as_slice(), Comment))
        );

        // with trailing \r\n
        assert_eq!(
            edn_comment(b";; this is a comment and should not appear\r\n"),
            Ok((vec!().as_slice(), Comment))
        );

        // preceding
        assert_eq!(
            edn_all(b";; this is a comment and should not appear\n[1 2 3]"),
            Ok((
                vec!().as_slice(),
                vec![Vector(vec!(Integer(1), Integer(2), Integer(3)))]
            ))
        );

        // following
        assert_eq!(
            edn_all(b"[1 2 3];; this is a comment and should not appear"),
            Ok((
                vec!().as_slice(),
                vec![Vector(vec!(Integer(1), Integer(2), Integer(3)))]
            ))
        );

        // middle
        assert_eq!(
            edn_all(b"[1 2 3];; this is a comment and should not appear\n[4 5 6]"),
            Ok((
                vec!().as_slice(),
                vec![
                    Vector(vec!(Integer(1), Integer(2), Integer(3))),
                    Vector(vec!(Integer(4), Integer(5), Integer(6)))
                ]
            ))
        );

        // at EOF
        assert_eq!(
            edn_all(b";; this is a comment and should not appear"),
            Ok((vec!().as_slice(), vec![]))
        );
    }

    #[test]
    fn commas_are_whitespace() {
        // lists
        assert_eq!(
            edn_all(b"(,,1,, 2 ,, 3,,,,)"),
            Ok((
                vec![].as_bytes(),
                vec![List(vec![Integer(1), Integer(2), Integer(3)])]
            ))
        );

        // vectors
        assert_eq!(
            edn_all(b"[,,1,2,3     ,]"),
            Ok((
                vec![].as_bytes(),
                vec![Vector(vec![Integer(1), Integer(2), Integer(3)])]
            ))
        );

        // maps
        assert_eq!(
            edn_all(b",{,:a,1,,,,:b,2,}"),
            Ok((
                vec![].as_bytes(),
                vec![Map(hashmap![
                    Keyword("a".to_string()),
                    Integer(1),
                    Keyword("b".to_string()),
                    Integer(2)
                ])]
            ))
        );

        // set
        assert_eq!(
            edn_all(b"#{,,,, 1 , 2, 3 ,         }"),
            Ok((
                vec![].as_bytes(),
                vec![Set(hashset![Integer(1), Integer(2), Integer(3)])]
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
                Some(Map(hashmap!(
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
                )))
            ))
        )
    }

    #[test]
    fn equality() {
        // Nil,
        assert_eq!(Nil, Nil);
        assert_ne!(Nil, Keyword("Nil".to_string()));

        // Bool(bool),
        assert_eq!(Bool(true), Bool(true));
        assert_ne!(Bool(true), Bool(false));

        // String(&'a str),
        assert_eq!(String("a"), String("a"));
        assert_ne!(String("a"), String("z"));

        // Character(char),
        assert_eq!(Character('a'), Character('a'));
        assert_ne!(Character('a'), Character('b'));

        // Symbol(String),
        assert_eq!(Symbol("a".to_string()), Symbol("a".to_string()));
        assert_ne!(Symbol("a".to_string()), Symbol("z".to_string()));

        // Keyword(String),
        assert_eq!(Keyword("a".to_string()), Keyword("a".to_string()));
        assert_ne!(Keyword("a".to_string()), Keyword("z".to_string()));

        // Integer(isize),
        assert_eq!(Integer(1), Integer(1));
        assert_ne!(Integer(1), Integer(2));

        // Float(f64),
        assert_eq!(Float(32.0), Float(32.0));
        assert_ne!(Float(32.0), Float(84.0));

        // Decimal(rust_decimal::Decimal),
        assert_eq!(
            rust_decimal::Decimal::from_str("32.0").unwrap(),
            rust_decimal::Decimal::from_str("32.0").unwrap()
        );
        assert_ne!(
            rust_decimal::Decimal::from_str("32.0").unwrap(),
            rust_decimal::Decimal::from_str("19.9999999999").unwrap()
        );

        // List(Vec<Edn<'a>>),
        assert_eq!(List(vec![]), List(vec![]));
        assert_eq!(List(vec![Integer(1)]), List(vec![Integer(1)]));
        assert_ne!(List(vec![Integer(1)]), List(vec![Float(1.0444444)]));

        // Vector(Vec<Edn<'a>>),
        assert_eq!(Vector(vec![]), Vector(vec![]));
        assert_eq!(Vector(vec![Integer(1)]), Vector(vec![Integer(1)]));
        assert_ne!(Vector(vec![Integer(1)]), List(vec![Integer(1)]));

        // Map(HashMap<Edn<'a>, Edn<'a>>),
        assert_eq!(
            Map(hashmap!(Keyword("a".to_string()), Integer(1))),
            Map(hashmap!(Keyword("a".to_string()), Integer(1)))
        );
        assert_ne!(
            Map(hashmap!(Keyword("a".to_string()), Float(2.1))),
            Map(hashmap!(Keyword("a".to_string()), Float(1.2)))
        );

        // Set(HashSet<Edn<'a>>),
        assert_eq!(Set(hashset![Integer(1)]), Set(hashset![Integer(1)]));
        assert_ne!(Set(hashset![Integer(1)]), Set(hashset![Integer(91391)]));
        assert_ne!(Set(hashset![Integer(1)]), List(vec![]));
    }

    #[test]
    fn parses_ascii_stl_src() {
        let mut edn = std::fs::File::open("./fixtures/ascii_stl.clj").unwrap();
        let mut buf = Vec::new();
        edn.read_to_end(&mut buf).unwrap();
        let start = std::time::Instant::now();
        let embedded_res = edn_all(&mut buf);
        let end = std::time::Instant::now();

        println!("ascii_stl_src time: {:?}", end - start);

        let (remaining, result) = embedded_res.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(result.len(), 6);
    }
}
