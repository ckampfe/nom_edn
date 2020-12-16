use nom::branch::alt;
use nom::bytes::complete::{escaped, is_a, tag};
use nom::character::complete::{anychar, none_of, one_of};
use nom::character::is_alphanumeric;
use nom::combinator::{complete, map, not, opt, peek, recognize, rest, value};
use nom::multi::{fold_many1, many0};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::{FindToken, Finish, IResult, InputTakeAtPosition};
use std::str::FromStr;

#[cfg(all(feature = "im", feature = "im-rc"))]
compile_error!("features `im` and `im-rc` are mutually exclusive");
#[cfg(feature = "im")]
use im::{HashMap, HashSet};
#[cfg(feature = "im-rc")]
use im_rc::{HashMap, HashSet};

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Edn {
    Nil,
    Bool(bool),
    String(String),
    Character(char),
    Symbol(String),
    Keyword(String),
    Integer(isize),
    Float(ordered_float::OrderedFloat<f64>),
    Decimal(rust_decimal::Decimal),
    List(Vec<Edn>),
    Vector(Vec<Edn>),
    Map(HashMap<Edn, Edn>),
    Set(HashSet<Edn>),
    // Right now `Comment` is a marker value that follows the edn spec
    // by ignoring any subsequent data, but in the future we could
    // make a variant of it that captures the comment data itself.
    // There could be circumstances where one would want to
    // capture comment data.
    Comment,
    // TODO: handle tagged elements
}

#[macro_export]
macro_rules! edn {
    ($b:expr) => {
        parse_edn_one($b).unwrap()
    };
}

#[macro_export]
macro_rules! edn_many {
    ($b:expr) => {
        parse_edn_many($b).unwrap()
    };
}

pub fn parse_edn_one<'a>(input: &'a str) -> Result<Edn, nom::error::VerboseError<&'a str>> {
    if input.is_empty() {
        return Ok(Edn::Nil);
    }

    let (_s, o) = edn_any(input).finish()?;
    match o {
        Some(edn) => Ok(edn),
        None => Ok(Edn::Nil),
    }
}

pub fn parse_edn_many<'a>(input: &'a str) -> Result<Vec<Edn>, nom::error::VerboseError<&'a str>> {
    let (s, _) = opt(space_or_comma)(input).finish()?;

    let (_s, edn) = many0(delimited(
        opt(many0(nom::character::complete::line_ending)),
        complete(edn_any),
        opt(many0(nom::character::complete::line_ending)),
    ))(s)
    .finish()?;

    Ok(edn.into_iter().flatten().collect())
}

fn space_or_comma(s: &str) -> IResult<&str, &str, nom::error::VerboseError<&str>> {
    s.split_at_position(|c| !" \t\r\n,".find_token(c))
}

fn edn_discard_sequence(s: &str) -> IResult<&str, Option<Edn>, nom::error::VerboseError<&str>> {
    let (s, _) = preceded(nom::bytes::complete::tag("#_"), recognize(edn_any))(s)?;

    Ok((s, None))
}

fn edn_nil(s: &str) -> IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = tag("nil")(s)?;
    Ok((s, Edn::Nil))
}

fn edn_bool(s: &str) -> IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, v) = map(alt((tag("true"), tag("false"))), |value| match value {
        _ if value == "true" => Edn::Bool(true),
        _ if value == "false" => Edn::Bool(false),
        _ => panic!("nonbool matched, definitely an error."),
    })(s)?;

    Ok((s, v))
}

fn edn_int(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, i) = map(
        pair(
            opt(alt((
                nom::bytes::complete::tag("+"),
                nom::bytes::complete::tag("-"),
            ))),
            nom::character::complete::digit1,
        ),
        |(sign, digits): (Option<&str>, &str)| {
            let i = if let Some(s) = sign {
                let nstr = digits;
                let n = nstr.parse::<isize>().unwrap();
                if s == "-" {
                    -n
                } else {
                    n
                }
            } else {
                let nstr = digits;
                nstr.parse::<isize>().unwrap()
            };

            Edn::Integer(i)
        },
    )(s)?;

    let (s, _) = not(nom::bytes::complete::tag("."))(s)?;

    Ok((s, i))
}

fn edn_float(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, f) = alt((
        map(
            pair(
                recognize(nom::number::complete::double),
                nom::bytes::complete::tag("M"),
            ),
            |(d, _): (&str, &str)| Edn::Decimal(rust_decimal::Decimal::from_str(d).unwrap()),
        ),
        map(nom::number::complete::double, |d| Edn::Float(d.into())),
    ))(s)?;

    Ok((s, f))
}

fn edn_string(s: &str) -> IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = tag("\"")(s)?;
    let (s, string) = map(
        escaped(none_of("\"\\"), '\\', one_of("\"ntr\\")),
        |s: &str| Edn::String(s.to_string()),
    )(s)?;
    let (s, _) = tag("\"")(s)?;

    Ok((s, string))
}

fn edn_char(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, c) = preceded(
        tag("\\"),
        alt((
            map(preceded(tag("u"), hex_u32), |c| {
                Edn::Character(unsafe { std::char::from_u32_unchecked(c) })
            }),
            map(tag("newline"), |_| Edn::Character('\n')),
            map(tag("return"), |_| Edn::Character('\r')),
            map(tag("space"), |_| Edn::Character(' ')),
            map(tag("tab"), |_| Edn::Character('\t')),
            map(anychar, Edn::Character),
        )),
    )(s)?;

    Ok((s, c))
}

fn edn_keyword(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = tag(":")(s)?;

    let mut optional_namespace = opt(pair(
        nom::bytes::complete::take_while1(matches_identifier),
        tag("/"),
    ));
    let (s, namespace) = optional_namespace(s)?;
    let (s, sym) = nom::bytes::complete::take_while1(matches_identifier)(s)?;

    if let Some((ns, slash)) = namespace {
        Ok((s, Edn::Keyword(vec![ns, slash, sym].concat())))
    } else {
        Ok((s, Edn::Keyword(sym.to_string())))
    }
}

fn edn_symbol(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    peek(not(tag(":")))(s)?;

    let mut optional_namespace = opt(pair(
        nom::bytes::complete::take_while1(matches_identifier),
        nom::bytes::complete::tag("/"),
    ));
    let (s, namespace) = optional_namespace(s)?;
    // let (s, sym) = nom::bytes::complete::take_while1(matches_identifier)(s)?;
    let (s, sym) = nom::multi::many1(nom::character::complete::satisfy(matches_identifier))(s)?;
    let sym: String = sym.iter().collect();

    if let Some((ns, slash)) = namespace {
        Ok((s, Edn::Symbol(vec![ns, slash, &sym].concat())))
    } else {
        Ok((s, Edn::Symbol(sym)))
    }
}

fn edn_list(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = nom::bytes::complete::tag("(")(s)?;

    let (s, elements) = many0(delimited(opt(space_or_comma), edn_any, opt(space_or_comma)))(s)?;

    let (s, _) = nom::bytes::complete::tag(")")(s)?;

    Ok((s, Edn::List(elements.into_iter().flatten().collect())))
}

fn edn_vector(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = nom::bytes::complete::tag("[")(s)?;

    let (s, elements) = many0(delimited(opt(space_or_comma), edn_any, opt(space_or_comma)))(s)?;

    let (s, _) = nom::bytes::complete::tag("]")(s)?;

    Ok((s, Edn::Vector(elements.into_iter().flatten().collect())))
}

fn edn_map(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = nom::bytes::complete::tag("{")(s)?;

    let (s, map) = opt(fold_many1(
        pair(
            delimited(
                opt(space_or_comma),
                nom::combinator::complete(edn_any),
                opt(space_or_comma),
            ),
            delimited(
                opt(space_or_comma),
                nom::combinator::complete(edn_any),
                opt(space_or_comma),
            ),
        ),
        HashMap::new(),
        |mut acc: HashMap<_, _>, (k, v)| match (k, v) {
            (Some(kk), Some(vv)) => {
                acc.insert(kk, vv);
                acc
            }
            _ => acc,
        },
    ))(s)?;

    let (s, _) = nom::bytes::complete::tag("}")(s)?;

    Ok((s, Edn::Map(map.unwrap_or_else(HashMap::new))))
}

fn edn_set(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = nom::bytes::complete::tag("#{")(s)?;

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

    let (s, _) = nom::bytes::complete::tag("}")(s)?;

    Ok((s, Edn::Set(set.unwrap_or_else(HashSet::new))))
}

fn edn_comment(s: &str) -> IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, c) = preceded(
        nom::bytes::complete::tag(";"),
        value(
            Edn::Comment,
            alt((
                alt((
                    terminated(
                        nom::bytes::complete::take_until("\n"),
                        nom::bytes::complete::tag("\n"),
                    ),
                    terminated(
                        nom::bytes::complete::take_until("\r\n"),
                        nom::bytes::complete::tag("\r\n"),
                    ),
                )),
                rest,
            )),
        ),
    )(s)?;

    Ok((s, c))
}

fn edn_any(s: &str) -> IResult<&str, Option<crate::Edn>, nom::error::VerboseError<&str>> {
    let (s, edn) = alt((
        edn_discard_sequence,
        map(edn_nil, Some),
        map(edn_list, Some),
        map(edn_map, Some),
        map(edn_vector, Some),
        map(edn_set, Some),
        map(edn_int, Some),
        map(edn_float, Some),
        map(edn_bool, Some),
        map(edn_keyword, Some),
        map(edn_string, Some),
        map(edn_symbol, Some),
        map(edn_char, Some),
        map(edn_comment, |_| None),
    ))(s)?;

    Ok((s, edn))
}

fn matches_identifier(c: char) -> bool {
    is_alphanumeric(c as u8) || c == '-' || c == '_' || c == '.' || c == '+' || c == '&'
}

// fork of https://docs.rs/nom/6.0.1/src/nom/number/complete.rs.html#1341-1361
fn hex_u32<'a, E: nom::error::ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, u32, E> {
    let (i, o) = is_a(&b"0123456789abcdefABCDEF"[..])(input)?;
    // Do not parse more than 8 characters for a u32
    let (parsed, remaining) = if o.len() <= 8 {
        (o, i)
    } else {
        (&input[..8], &input[8..])
    };

    let res = parsed
        .chars()
        .rev()
        .enumerate()
        .map(|(k, v)| {
            let digit = v as char;
            digit.to_digit(16).unwrap_or(0) << (k * 4)
        })
        .sum();

    Ok((remaining, res))
}

#[cfg(test)]
mod tests {
    use super::Edn::*;
    use super::*;
    use rust_decimal;
    use std::io::Read;
    use std::str::FromStr;

    const DEPS_DOT_EDN: &str = include_str!("../fixtures/deps.edn");
    const ASCII_STL: &str = include_str!("../fixtures/ascii_stl.clj");

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
    fn nil() {
        let nil = "nil";
        let res = edn_nil(nil);
        assert_eq!(res, Ok(("", Edn::Nil)));
    }

    #[test]
    fn bool() {
        let truestr = "true";
        let falsestr = "false";
        let trueres = edn_bool(truestr);
        let falseres = edn_bool(falsestr);
        assert_eq!(trueres, Ok(("", Bool(true))));
        assert_eq!(falseres, Ok(("", Bool(false))));
    }

    #[test]
    fn keyword() {
        let keystr = ":a-kw";
        let res = nom::combinator::complete(edn_keyword)(keystr);
        assert_eq!(res, Ok(("", Keyword("a-kw".to_string()))));
    }

    #[test]
    fn keyword_namespaced() {
        let keystr = ":org.clojure/clojure";
        let res = nom::combinator::complete(edn_keyword)(keystr);
        assert_eq!(res, Ok(("", Keyword("org.clojure/clojure".to_string()))));
    }

    #[test]
    fn int() {
        let intstr = "1";
        let res = edn_int(intstr);
        assert_eq!(res, Ok(("", Integer(1))));
    }

    #[test]
    fn float() {
        let negative_0 = -0.0f64;
        let negative_1 = -1.0f64;
        let negative_120_000 = -120_000.0;
        assert_eq!(edn_float("0.0"), Ok(("", Float(0.0.into()))));
        assert_eq!(edn_float("-0.0"), Ok(("", Float(negative_0.into()))));
        assert_eq!(edn_float("1.0"), Ok(("", Float(1.0.into()))));
        assert_eq!(edn_float("-1.0"), Ok(("", Float(negative_1.into()))));
        assert_eq!(
            edn_float("-1.2E5"),
            Ok(("", Float(negative_120_000.into())))
        );
        assert_eq!(
            edn_float("-120000"),
            Ok(("", Float(negative_120_000.into())))
        );
    }

    #[test]
    fn decimal() {
        assert_eq!(
            edn_float("0.0M"),
            Ok(("", Decimal(rust_decimal::Decimal::from_str("0.0").unwrap())))
        );

        assert_eq!(
            edn_float("1140141.1041040014014141M"),
            Ok((
                "",
                Decimal(rust_decimal::Decimal::from_str("1140141.1041040014014141").unwrap())
            ))
        );
    }

    #[test]
    fn symbol() {
        let symstr = "a-sym";
        let res = edn_symbol(symstr);
        assert_eq!(res, Ok(("", Symbol("a-sym".to_string()))));
    }

    #[test]
    fn symbol_namedspaced() {
        let symstr = "org.clojure/clojure";
        let res = edn_symbol(symstr);
        assert_eq!(res, Ok(("", Symbol("org.clojure/clojure".to_string()))));
    }

    #[test]
    fn string() {
        let strstr = "\"hello\"";
        let res = edn_string(strstr);
        assert_eq!(res, Ok(("", String("hello".to_string()))));
    }

    #[test]
    fn string_escapes() {
        let mut embedded_str = std::fs::File::open("./fixtures/embedded_str").unwrap();
        // let embedded_str = "\"hel\"lo\"";
        let mut buf = std::string::String::new();
        embedded_str.read_to_string(&mut buf).unwrap();
        let embedded_res = edn_string(&mut buf);
        assert_eq!(embedded_res, Ok(("", String("hel\\\"lo".to_string()))));
    }

    #[test]
    fn char_unicode_literals() {
        let charstr1 = "\\u0065";
        let charstr2 = "\\u0177";
        let res1 = edn_char(charstr1);
        let res2 = edn_char(charstr2);
        assert_eq!(res1, Ok(("", Character('e'))));
        assert_eq!(res2, Ok(("", Character('ŷ'))));
    }

    #[test]
    fn char_ascii() {
        let charstr1 = "\\a";
        let charstr2 = "\\8";
        let res1 = edn_char(charstr1);
        let res2 = edn_char(charstr2);
        assert_eq!(res1, Ok(("", Character('a'))));
        assert_eq!(res2, Ok(("", Character('8'))));
    }

    #[test]
    fn char_special_sequence_literals() {
        let charstr_newline = "\\newline";
        let res1 = edn_char(charstr_newline);
        assert_eq!(res1, Ok(("", Character('\n'))));
        let charstr_return = "\\return";
        let res1 = edn_char(charstr_return);
        assert_eq!(res1, Ok(("", Character('\r'))));
        let charstr_space = "\\space";
        let res1 = edn_char(charstr_space);
        assert_eq!(res1, Ok(("", Character(' '))));
        let charstr_tab = "\\tab";
        let res1 = edn_char(charstr_tab);
        assert_eq!(res1, Ok(("", Character('\t'))));
    }

    #[test]
    fn list_homogenous() {
        let list_str = "(:a :b :c)";
        let list_res = edn_list(list_str);
        assert_eq!(
            list_res,
            Ok((
                "",
                List(vec!(
                    Keyword("a".to_string()),
                    Keyword("b".to_string()),
                    Keyword("c".to_string())
                ))
            ))
        );
    }

    #[test]
    fn list_heterogenous() {
        let list_str = "(:a b true false some-sym :c)";
        let list_res = edn_list(list_str);
        assert_eq!(
            list_res,
            Ok((
                "",
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
    fn list_heterogenous_nested() {
        let list_str = "(:a b (1 2 5 :e) true false [[] 232 ()] some-sym :c)";
        let list_res = edn_list(list_str);
        assert_eq!(
            list_res,
            Ok((
                "",
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
    fn list_whitespace() {
        let list_str = "(\"hello\"abc)";
        let list_res = edn_vector(list_str);
        assert!(list_res.is_err());
    }

    #[test]
    fn vector_homogenous() {
        let vector_str = "[:a :b :c]";
        let vector_res = edn_vector(vector_str);
        assert_eq!(
            vector_res,
            Ok((
                "",
                Vector(vec!(
                    Keyword("a".to_string()),
                    Keyword("b".to_string()),
                    Keyword("c".to_string())
                ))
            ))
        );
    }

    #[test]
    fn vector_heterogenous() {
        let vector_str = "[:a b 1 true false some-sym 44444 :c]";
        let vector_res = edn_vector(vector_str);
        assert_eq!(
            vector_res,
            Ok((
                "",
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
    fn vector_empty() {
        let vector_str = "[]";
        let vector_res = edn_vector(vector_str);
        assert_eq!(vector_res, Ok(("", Vector(vec!()))));
    }

    #[test]
    fn vector_varargs() {
        let vector_str = "[& args]";
        let vector_res = edn_vector(vector_str);
        assert_eq!(
            vector_res,
            Ok((
                "",
                Vector(vec![Symbol("&".to_string()), Symbol("args".to_string())])
            ))
        );
    }

    #[test]
    fn vector_heterogenous_nested() {
        let vector_str = "[:a b true false [[] 232 ()] some-sym :c]";
        let vector_res = edn_vector(vector_str);
        assert_eq!(
            vector_res,
            Ok((
                "",
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
    fn map() {
        let map_str = "{:a 1}";
        let map_res = edn_map(map_str);
        assert_eq!(
            map_res,
            Ok(("", Map(hashmap!(Keyword("a".to_string()), Integer(1)))))
        );
    }

    #[test]
    fn map_empty() {
        let map_str = "{}";
        let map_res = edn_map(map_str);
        assert_eq!(map_res, Ok(("", Map(hashmap!()))));
    }

    #[test]
    fn map_nested_values() {
        let map_str = "{:a [1 2 4.01]}";
        let map_res = edn_map(map_str);
        assert_eq!(
            map_res,
            Ok((
                "",
                Map(hashmap!(
                    Keyword("a".to_string()),
                    Vector(vec!(Integer(1), Integer(2), Float(4.01.into())))
                ))
            ))
        );
    }

    #[test]
    fn map_nested_keys() {
        let map_str = "{[1 2 3] :a\n {} :bcd {} :zzzzzzzz}";
        let (map_remaining, map_res) = edn_map(map_str).unwrap();

        // :bcd should be gone, as :zzzzzzzz overwrites
        assert!(match &map_res {
            Map(m) => !m
                .values()
                .collect::<HashSet<&Edn>>()
                .contains(&Symbol("bcd".into())),
            _ => panic!(),
        });

        assert_eq!(
            (map_remaining, map_res),
            (
                "",
                Map(hashmap!(
                    Vector(vec!(Integer(1), Integer(2), Integer(3))),
                    Keyword("a".to_string()),
                    Map(hashmap!()),
                    Keyword("zzzzzzzz".to_string())
                ))
            )
        );
    }

    #[test]
    fn set() {
        let set_str = "#{:a 1}";
        let set_res = edn_set(set_str);
        assert_eq!(
            set_res,
            Ok(("", Set(hashset!(Keyword("a".to_string()), Integer(1)))))
        );
    }

    #[test]
    fn set_empty() {
        let set_str = "#{}";
        let set_res = edn_set(set_str);
        assert_eq!(set_res, Ok(("", Set(hashset!()))));
    }

    #[test]
    fn set_nested_values() {
        let set_str = "#{:a [1 2 3]}";
        let set_res = edn_set(set_str);
        assert_eq!(
            set_res,
            Ok((
                "",
                Set(hashset!(
                    Keyword("a".to_string()),
                    Vector(vec!(Integer(1), Integer(2), Integer(3)))
                ))
            ))
        );
    }

    #[test]
    fn set_nested_keys() {
        let set_str = "#{[1 2 3] :a\n {} :bcd {} #{} [] (1 2 #{}) :zzzzzzzz}"; // only one nested empty map
        let set_res = edn_set(set_str);
        assert_eq!(
            set_res,
            Ok((
                "",
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
    fn set_leading_whitespace() {
        let set_str = "#{,,,1 2 5}";
        let set_res = edn_set(set_str);
        assert_eq!(
            set_res,
            Ok(("", Set(hashset!(Integer(1), Integer(2), Integer(5)))))
        );
    }

    #[test]
    fn set_trailing_whitespace() {
        let set_str = "#{1 2 5,, ,}";
        let set_res = edn_set(set_str);
        assert_eq!(
            set_res,
            Ok(("", Set(hashset!(Integer(1), Integer(2), Integer(5)))))
        );
    }

    #[test]
    fn set_leading_and_trailing_whitespace() {
        let set_str = "#{ ,,      ,,   1 2 5,, ,}";
        let set_res = edn_set(set_str);
        assert_eq!(
            set_res,
            Ok(("", Set(hashset!(Integer(1), Integer(2), Integer(5)))))
        );
    }

    #[test]
    fn discard_sequence() {
        // only term is discarded results in
        // an empty, but valid result
        assert_eq!(edn_any("#_{:a :b :c :d}"), Ok(("", None)));

        assert_eq!(
            edn_any("[1 2 #_3 4]"),
            Ok(("", Some(Vector(vec!(Integer(1), Integer(2), Integer(4))))))
        );

        // with weird nesting
        assert_eq!(
            edn_any("[1 2 #_[1 2 #_3] 4]"),
            Ok(("", Some(Vector(vec!(Integer(1), Integer(2), Integer(4))))))
        );

        // with varied types
        assert_eq!(
            edn_map("{:a 1.01 #_:b #_38000 :c :d}"),
            Ok((
                "",
                Map(hashmap!(
                    Keyword("a".to_string()),
                    Float(1.01.into()),
                    Keyword("c".to_string()),
                    Keyword("d".to_string())
                ))
            ))
        )
    }

    #[test]
    fn comments() {
        // with trailing newline
        assert_eq!(
            edn_comment(";; this is a comment and should not appear\n"),
            Ok(("", Comment))
        );

        // with trailing \r\n
        assert_eq!(
            edn_comment(";; this is a comment and should not appear\r\n"),
            Ok(("", Comment))
        );

        // preceding
        assert_eq!(
            edn_many!(";; this is a comment and should not appear\n[,,, 1,, 2 3    ,,]"),
            vec![Vector(vec!(Integer(1), Integer(2), Integer(3)))]
        );

        // following
        assert_eq!(
            edn_many!("[  1, 2, 3, ,,,];; this is a comment and should not appear"),
            vec![Vector(vec!(Integer(1), Integer(2), Integer(3)))]
        );

        // middle
        assert_eq!(
            edn_many!("[1 2 3];; this is a comment and should not appear\n[4 5 6]"),
            vec![
                Vector(vec!(Integer(1), Integer(2), Integer(3))),
                Vector(vec!(Integer(4), Integer(5), Integer(6)))
            ]
        );

        // at EOF
        assert_eq!(
            edn_many!(";; this is a comment and should not appear"),
            vec![]
        );
    }

    #[test]
    fn whitespace_commas() {
        // lists
        assert_eq!(
            edn_many!("(,,1,, 2 ,, 3,,,,)"),
            vec![List(vec![Integer(1), Integer(2), Integer(3)])]
        );

        // vectors
        assert_eq!(
            edn_many!("[,,1,2,3     ,]"),
            vec![Vector(vec![Integer(1), Integer(2), Integer(3)])]
        );

        // maps
        assert_eq!(
            edn_many!(",{,:a,1,,,,:b,2,}"),
            vec![Map(hashmap![
                Keyword("a".to_string()),
                Integer(1),
                Keyword("b".to_string()),
                Integer(2)
            ])]
        );

        // set
        assert_eq!(
            edn_many!("#{,,,, 1 , 2, 3 ,         }"),
            vec![Set(hashset![Integer(1), Integer(2), Integer(3)])]
        );
    }

    #[test]
    fn real_edn() {
        let start = std::time::Instant::now();
        let embedded_res = edn!(DEPS_DOT_EDN);
        let end = std::time::Instant::now();

        println!("{:?}", end - start);

        assert_eq!(
            embedded_res,
            Map(hashmap!(
                Keyword("paths".to_string()),
                Vector(vec!(
                    String("resources".to_string()),
                    String("src".to_string())
                )),
                Keyword("deps".to_string()),
                Map(hashmap!(
                    Symbol("org.clojure/clojure".to_string()),
                    Map(hashmap!(
                        Keyword("mvn/version".to_string()),
                        String("1.10.0".to_string())
                    )),
                    Symbol("instaparse".to_string()),
                    Map(hashmap!(
                        Keyword("mvn/version".to_string()),
                        String("1.4.9".to_string())
                    )),
                    Symbol("quil".to_string()),
                    Map(hashmap!(
                        Keyword("mvn/version".to_string()),
                        String("2.8.0".to_string()),
                        Keyword("exclusions".to_string()),
                        Vector(vec!(Symbol("com.lowagie/itext".to_string())))
                    ),),
                    Symbol("com.hypirion/clj-xchart".to_string()),
                    Map(hashmap!(
                        Keyword("mvn/version".to_string()),
                        String("0.2.0".to_string())
                    )),
                    Symbol("net.mikera/core.matrix".to_string()),
                    Map(hashmap!(
                        Keyword("mvn/version".to_string()),
                        String("0.62.0".to_string())
                    )),
                    Symbol("net.mikera/vectorz-clj".to_string()),
                    Map(hashmap!(
                        Keyword("mvn/version".to_string()),
                        String("0.48.0".to_string())
                    ))
                )),
                Keyword("aliases".to_string()),
                Map(hashmap!(
                    Keyword("more-mem".to_string()),
                    Map(hashmap!(
                        Keyword("jvm-opts".to_string()),
                        Vector(vec!(String("-Xmx12G -Xms12G".to_string())))
                    ),),
                    Keyword("test".to_string()),
                    Map(hashmap!(
                        Keyword("extra-paths".to_string()),
                        Vector(vec!(String("test".to_string()))),
                        Keyword("extra-deps".to_string()),
                        Map(hashmap!(
                            Symbol("org.clojure/test.check".to_string()),
                            Map(hashmap!(
                                Keyword("mvn/version".to_string()),
                                String("RELEASE".to_string())
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
                                String("https://github.com/cognitect-labs/test-runner".to_string()),
                                Keyword("sha".to_string()),
                                String("76568540e7f40268ad2b646110f237a60295fa3c".to_string())
                            ),)
                        )),
                        Keyword("main-opts".to_string()),
                        Vector(vec!(
                            String("-m".to_string()),
                            String("cognitect.test-runner".to_string()),
                            String("-d".to_string()),
                            String("test".to_string())
                        ),)
                    ))
                ),)
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
        assert_eq!(String("a".to_string()), String("a".to_string()));
        assert_ne!(String("a".to_string()), String("z".to_string()));

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
        assert_eq!(Float(32.0.into()), Float(32.0.into()));
        assert_ne!(Float(32.0.into()), Float(84.0.into()));

        // Decimal(rust_decimal::Decimal),
        assert_eq!(
            Decimal(rust_decimal::Decimal::from_str("32.0").unwrap()),
            Decimal(rust_decimal::Decimal::from_str("32.0").unwrap())
        );
        assert_ne!(
            Decimal(rust_decimal::Decimal::from_str("32.0").unwrap()),
            Decimal(rust_decimal::Decimal::from_str("19.9999999999").unwrap())
        );
        assert_ne!(
            Decimal(rust_decimal::Decimal::from_str("32.0").unwrap()),
            Float(32.0.into())
        );

        // List(Vec<Edn<'a>>),
        assert_eq!(List(vec![]), List(vec![]));
        assert_eq!(List(vec![Integer(1)]), List(vec![Integer(1)]));
        assert_ne!(List(vec![Integer(1)]), List(vec![Float(1.0444444.into())]));

        // Vector(Vec<Edn<'a>>),
        assert_eq!(Vector(vec![]), Vector(vec![]));
        assert_eq!(Vector(vec![Integer(1)]), Vector(vec![Integer(1)]));
        assert_ne!(Vector(vec![Integer(1)]), List(vec![Integer(1)]));

        // Map(HashMap<Edn<'a>, Edn<'a>>),
        assert_eq!(Map(hashmap!()), Map(hashmap!()));
        assert_eq!(
            Map(hashmap!(Keyword("a".to_string()), Integer(1))),
            Map(hashmap!(Keyword("a".to_string()), Integer(1)))
        );
        assert_eq!(
            Map(hashmap!(
                Keyword("a".to_string()),
                Integer(1),
                Keyword("b".to_string()),
                Integer(2)
            )),
            Map(hashmap!(
                Keyword("b".to_string()),
                Integer(2),
                Keyword("a".to_string()),
                Integer(1)
            ))
        );
        assert_ne!(
            Map(hashmap!(Keyword("a".to_string()), Float(2.1.into()))),
            Map(hashmap!(Keyword("a".to_string()), Float(1.2.into())))
        );

        // Set(HashSet<Edn<'a>>),
        assert_eq!(Set(hashset![Integer(1)]), Set(hashset![Integer(1)]));
        assert_ne!(Set(hashset![Integer(1)]), Set(hashset![Integer(91391)]));
        assert_ne!(Set(hashset![Integer(1)]), List(vec![]));
    }

    #[test]
    fn parses_ascii_stl_src() {
        let start = std::time::Instant::now();
        let result = edn_many!(ASCII_STL);
        let end = std::time::Instant::now();

        println!("ascii_stl_src time: {:?}", end - start);

        assert_eq!(result.len(), 6);
    }

    #[test]
    fn empty_input() {
        assert_eq!(Edn::Nil, edn!(""))
    }
}
