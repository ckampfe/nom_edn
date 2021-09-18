use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take_until, take_while1};
use nom::character::complete::{
    anychar, digit1, line_ending, none_of, not_line_ending, one_of, satisfy,
};
use nom::character::{is_alphabetic, is_alphanumeric};
use nom::combinator::{complete, map, not, opt, peek, recognize, rest, value};
use nom::error::{FromExternalError, VerboseError};
use nom::multi::{fold_many1, many0};
use nom::number::complete::double;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::{FindToken, Finish, IResult, InputTakeAtPosition};
use std::str::FromStr;

#[cfg(all(feature = "im", feature = "im-rc"))]
compile_error!("features `im` and `im-rc` are mutually exclusive");
#[cfg(feature = "im")]
use im::{HashMap as Map, HashSet as Set};
#[cfg(feature = "im-rc")]
use im_rc::{HashMap as Map, HashSet as Set};
#[cfg(all(not(feature = "im"), not(feature = "im-rc")))]
use std::collections::{BTreeMap as Map, BTreeSet as Set};

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Edn<'edn> {
    Nil,
    Bool(bool),
    String(&'edn str),
    Character(char),
    Symbol {
        namespace: Option<&'edn str>,
        name: &'edn str,
    },
    Keyword {
        namespace: Option<&'edn str>,
        name: &'edn str,
    },
    Integer(isize),
    Float(ordered_float::OrderedFloat<f64>),
    Decimal(rust_decimal::Decimal),
    List(Vec<Edn<'edn>>),
    Vector(Vec<Edn<'edn>>),
    Map(Map<Edn<'edn>, Edn<'edn>>),
    Set(Set<Edn<'edn>>),
    // Right now `Comment` is a marker value that follows the edn spec
    // by ignoring any subsequent data, but in the future we could
    // make a variant of it that captures the comment data itself.
    // There could be circumstances where one would want to
    // capture comment data.
    Comment,
    Tag(Tag<'edn>),
    // TODO: handle tagged elements
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Tag<'edn> {
    Inst(chrono::DateTime<chrono::offset::FixedOffset>),
    UUID(uuid::Uuid),
    UserDefined {
        prefix: &'edn str,
        name: &'edn str,
        element: Box<Edn<'edn>>,
    },
}

pub struct InvalidTagElementError;

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

pub fn parse_edn_one(input: &str) -> Result<Edn, nom::error::VerboseError<&str>> {
    if input.is_empty() {
        return Ok(Edn::Nil);
    }

    let (_s, o) = edn_any(input).finish()?;
    match o {
        Some(edn) => Ok(edn),
        None => Ok(Edn::Nil),
    }
}

pub fn parse_edn_many(input: &str) -> Result<Vec<Edn>, nom::error::VerboseError<&str>> {
    let (s, _) = opt(space_or_comma)(input).finish()?;

    let (_s, edn) = many0(delimited(
        opt(many0(line_ending)),
        complete(edn_any),
        opt(many0(line_ending)),
    ))(s)
    .finish()?;

    Ok(edn.into_iter().flatten().collect())
}

fn space_or_comma(s: &str) -> IResult<&str, &str, nom::error::VerboseError<&str>> {
    s.split_at_position(|c| !" \t\r\n,".find_token(c))
}

fn edn_discard_sequence(s: &str) -> IResult<&str, Option<Edn>, nom::error::VerboseError<&str>> {
    let (s, _) = preceded(tag("#_"), recognize(edn_any))(s)?;

    Ok((s, None))
}

fn edn_nil(s: &str) -> IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = tag("nil")(s)?;
    Ok((s, Edn::Nil))
}

fn edn_bool(s: &str) -> IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, v) = alt((
        map(tag("true"), |_| Edn::Bool(true)),
        map(tag("false"), |_| Edn::Bool(false)),
    ))(s)?;

    Ok((s, v))
}

fn edn_int(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, i) = map(
        alt((
            digit1,
            recognize(pair(tag("-"), digit1)),
            preceded(tag("+"), digit1),
        )),
        |digits: &str| digits.parse::<isize>().unwrap(),
    )(s)?;

    let (s, _) = not(tag("."))(s)?;

    Ok((s, Edn::Integer(i)))
}

fn edn_float(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, f) = alt((
        map(terminated(recognize(double), tag("M")), |d: &str| {
            Edn::Decimal(rust_decimal::Decimal::from_str(d).unwrap())
        }),
        map(nom::number::complete::double, |d| Edn::Float(d.into())),
    ))(s)?;

    Ok((s, f))
}

fn edn_string(s: &str) -> IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = tag("\"")(s)?;
    let (s, string) = map(
        escaped(none_of("\"\\"), '\\', one_of("\"ntr\\")),
        |s: &str| Edn::String(s),
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

    let mut optional_namespace = opt(terminated(identifier, tag("/")));
    let (s, namespace) = optional_namespace(s)?;
    let (s, sym) = identifier(s)?;

    Ok((
        s,
        Edn::Keyword {
            namespace,
            name: sym,
        },
    ))
}

fn edn_symbol(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    peek(not(tag(":")))(s)?;

    let mut optional_namespace = opt(terminated(identifier, tag("/")));
    let (s, namespace) = optional_namespace(s)?;
    let (s, sym) = identifier(s)?;

    Ok((
        s,
        Edn::Symbol {
            namespace,
            name: sym,
        },
    ))
}

fn edn_list(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = tag("(")(s)?;

    let (s, elements) = many0(delimited(opt(space_or_comma), edn_any, opt(space_or_comma)))(s)?;

    let (s, _) = tag(")")(s)?;

    Ok((s, Edn::List(elements.into_iter().flatten().collect())))
}

fn edn_vector(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = tag("[")(s)?;

    let (s, elements) = many0(delimited(opt(space_or_comma), edn_any, opt(space_or_comma)))(s)?;

    let (s, _) = tag("]")(s)?;

    Ok((s, Edn::Vector(elements.into_iter().flatten().collect())))
}

fn edn_map(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = tag("{")(s)?;

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
        Map::new,
        |mut acc: Map<_, _>, (k, v)| match (k, v) {
            (Some(kk), Some(vv)) => {
                acc.insert(kk, vv);
                acc
            }
            _ => acc,
        },
    ))(s)?;

    let (s, _) = tag("}")(s)?;

    Ok((s, Edn::Map(map.unwrap_or_else(Map::new))))
}

fn edn_set(s: &str) -> nom::IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = tag("#{")(s)?;

    let (s, set) = opt(fold_many1(
        delimited(opt(space_or_comma), edn_any, opt(space_or_comma)),
        Set::new,
        |mut acc: Set<_>, v| {
            if let Some(actual_v) = v {
                acc.insert(actual_v);
            }

            acc
        },
    ))(s)?;

    let (s, _) = tag("}")(s)?;

    Ok((s, Edn::Set(set.unwrap_or_else(Set::new))))
}

fn edn_comment(s: &str) -> IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, c) = preceded(
        tag(";"),
        value(
            Edn::Comment,
            alt((terminated(not_line_ending, line_ending), rest)),
        ),
    )(s)?;

    Ok((s, c))
}

fn edn_tag(s: &str) -> IResult<&str, crate::Edn, nom::error::VerboseError<&str>> {
    let (s, _) = preceded(tag("#"), peek(satisfy(|c: char| is_alphabetic(c as u8))))(s)?;

    let (s, tag) = alt((edn_tag_inst, edn_tag_uuid, edn_tag_user_defined))(s)?;

    Ok((s, Edn::Tag(tag)))
}

fn edn_tag_inst(s: &str) -> IResult<&str, crate::Tag, nom::error::VerboseError<&str>> {
    let (s, _) = tag("inst")(s)?;
    let (s, _) = space_or_comma(s)?;
    let (s, _) = tag("\"")(s)?;
    let (s, as_str) = take_until("\"")(s)?;
    let (s, _) = tag("\"")(s)?;

    let datetime = chrono::DateTime::parse_from_rfc3339(as_str).map_err(|e| {
        nom::Err::Error(nom::error::VerboseError::from_external_error(
            s,
            nom::error::ErrorKind::Tag,
            e,
        ))
    })?;

    let tag = Tag::Inst(datetime);

    Ok((s, tag))
}

fn edn_tag_uuid(s: &str) -> IResult<&str, crate::Tag, nom::error::VerboseError<&str>> {
    let (s, _) = tag("uuid")(s)?;
    let (s, _) = space_or_comma(s)?;
    let (s, _) = tag("\"")(s)?;
    let (s, as_str) = take_until("\"")(s)?;
    let (s, _) = tag("\"")(s)?;

    let uuid = uuid::Uuid::from_str(as_str).map_err(|e| {
        nom::Err::Error(nom::error::VerboseError::from_external_error(
            s,
            nom::error::ErrorKind::Tag,
            e,
        ))
    })?;

    let tag = Tag::UUID(uuid);

    Ok((s, tag))
}

fn edn_tag_user_defined(s: &str) -> IResult<&str, crate::Tag, nom::error::VerboseError<&str>> {
    let (s, prefix) = identifier(s)?;

    let (s, _) = tag("/")(s)?;

    let (s, name) = identifier(s)?;

    let (s, _) = space_or_comma(s)?;

    match edn_any(s)? {
        (s, Some(Edn::Tag(_))) | (s, Some(Edn::Comment)) | (s, None) => {
            Err(nom::Err::Error(VerboseError::from_external_error(
                s,
                nom::error::ErrorKind::Tag,
                InvalidTagElementError,
            )))
        }

        (s, Some(element)) => {
            let tag = Tag::UserDefined {
                prefix,
                name,
                element: Box::new(element),
            };

            Ok((s, tag))
        }
    }
}

fn edn_any(s: &str) -> IResult<&str, Option<crate::Edn>, nom::error::VerboseError<&str>> {
    let (s, edn) = alt((
        map(edn_string, Some),
        map(edn_bool, Some),
        map(edn_int, Some),
        map(edn_float, Some),
        map(edn_symbol, Some),
        map(edn_keyword, Some),
        map(edn_nil, Some),
        map(edn_char, Some),
        map(edn_list, Some),
        map(edn_map, Some),
        map(edn_vector, Some),
        map(edn_set, Some),
        map(edn_tag, Some),
        map(edn_comment, |_| None),
        edn_discard_sequence,
    ))(s)?;

    Ok((s, edn))
}

fn identifier(s: &str) -> nom::IResult<&str, &str, nom::error::VerboseError<&str>> {
    take_while1(matches_identifier_char)(s)
}

fn matches_identifier_char(c: char) -> bool {
    is_alphanumeric(c as u8) || c == '-' || c == '_' || c == '.' || c == '+' || c == '&'
}

// fork of https://docs.rs/nom/6.0.1/src/nom/number/complete.rs.html#1341-1361
fn hex_u32<'a, E: nom::error::ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, u32, E> {
    let (i, o) = take_while1(|chr| {
        let chr = chr as u8;
        (0x30..=0x39).contains(&chr) || (0x41..=0x46).contains(&chr) || (0x61..=0x66).contains(&chr)
    })(input)?;

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
            crate::Map::new()
        };
        ( $($x:expr, $y:expr),* ) => {
            {
                let mut hm = crate::Map::new();

                $(
                    hm.insert($x, $y);
                )*

                hm
            }
        };
    }

    macro_rules! hashset {
        () => {
            crate::Set::new()
        };
        ( $($x:expr),* ) => {
            {
                let mut hs = crate::Set::new();

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
        assert_eq!(
            res,
            Ok((
                "",
                Keyword {
                    namespace: None,
                    name: "a-kw"
                }
            ))
        );
    }

    #[test]
    fn keyword_namespaced() {
        let keystr = ":org.clojure/clojure";
        let res = nom::combinator::complete(edn_keyword)(keystr);
        assert_eq!(
            res,
            Ok((
                "",
                Keyword {
                    namespace: Some("org.clojure"),
                    name: "clojure"
                }
            ))
        );
    }

    #[test]
    fn int() {
        let intstr = "1";
        let res = edn_int(intstr);
        assert_eq!(res, Ok(("", Integer(1))));

        let intstr = "-1";
        let res = edn_int(intstr);
        assert_eq!(res, Ok(("", Integer(-1))));

        let intstr = isize::MAX.to_string();
        let res = edn_int(&intstr);
        assert_eq!(res, Ok(("", Integer(isize::MAX))));

        let intstr = isize::MIN.to_string();
        let res = edn_int(&intstr);
        assert_eq!(res, Ok(("", Integer(isize::MIN))));
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
        assert_eq!(
            res,
            Ok((
                "",
                Symbol {
                    namespace: None,
                    name: "a-sym"
                }
            ))
        );
    }

    #[test]
    fn symbol_namedspaced() {
        let symstr = "org.clojure/clojure";
        let res = edn_symbol(symstr);
        assert_eq!(
            res,
            Ok((
                "",
                Symbol {
                    namespace: Some("org.clojure"),
                    name: "clojure"
                }
            ))
        );
    }

    #[test]
    fn string() {
        let strstr = "\"hello\"";
        let res = edn_string(strstr);
        assert_eq!(res, Ok(("", String("hello"))));
    }

    #[test]
    fn string_escapes() {
        let mut embedded_str = std::fs::File::open("./fixtures/embedded_str").unwrap();
        // let embedded_str = "\"hel\"lo\"";
        let mut buf = std::string::String::new();
        embedded_str.read_to_string(&mut buf).unwrap();
        let embedded_res = edn_string(&mut buf);
        assert_eq!(embedded_res, Ok(("", String("hel\\\"lo"))));
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
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Keyword {
                        namespace: None,
                        name: "b"
                    },
                    Keyword {
                        namespace: None,
                        name: "c"
                    }
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
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Symbol {
                        namespace: None,
                        name: "b"
                    },
                    Bool(true),
                    Bool(false),
                    Symbol {
                        namespace: None,
                        name: "some-sym"
                    },
                    Keyword {
                        namespace: None,
                        name: "c"
                    }
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
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Symbol {
                        namespace: None,
                        name: "b"
                    },
                    List(vec!(
                        Integer(1),
                        Integer(2),
                        Integer(5),
                        Keyword {
                            namespace: None,
                            name: "e"
                        }
                    )),
                    Bool(true),
                    Bool(false),
                    Vector(vec!(Vector(vec!()), Integer(232), List(vec!()))),
                    Symbol {
                        namespace: None,
                        name: "some-sym"
                    },
                    Keyword {
                        namespace: None,
                        name: "c"
                    }
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
                Vector(vec![
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Keyword {
                        namespace: None,
                        name: "b"
                    },
                    Keyword {
                        namespace: None,
                        name: "c"
                    }
                ])
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
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Symbol {
                        namespace: None,
                        name: "b"
                    },
                    Integer(1),
                    Bool(true),
                    Bool(false),
                    Symbol {
                        namespace: None,
                        name: "some-sym"
                    },
                    Integer(44444),
                    Keyword {
                        namespace: None,
                        name: "c"
                    }
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
                Vector(vec![
                    Symbol {
                        namespace: None,
                        name: "&"
                    },
                    Symbol {
                        namespace: None,
                        name: "args"
                    }
                ])
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
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Symbol {
                        namespace: None,
                        name: "b"
                    },
                    Bool(true),
                    Bool(false),
                    Vector(vec!(Vector(vec!()), Integer(232), List(vec!()))),
                    Symbol {
                        namespace: None,
                        name: "some-sym"
                    },
                    Keyword {
                        namespace: None,
                        name: "c"
                    }
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
            Ok((
                "",
                Map(hashmap!(
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Integer(1)
                ))
            ))
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
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
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
            Map(m) => !m.values().collect::<crate::Set<&Edn>>().contains(&Symbol {
                namespace: None,
                name: "bcd"
            }),
            _ => panic!(),
        });

        assert_eq!(
            (map_remaining, map_res),
            (
                "",
                Map(hashmap!(
                    Vector(vec!(Integer(1), Integer(2), Integer(3))),
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Map(hashmap!()),
                    Keyword {
                        namespace: None,
                        name: "zzzzzzzz"
                    }
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
            Ok((
                "",
                Set(hashset!(
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Integer(1)
                ))
            ))
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
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
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
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Map(hashmap!()),
                    Keyword {
                        namespace: None,
                        name: "bcd"
                    },
                    Set(hashset!()),
                    Vector(vec!()),
                    List(vec!(Integer(1), Integer(2), Set(hashset!()))),
                    Keyword {
                        namespace: None,
                        name: "zzzzzzzz"
                    }
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
                    Keyword {
                        namespace: None,
                        name: "a"
                    },
                    Float(1.01.into()),
                    Keyword {
                        namespace: None,
                        name: "c"
                    },
                    Keyword {
                        namespace: None,
                        name: "d"
                    }
                ))
            ))
        )
    }

    #[test]
    fn tags_inst() {
        let inst = chrono::DateTime::parse_from_rfc3339("1985-04-12T23:20:50.52Z").unwrap();
        assert_eq!(
            edn_tag(r###"#inst "1985-04-12T23:20:50.52Z""###),
            Ok(("", Edn::Tag(crate::Tag::Inst(inst))))
        );
        assert_eq!(
            edn_tag("#inst \t \n\n \r\n \t\t \"1985-04-12T23:20:50.52Z\""),
            Ok(("", Edn::Tag(crate::Tag::Inst(inst))))
        );
    }
    #[test]
    fn tags_uuid() {
        let uuid = uuid::Uuid::from_str("f81d4fae-7dec-11d0-a765-00a0c91e6bf6").unwrap();
        assert_eq!(
            edn_tag("#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""),
            Ok(("", Edn::Tag(crate::Tag::UUID(uuid))))
        );
    }

    #[test]
    fn tags_user_defined() {
        // custom simple tag
        assert_eq!(
            edn_tag("#myapp/Foo \"string as tag element\""),
            Ok((
                "",
                Edn::Tag(crate::Tag::UserDefined {
                    prefix: "myapp",
                    name: "Foo",
                    element: Box::new(Edn::String("string as tag element"))
                })
            ))
        );

        // custom complex tag
        assert_eq!(
            edn_tag("#myapp/Foo [1, \"2\", :a_keyword]"),
            Ok((
                "",
                Edn::Tag(crate::Tag::UserDefined {
                    prefix: "myapp",
                    name: "Foo",
                    element: Box::new(Edn::Vector(vec![
                        Edn::Integer(1),
                        Edn::String("2"),
                        Edn::Keyword {
                            namespace: None,
                            name: "a_keyword"
                        }
                    ]))
                })
            ))
        );

        // tags require a name
        assert_eq!(
            edn_tag("#myapp [1, \"2\", :a_keyword]"),
            Err(nom::Err::Error(VerboseError {
                errors: vec![
                    (
                        " [1, \"2\", :a_keyword]",
                        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Tag)
                    ),
                    (
                        "myapp [1, \"2\", :a_keyword]",
                        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Alt)
                    )
                ]
            }))
        );

        // tags require a prefix
        assert_eq!(
            edn_tag("#Foo [1, \"2\", :a_keyword]"),
            Err(nom::Err::Error(VerboseError {
                errors: vec![
                    (
                        " [1, \"2\", :a_keyword]",
                        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Tag)
                    ),
                    (
                        "Foo [1, \"2\", :a_keyword]",
                        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Alt)
                    )
                ]
            }))
        );

        // tags can't be tag elements
        assert_eq!(
            edn_tag("#myapp/Foo #myapp/ASecondTag []"),
            Err(nom::Err::Error(VerboseError {
                errors: vec![
                    (
                        "",
                        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Tag)
                    ),
                    (
                        "myapp/Foo #myapp/ASecondTag []",
                        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Alt)
                    )
                ]
            }))
        );

        // comments can't be tag elements
        assert_eq!(
            edn_tag("#myapp/Foo ;; some comment"),
            Err(nom::Err::Error(VerboseError {
                errors: vec![
                    (
                        "",
                        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Tag)
                    ),
                    (
                        "myapp/Foo ;; some comment",
                        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Alt)
                    )
                ]
            }))
        );
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
                Keyword {
                    namespace: None,
                    name: "a"
                },
                Integer(1),
                Keyword {
                    namespace: None,
                    name: "b"
                },
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
                Keyword {
                    namespace: None,
                    name: "paths"
                },
                Vector(vec!(String("resources"), String("src"))),
                Keyword {
                    namespace: None,
                    name: "deps"
                },
                Map(hashmap!(
                    Symbol {
                        namespace: Some("org.clojure"),
                        name: "clojure"
                    },
                    Map(hashmap!(
                        Keyword {
                            namespace: Some("mvn"),
                            name: "version"
                        },
                        String("1.10.0")
                    )),
                    Symbol {
                        namespace: None,
                        name: "instaparse".into()
                    },
                    Map(hashmap!(
                        Keyword {
                            namespace: Some("mvn"),
                            name: "version"
                        },
                        String("1.4.9")
                    )),
                    Symbol {
                        namespace: None,
                        name: "quil"
                    },
                    Map(hashmap!(
                        Keyword {
                            namespace: Some("mvn"),
                            name: "version"
                        },
                        String("2.8.0"),
                        Keyword {
                            namespace: None,
                            name: "exclusions"
                        },
                        Vector(vec!(Symbol {
                            namespace: Some("com.lowagie"),
                            name: "itext"
                        }))
                    ),),
                    Symbol {
                        namespace: Some("com.hypirion"),
                        name: "clj-xchart"
                    },
                    Map(hashmap!(
                        Keyword {
                            namespace: Some("mvn"),
                            name: "version"
                        },
                        String("0.2.0")
                    )),
                    Symbol {
                        namespace: Some("net.mikera"),
                        name: "core.matrix"
                    },
                    Map(hashmap!(
                        Keyword {
                            namespace: Some("mvn"),
                            name: "version"
                        },
                        String("0.62.0")
                    )),
                    Symbol {
                        namespace: Some("net.mikera"),
                        name: "vectorz-clj"
                    },
                    Map(hashmap!(
                        Keyword {
                            namespace: Some("mvn"),
                            name: "version"
                        },
                        String("0.48.0")
                    ))
                )),
                Keyword {
                    namespace: None,
                    name: "aliases"
                },
                Map(hashmap!(
                    Keyword {
                        namespace: None,
                        name: "more-mem"
                    },
                    Map(hashmap!(
                        Keyword {
                            namespace: None,
                            name: "jvm-opts"
                        },
                        Vector(vec!(String("-Xmx12G -Xms12G")))
                    ),),
                    Keyword {
                        namespace: None,
                        name: "test"
                    },
                    Map(hashmap!(
                        Keyword {
                            namespace: None,
                            name: "extra-paths"
                        },
                        Vector(vec!(String("test"))),
                        Keyword {
                            namespace: None,
                            name: "extra-deps"
                        },
                        Map(hashmap!(
                            Symbol {
                                namespace: Some("org.clojure"),
                                name: "test.check"
                            },
                            Map(hashmap!(
                                Keyword {
                                    namespace: Some("mvn"),
                                    name: "version"
                                },
                                String("RELEASE")
                            ))
                        ))
                    )),
                    Keyword {
                        namespace: None,
                        name: "runner"
                    },
                    Map(hashmap!(
                        Keyword {
                            namespace: None,
                            name: "extra-deps"
                        },
                        Map(hashmap!(
                            Symbol {
                                namespace: Some("com.cognitect"),
                                name: "test-runner"
                            },
                            Map(hashmap!(
                                Keyword {
                                    namespace: Some("git"),
                                    name: "url"
                                },
                                String("https://github.com/cognitect-labs/test-runner"),
                                Keyword {
                                    namespace: None,
                                    name: "sha"
                                },
                                String("76568540e7f40268ad2b646110f237a60295fa3c")
                            ),)
                        )),
                        Keyword {
                            namespace: None,
                            name: "main-opts"
                        },
                        Vector(vec!(
                            String("-m"),
                            String("cognitect.test-runner"),
                            String("-d"),
                            String("test")
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
        assert_ne!(
            Nil,
            Keyword {
                namespace: None,
                name: "Nil"
            }
        );

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
        assert_eq!(
            Symbol {
                namespace: None,
                name: "a"
            },
            Symbol {
                namespace: None,
                name: "a"
            }
        );
        assert_ne!(
            Symbol {
                namespace: None,
                name: "a"
            },
            Symbol {
                namespace: None,
                name: "z"
            }
        );

        // Keyword(String),
        assert_eq!(
            Keyword {
                namespace: None,
                name: "a"
            },
            Keyword {
                namespace: None,
                name: "a"
            }
        );
        assert_ne!(
            Keyword {
                namespace: None,
                name: "a"
            },
            Keyword {
                namespace: None,
                name: "z"
            }
        );

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
            Map(hashmap!(
                Keyword {
                    namespace: None,
                    name: "a"
                },
                Integer(1)
            )),
            Map(hashmap!(
                Keyword {
                    namespace: None,
                    name: "a"
                },
                Integer(1)
            ))
        );
        assert_eq!(
            Map(hashmap!(
                Keyword {
                    namespace: None,
                    name: "a"
                },
                Integer(1),
                Keyword {
                    namespace: None,
                    name: "b"
                },
                Integer(2)
            )),
            Map(hashmap!(
                Keyword {
                    namespace: None,
                    name: "b"
                },
                Integer(2),
                Keyword {
                    namespace: None,
                    name: "a"
                },
                Integer(1)
            ))
        );
        assert_ne!(
            Map(hashmap!(
                Keyword {
                    namespace: None,
                    name: "a"
                },
                Float(2.1.into())
            )),
            Map(hashmap!(
                Keyword {
                    namespace: None,
                    name: "a"
                },
                Float(1.2.into())
            ))
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
