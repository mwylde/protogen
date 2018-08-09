#[macro_use]
extern crate nom;

use nom::{is_alphanumeric, is_hex_digit, is_digit};
use std::str::FromStr;
use std::str;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn multiple_fields() {
        let text = r#"
hci_command = {
  public @ocf: u8;
  length: u8;
}"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                    },
                    Field {
                        public: false,
                        variable: false,
                        name: "length".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                    }
                ] })));
    }

    #[test]
    fn simple() {
        let text = " hci_command =  { }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![] })));
    }

    #[test]
    fn single_field() {
        let text = " hci_command =  { public @ocf : u8; }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                    }
                ]
            })));
    }

    #[test]
    fn message_args() {
        let text = " hci_command ($type: u8, $name: String) = { }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![
                    Arg {
                        name: "type".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                    },
                    Arg {
                        name: "name".to_string(),
                        data_type: DataType::Value("String".to_string()),
                        value: None
                    }
                ],
                fields: vec![]
            })));
    }

    #[test]
    fn message_args_with_values() {
        let text = " hci_command ($type: u8 = 0xFF, $name: String = \"hello world!\") = { }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![
                    Arg {
                        name: "type".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: Some(Value::Number(0xFF)),
                    },
                    Arg {
                        name: "name".to_string(),
                        data_type: DataType::Value("String".to_string()),
                        value: Some(Value::String("hello world!".to_string())),
                    }
                ],
                fields: vec![]
            })));
    }

    #[test]
    fn field_hex_value() {
        let text = " hci_command =  { public @ocf : u8 = 0x22; }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: Some(Value::Number(0x22u64)),
                    }
                ]
            })));
    }

    #[test]
    fn field_string_value() {
        let text = " hci_command =  { public @name : String = \"hello world!\" ; }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "name".to_string(),
                        data_type: DataType::Value("String".to_string()),
                        value: Some(Value::String("hello world!".to_string())),
                    }
                ]
            })));
    }

    #[test]
    fn test_number() {
        assert_eq!(number(b"0x10;"), Ok((&[b';'][..], Value::Number(0x10))));
        assert_eq!(number(b"0X5235;"), Ok((&[b';'][..], Value::Number(0x5235))));
        assert_eq!(number(b"1241;"), Ok((&[b';'][..], Value::Number(1241))));
    }

    #[test]
    fn multiple_fields_with_values() {
        let text = r#"
hci_command = {
  public @ocf: u8 = 0x2214;
  length: u32 = 55;
  @name: String = "this is a string.";
}"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: Some(Value::Number(0x2214)),
                    },
                    Field {
                        public: false,
                        variable: false,
                        name: "length".to_string(),
                        data_type: DataType::Value("u32".to_string()),
                        value: Some(Value::Number(55)),
                    },
                    Field {
                        public: false,
                        variable: true,
                        name: "name".to_string(),
                        data_type: DataType::Value("String".to_string()),
                        value: Some(Value::String("this is a string.".to_string())),
                    }
                ] })));
    }

    #[test]
    fn choose() {
        let text = r#"
hci_message ($name: u32) = {
  @type: u8;
  public message: choose {
    HciCommand = hci_command(@type) |
    HciData = hci_data(@type, $name)
  };
}"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_message".to_string(),
                args: vec![
                    Arg {
                        name: "name".to_string(),
                        data_type: DataType::Value("u32".to_string()),
                        value: None,
                    },
                ],
                fields: vec![
                    Field {
                        public: false,
                        variable: true,
                        name: "type".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                    },
                    Field {
                        public: true,
                        variable: false,
                        name: "message".to_string(),
                        data_type: DataType::Choose(vec![
                            ChooseVariant {
                                name: "HciCommand".to_string(),
                                data_type: DataType::Message {
                                    name: "hci_command".to_string(),
                                    args: vec!["@type".to_string()],
                                }
                            },
                            ChooseVariant {
                                name: "HciData".to_string(),
                                data_type: DataType::Message {
                                    name: "hci_data".to_string(),
                                    args: vec![
                                        "@type".to_string(),
                                        "$name".to_string()
                                    ],
                                }
                            },
                        ]),
                        value: None,
                    }
                ] })));
    }

    #[test]
    fn array() {
        let text = r#"
hci_command = {
  @type: [u8; 12];
  @length: u8;
  public @data: [u8; @length];
}"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: false,
                        variable: true,
                        name: "type".to_string(),
                        data_type: DataType::Array {
                            data_type: Box::new(DataType::Value("u8".to_string())),
                            length: Expression::Value(Value::Number(12))
                        },
                        value: None,
                    },
                    Field {
                        public: false,
                        variable: true,
                        name: "length".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                    },
                    Field {
                        public: true,
                        variable: true,
                        name: "data".to_string(),
                        data_type: DataType::Array {
                            data_type: Box::new(DataType::Value("u8".to_string())),
                            length: Expression::Variable("@length".to_string())
                        },
                        value: None,
                    },

                ] })));
    }

    #[test]
    fn apply() {
        let text = r#"
hci_command = {
  public command: apply @data choose {
    Reset = reset(@ocf) |
    SetEventFilter = set_event_filter(@ocf)
  };
}"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: false,
                        name: "command".to_string(),
                        data_type: DataType::Apply {
                            source: "@data".to_string(),
                            data_type: Box::new(DataType::Choose(vec![
                                ChooseVariant {
                                    name: "Reset".to_string(),
                                    data_type: DataType::Message {
                                        name: "reset".to_string(),
                                        args: vec!["@ocf".to_string()],
                                    },
                                },
                                ChooseVariant {
                                    name: "SetEventFilter".to_string(),
                                    data_type: DataType::Message {
                                        name: "set_event_filter".to_string(),
                                        args: vec!["@ocf".to_string()],
                                    },
                                }
                            ])),
                        },
                        value: None,
                    },

                ] })));
    }

    #[test]
    fn message_type() {
        let text = r#"
inquiry_result = {
  public condition: filter_condition();
}!"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[33][..], Message {
                name: "inquiry_result".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: false,
                        name: "condition".to_string(),
                        data_type: DataType::Message {
                            name: "filter_condition".to_string(),
                            args: vec![],
                        },
                        value: None,
                    },

                ] })));
    }

    #[test]
    fn string_parser() {
        assert_eq!(string("\"hello world\"".as_bytes()),
        Ok((&[][..], Value::String("hello world".to_string()))))
    }

    #[test]
    fn test_messages() {
        let text = r#"
        hci_command = { }
        hci_message = {}
        "#;

        assert_eq!(
            source_file(text.trim().as_bytes()),
            Ok((&[][..], vec![
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![]
                },
                Message {
                    name: "hci_message".to_string(),
                    args: vec![],
                    fields: vec![]
                },
            ])));
    }

    #[test]
    fn test_file() {
        let source = include_str!("../examples/hci_message.pg");
        match source_file(source.trim().as_bytes()) {
            Ok((rem, messages)) => {
                assert_eq!(0, rem.len());
                assert_eq!(11, messages.len());
            }
            Err(e) => {
                debug_assert!(false, "got error: {:?}", e);
            }
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Expression {
    Value(Value),
    Variable(String),
}


#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum DataType {
    Value(String),
    Array {
        data_type: Box<DataType>,
        length: Expression,
    },
    Message {
        name: String,
        args: Vec<String>
    },
    Choose(Vec<ChooseVariant>),
    Apply {
        source: String,
        data_type: Box<DataType>,
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Value {
    String(String),
    Number(u64),
    ByteArray(Vec<u8>),
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Field {
    public: bool,
    variable: bool,
    name: String,
    data_type: DataType,
    value: Option<Value>,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct ChooseVariant {
    name: String,
    data_type: DataType
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Arg {
    name: String,
    data_type: DataType,
    value: Option<Value>,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Message {
    name: String,
    args: Vec<Arg>,
    fields: Vec<Field>,
}

fn is_symbol_char(i: u8) -> bool {
    is_alphanumeric(i) || i == b'_'
}

fn is_string_char(i: u8) -> bool {
    i != b'\"'
}

named!(symbol<&str>, map_res!(take_while1!(is_symbol_char), str::from_utf8));

named!(pub string<Value>,
    map!(delimited!(tag!("\""),
        map_res!(
            // escaped!(call!(is_string_char), '\\', one_of!("\"n\\")),
            take_while!(is_string_char),
            str::from_utf8),
        tag!("\"")), |s| Value::String(s.to_string())));

fn convert_hex(bytes: &[u8]) -> u64 {
    bytes.iter()
        .rev()
        .enumerate()
        .map(|(k, &v)| {
            ((v as char).to_digit(16).unwrap_or(0) << (k * 4)) as u64
        })
        .sum()
}

named!(hex_u64<u64>,
    map!(take_while_m_n!(1, 16, is_hex_digit), convert_hex));

named!(hex_number<u64>,
    do_parse!(
        _0x: alt_complete!(tag!("0x") | tag!("0X")) >>
        digits: hex_u64 >>
        ( digits )));

named!(dec_number<u64>,
    map_res!(map_res!(take_while1!(is_digit), str::from_utf8),
        FromStr::from_str));

named!(number<Value>,
    map!(alt_complete!(hex_number | dec_number), Value::Number));

named!(value<Value>,
  alt!(string | number));

named!(variable<String>,
    do_parse!(
        sigil: map_res!(alt!(tag!("@") | tag!("$")), str::from_utf8) >>
        name: symbol >>
        (
             [sigil, name].join("").to_string()
        )));

named!(message_type<DataType>,
    ws!(do_parse!(
        name: symbol >>
        args: delimited!(
            tag!("("),
            separated_list!(tag!(","), variable),
            tag!(")")) >>
        (
           DataType::Message { name: name.to_string(), args }
        ))));

named!(choose_type<DataType>,
    ws!(do_parse!(
        _choose: tag!("choose") >>
        variants: delimited!(
            tag!("{"),
            separated_list!(tag!("|"),
                do_parse!(
                    name: symbol >>
                    _eq: tag!("=") >>
                    data_type: data_type >>
                    (
                        ChooseVariant {
                            name: name.to_string(),
                            data_type
                        }
                    )
                )
            ),
            tag!("}")
        ) >>
        (
            DataType::Choose(variants)
        ))));

named!(apply_type<DataType>,
    ws!(do_parse!(
         _apply: tag!("apply") >>
         source: variable >>
         data_type: data_type >>
         (
             DataType::Apply {
                 source,
                 data_type: Box::new(data_type)
             }
         ))));

named!(expression<Expression>,
    ws!(alt!(
       complete!(variable) => {|v| Expression::Variable(v)} |
       complete!(value)    => {|v| Expression::Value(v)})));

named!(array_type<DataType>,
    ws!(delimited!(
        tag!("["),
        map!(separated_pair!(data_type, tag!(";"), expression),
            |ab| DataType::Array { data_type: Box::new(ab.0), length: ab.1}),
        tag!("]"))));

named!(data_type<DataType>,
    ws!(alt!(
        complete!(array_type) |
        complete!(choose_type) |
        complete!(apply_type) |
        complete!(message_type) |
        map!(complete!(symbol), |s| DataType::Value(s.to_string())))));

named!(assign_value<Value>,
    ws!(do_parse!(
        _equals: tag!("=") >>
        value: value >>
        ( value ))));

named!(field<Field>,
    ws!(do_parse!(
        public: opt!(tag!("public")) >>
        variable: opt!(tag!("@")) >>
        name: symbol >>
        _colon: tag!(":") >>
        data_type: data_type >>
        value: opt!(assign_value) >>
        _semicolon: tag!(";") >>
        (
            Field {
               public: public.is_some(),
               variable: variable.is_some(),
               name: name.trim().to_string(),
               data_type,
               value
            }
        ))));

named!(args<Vec<Arg>>,
    ws!(delimited!(
        tag!("("),
        separated_list!(tag!(","),
            do_parse!(
                _sigil: tag!("$") >>
                name: symbol >>
                _colon: tag!(":") >>
                type_name: symbol >>
                value: opt!(assign_value) >>
                (
                   Arg {
                      name: name.to_string(),
                      data_type: DataType::Value(type_name.to_string()),
                      value: value
                   }
                )
            )),
        tag!(")")))
);

fn is_whitespace(b: u8) -> bool {
    b == b' ' || b == b'\t' || b == b'\n' || b == b'\r'
}

named!(many_ws, take_while!(is_whitespace));

named!(pub message<Message>,
    dbg_dmp!(do_parse!(
      many_ws >>
      name: symbol >>
      many_ws >>
      args: opt!(complete!(args)) >>
      many_ws >>
      _eq: tag!("=") >>
      many_ws >>
      fields: delimited!(tag!("{"), ws!(many0!(complete!(field))), tag!("}")) >>
      (
        Message {
            name: name.trim().to_string(),
            args: args.unwrap_or(vec![]),
            fields,
        }
      ))));

named!(pub source_file<Vec<Message>>,
    do_parse!(
        many_ws >>
        messages: separated_list!(complete!(many_ws), complete!(message)) >>
        opt!(complete!(many_ws)) >>
        (
             messages
        )));
