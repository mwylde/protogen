use nom::*;
use std::str;
use std::str::FromStr;

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
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![
                        Field {
                            public: true,
                            variable: true,
                            name: "ocf".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: None,
                            constraints: None,
                        },
                        Field {
                            public: false,
                            variable: false,
                            name: "length".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: None,
                            constraints: None
                        },
                    ],
                }
            ))
        );
    }

    #[test]
    fn simple() {
        let text = " hci_command =  { }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![],
                }
            ))
        );
    }

    #[test]
    fn single_field() {
        let text = " hci_command =  { public @ocf : u8; }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        apply_to: None,
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                        constraints: None
                    }],
                }
            ))
        );
    }

    #[test]
    fn comment() {
        let text = r"
        // hci command
        hci_command =  {
            // yeah ocf
            public @ocf : u8;
        }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        apply_to: None,
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                        constraints: None,
                    }],
                }
            ))
        );
    }

    #[test]
    fn message_args() {
        let text = " hci_command ($type: u8, public $name: String) = { }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![
                        Arg {
                            public: false,
                            name: "type".to_string(),
                            data_type: DataType::Value("u8".to_string()),
                            value: None,
                        },
                        Arg {
                            public: true,
                            name: "name".to_string(),
                            data_type: DataType::Value("String".to_string()),
                            value: None,
                        },
                    ],
                    fields: vec![],
                }
            ))
        );
    }

    #[test]
    fn message_args_with_values() {
        let text = " hci_command ($type: u8 = 0xFF, $name: String = \"hello world!\") = { }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![
                        Arg {
                            public: false,
                            name: "type".to_string(),
                            data_type: DataType::Value("u8".to_string()),
                            value: Some(Value::Number(0xFF)),
                        },
                        Arg {
                            public: false,
                            name: "name".to_string(),
                            data_type: DataType::Value("String".to_string()),
                            value: Some(Value::String("hello world!".to_string())),
                        },
                    ],
                    fields: vec![],
                }
            ))
        );
    }

    #[test]
    fn field_hex_value() {
        let text = " hci_command =  { public @ocf : u8 = 0x22; }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        apply_to: None,
                        data_type: DataType::Value("u8".to_string()),
                        value: Some(Expression::Value(Value::Number(0x22u64))),
                        constraints: None,
                    }],
                }
            ))
        );
    }

    #[test]
    fn field_constraints() {
        let text = " hci_command =  { \
           header: [u8; 3] | [b\"wtf\"]; \
           public @ocf : u8 | [0x22 + 10, 5]; }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![
                        Field {
                            public: false,
                            variable: false,
                            name: "header".to_string(),
                            apply_to: None,
                            data_type: DataType::Array {
                                data_type: Box::new(DataType::Value("u8".to_string())),
                                length: ex_num(3)
                            },
                            value: None,
                            constraints: Some(vec![Expression::Value(Value::ByteArray(
                                "wtf".as_bytes().to_vec()))]),
                        },
                        Field {
                            public: true,
                            variable: true,
                            name: "ocf".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: None,
                            constraints: Some(vec![
                                Expression::Binop("+".to_string(),
                                                  Box::new(ex_num(0x22)),
                                                  Box::new(ex_num(10))),
                                ex_num(5)]),
                        }],
                }
            ))
        );
    }

    #[test]
    fn field_string_value() {
        let text = " hci_command =  { public @name : String = \"hello world!\" ; }";

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![Field {
                        public: true,
                        variable: true,
                        name: "name".to_string(),
                        apply_to: None,
                        data_type: DataType::Value("String".to_string()),
                        value: Some(Expression::Value(Value::String("hello world!".to_string()))),
                        constraints: None,
                    }],
                }
            ))
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(number_value(b"0x10;"), Ok((&[b';'][..], Value::Number(0x10))));
        assert_eq!(number_value(b"0X5235;"), Ok((&[b';'][..], Value::Number(0x5235))));
        assert_eq!(number_value(b"1241;"), Ok((&[b';'][..], Value::Number(1241))));
        assert_eq!(number_value(b"0b100000010011001;"), Ok((&[b';'][..], Value::Number(16537))));
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
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![
                        Field {
                            public: true,
                            variable: true,
                            name: "ocf".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: Some(Expression::Value(Value::Number(0x2214))),
                            constraints: None,
                        },
                        Field {
                            public: false,
                            variable: false,
                            name: "length".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u32".to_string()),
                            value: Some(Expression::Value(Value::Number(55))),
                            constraints: None,
                        },
                        Field {
                            public: false,
                            variable: true,
                            name: "name".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("String".to_string()),
                            value: Some(Expression::Value(Value::String("this is a string.".to_string()))),
                            constraints: None,
                        },
                    ],
                }
            ))
        );
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
            Ok((
                &[][..],
                Message {
                    name: "hci_message".to_string(),
                    args: vec![Arg {
                        public: false,
                        name: "name".to_string(),
                        data_type: DataType::Value("u32".to_string()),
                        value: None,
                    }],
                    fields: vec![
                        Field {
                            public: false,
                            variable: true,
                            name: "type".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: None,
                            constraints: None,
                        },
                        Field {
                            public: true,
                            variable: false,
                            name: "message".to_string(),
                            apply_to: None,
                            data_type: DataType::Choose(vec![
                                ChooseVariant {
                                    name: "HciCommand".to_string(),
                                    data_type: DataType::Message {
                                        name: "hci_command".to_string(),
                                        args: vec![Expression::Variable("@type".to_string())],
                                    },
                                },
                                ChooseVariant {
                                    name: "HciData".to_string(),
                                    data_type: DataType::Message {
                                        name: "hci_data".to_string(),
                                        args: vec![
                                            Expression::Variable("@type".to_string()),
                                            Expression::Variable("$name".to_string())],
                                    },
                                },
                            ]),
                            value: None,
                            constraints: None,
                        },
                    ],
                }
            ))
        );
    }

    fn ex_var(s: &str) -> Expression {
        Expression::Variable(s.to_string())
    }

    fn ex_num(d: u64) -> Expression {
        Expression::Value(Value::Number(d))
    }

    #[test]
    fn str_utf8() {
        let text = r#"
message = {
  @len: u8;
  public str: str_utf8(@len);
}"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "message".to_string(),
                    args: vec![],
                    fields: vec![
                        Field {
                            public: false,
                            variable: true,
                            name: "len".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: None,
                            constraints: None,
                        },
                        Field {
                            public: true,
                            variable: false,
                            name: "str".to_string(),
                            apply_to: None,
                            data_type: DataType::Message {
                                name: "str_utf8".to_string(),
                                args: vec![
                                    Expression::Variable("@len".to_string())
                                ]
                            },
                            value: None,
                            constraints: None,
                        }
                    ]
                }))
        )
    }

    #[test]
    fn array() {
        let text = r#"
hci_command = {
  @type: [u8; 12];
  @length: u8;
  public @data: [u8; @length];
  expr: [u8; @length / 2];
}"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![
                        Field {
                            public: false,
                            variable: true,
                            name: "type".to_string(),
                            apply_to: None,
                            data_type: DataType::Array {
                                data_type: Box::new(DataType::Value("u8".to_string())),
                                length: Expression::Value(Value::Number(12)),
                            },
                            value: None,
                            constraints: None,
                        },
                        Field {
                            public: false,
                            variable: true,
                            name: "length".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: None,
                            constraints: None,
                        },
                        Field {
                            public: true,
                            variable: true,
                            name: "data".to_string(),
                            apply_to: None,
                            data_type: DataType::Array {
                                data_type: Box::new(DataType::Value("u8".to_string())),
                                length: ex_var("@length"),
                            },
                            value: None,
                            constraints: None,
                        },
                        Field {
                            public: false,
                            variable: false,
                            name: "expr".to_string(),
                            apply_to: None,
                            data_type: DataType::Array {
                                data_type: Box::new(DataType::Value("u8".to_string())),
                                length: Expression::Binop("/".to_string(),
                                                          Box::new(ex_var("@length")),
                                                          Box::new(ex_num(2)))
                            },
                            value: None,
                            constraints: None,
                        },
                    ],
                }
            ))
        );
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
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![Field {
                        public: true,
                        variable: false,
                        name: "command".to_string(),
                        apply_to: Some("@data".to_string()),
                        data_type: DataType::Choose(vec![
                            ChooseVariant {
                                name: "Reset".to_string(),
                                data_type: DataType::Message {
                                    name: "reset".to_string(),
                                    args: vec![Expression::Variable("@ocf".to_string())],
                                },
                            },
                            ChooseVariant {
                                name: "SetEventFilter".to_string(),
                                data_type: DataType::Message {
                                    name: "set_event_filter".to_string(),
                                    args: vec![Expression::Variable("@ocf".to_string())],
                                },
                            },
                        ]),
                        value: None,
                        constraints: None,
                    }],
                }
            ))
        );
    }

    #[test]
    fn message_type() {
        let text = r#"
inquiry_result = {
  public condition: filter_condition();
}!"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[33][..],
                Message {
                    name: "inquiry_result".to_string(),
                    args: vec![],
                    fields: vec![Field {
                        public: true,
                        variable: false,
                        name: "condition".to_string(),
                        apply_to: None,
                        data_type: DataType::Message {
                            name: "filter_condition".to_string(),
                            args: vec![],
                        },
                        value: None,
                        constraints: None,
                    }],
                }
            ))
        );
    }

    #[test]
    fn string_parser() {
        assert_eq!(
            string_value("\"hello world\"".as_bytes()),
            Ok((&[][..], Value::String("hello world".to_string())))
        )
    }

    #[test]
    fn test_messages() {
        let text = r#"
        hci_command = { }
        hci_message = {}
        "#;

        assert_eq!(
            source_file(text.trim().as_bytes()),
            Ok((
                &[][..],
                vec![
                    Message {
                        name: "hci_command".to_string(),
                        args: vec![],
                        fields: vec![],
                    },
                    Message {
                        name: "hci_message".to_string(),
                        args: vec![],
                        fields: vec![],
                    },
                ]
            ))
        );
    }

    #[test]
    fn expressions() {
        let text = "@var * 5;";
        assert_eq!(expression(text.as_bytes()),
                   Ok((&[b';'][..], Expression::Binop("*".to_string(),
                                                  Box::new(ex_var("@var")),
                                                  Box::new(ex_num(5))))));

        let text = "(@var * 5);";
        assert_eq!(expression(text.as_bytes()),
                   Ok((&[b';'][..], Expression::Binop("*".to_string(),
                                                      Box::new(ex_var("@var")),
                                                      Box::new(ex_num(5))))));

        fn var_minus_five() -> Box<Expression> {
            Box::new(Expression::Binop(
                "-".to_string(),
                Box::new(ex_var("@var")),
                Box::new(ex_num(5))))
        }

        let text = "(@var - 5) * 6;";
        assert_eq!(expression(text.as_bytes()),
                   Ok((&[b';'][..], Expression::Binop(
                       "*".to_string(),
                       var_minus_five(),
                       Box::new(ex_num(6))))));

        let text = "6 * (@var - 5);";
        assert_eq!(expression(text.as_bytes()),
                   Ok((&[b';'][..], Expression::Binop(
                       "*".to_string(),
                       Box::new(ex_num(6)),
                       var_minus_five()))));
    }

    #[test]
    fn expression_value() {
        let text = r#"
hci_command = {
  @opcode: u16;
  public ocf: u8 = @opcode & 0b11100000;
}"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((
                &[][..],
                Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                    fields: vec![
                        Field {
                            public: false,
                            variable: true,
                            name: "opcode".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u16".to_string()),
                            value: None,
                            constraints: None,
                        },
                        Field {
                            public: true,
                            variable: false,
                            name: "ocf".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: Some(Expression::Binop(
                                "&".to_string(),
                                Box::new(Expression::Variable("@opcode".to_string())),
                                Box::new(Expression::Value(Value::Number(224))))),
                            constraints: None,
                        },
                    ],
                }
            ))
        );
    }

//    #[test]
//    fn test_file() {
//        let source = include_str!("../../protogen-examples/src/hci_message.protogen");
//        match source_file(source.trim().as_bytes()) {
//            Ok((rem, messages)) => {
//                assert_eq!(0, rem.len());
//                assert_eq!(12, messages.len());
//            }
//            Err(e) => {
//                debug_assert!(false, "got error: {:?}", e);
//            }
//        }
//    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum Expression {
    Value(Value),
    Variable(String),
    Binop(String, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum DataType {
    Value(String),
    Array {
        data_type: Box<DataType>,
        length: Expression,
    },
    Message {
        name: String,
        args: Vec<Expression>,
    },
    ManyCombinator {
        data_type: Box<DataType>,
    },
    RestCombinator,
    Choose(Vec<ChooseVariant>)
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum Value {
    String(String),
    ByteArray(Vec<u8>),
    Number(u64),
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Field {
    pub public: bool,
    pub variable: bool,
    pub name: String,
    pub apply_to: Option<String>,
    pub data_type: DataType,
    pub value: Option<Expression>,
    pub constraints: Option<Vec<Expression>>
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct ChooseVariant {
    pub name: String,
    pub data_type: DataType,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct Arg {
    pub public: bool,
    pub name: String,
    pub data_type: DataType,
    pub value: Option<Value>,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Message {
    pub name: String,
    pub args: Vec<Arg>,
    pub fields: Vec<Field>,
}

fn is_symbol_char(i: u8) -> bool {
    is_alphanumeric(i) || i == b'_'
}

fn is_string_char(i: u8) -> bool {
    i != b'\"'
}

named!(comment<()>, do_parse!(tag!("//") >> take_until_and_consume!("\n") >> ()));

named!(br<()>, alt!(map!(multispace, |_| ()) | comment));

named!(
    symbol<&str>,
    map_res!(take_while1!(is_symbol_char), str::from_utf8)
);


named!(pub string<String>,
    map!(delimited!(tag!("\""),
        map_res!(
            // escaped!(call!(is_string_char), '\\', one_of!("\"n\\")),
            take_while!(is_string_char),
            str::from_utf8),
        tag!("\"")), |s| s.to_string()));

named!(pub string_value<Value>,
    map!(string, |s| Value::String(s)));

fn convert_hex(bytes: &[u8]) -> u64 {
    bytes
        .iter()
        .rev()
        .enumerate()
        .map(|(k, &v)| ((v as char).to_digit(16).unwrap_or(0) << (k * 4)) as u64)
        .sum()
}

fn convert_bin(bytes: &[u8]) -> u64 {
    bytes
        .iter()
        .rev()
        .enumerate()
        .map(|(k, &v)| ((v as char).to_digit(2).unwrap_or(0) << k) as u64)
        .sum()
}

named!(
    hex_u64<u64>,
    map!(take_while_m_n!(1, 16, is_hex_digit), convert_hex));

named!(
    bin_u64<u64>,
    map!(take_while_m_n!(1, 64, |c| c == b'0' || c == b'1'), convert_bin));

named!(
    hex_number<u64>,
    do_parse!(_0x: alt_complete!(tag!("0x") | tag!("0X")) >> digits: hex_u64 >> (digits)));

named!(bin_number<u64>,
  do_parse!(_0x: alt_complete!(tag!("0b") | tag!("0B")) >> digits: bin_u64 >> (digits)));

named!(
    dec_number<u64>,
    map_res!(
        map_res!(take_while1!(is_digit), str::from_utf8),
        FromStr::from_str
    )
);

named!(number<u64>,
    alt_complete!(hex_number | bin_number | dec_number)
);

named!(
    number_value<Value>,
    map!(number, Value::Number));

named!(
    byte_array_value<Value>,
    do_parse!(
      tag!("b") >> string: string >> ( Value::ByteArray(string.as_bytes().to_vec()))
    ));

named!(
    variable<String>,
    do_parse!(
        sigil: map_res!(alt!(tag!("@") | tag!("$")), str::from_utf8)
            >> name: symbol
            >> ([sigil, name].join("").to_string())
    ));

named!(value<Value>, alt!(byte_array_value | string_value | number_value));

named!(
    message_type<DataType>,
    ws!(do_parse!(
        name: symbol >> args: delimited!(tag!("("), separated_list!(tag!(","), expression), tag!(")"))
            >> (DataType::Message {
                name: name.to_string(),
                args,
            })
    ))
);

named!(
    many_combinator_type<DataType>,
    ws!(do_parse!(
        tag!("many!")
            >> data_type: delimited!(tag!("("), data_type, tag!(")"))
            >> (DataType::ManyCombinator {
                data_type: Box::new(data_type),
            })
    )));

named!(
    rest_combinator_type<DataType>,
    ws!(do_parse!(
        tag!("rest!()")
            >> (DataType::RestCombinator { })
    )));


named!(
    choose_type<DataType>,
    ws!(do_parse!(
        _choose: tag!("choose")
            >> variants:
                delimited!(
                    tag!("{"),
                    separated_list!(
                        tag!("|"),
                        do_parse!(
                            name: symbol >> _eq: tag!("=") >> data_type: data_type
                                >> (ChooseVariant {
                                    name: name.to_string(),
                                    data_type,
                                })
                        )
                    ),
                    tag!("}")
                ) >> (DataType::Choose(variants))
    ))
);

named!(apply<String>,
    ws!(do_parse!(
        tag!("apply") >>
        source: variable >>
         ( source )
    )));

named!(parens<Expression>,
    ws!(delimited!(char!('('), expression, char!(')'))));

named!(binop<Expression>,
    ws!(do_parse!(
        lh: terminal_expression >>
        op: map_res!(alt!(
            tag!("|") | tag!("&") | tag!("<<") | tag!(">>") |
            tag!("-") | tag!("+") | tag!("/") | tag!("*")), str::from_utf8) >>
        rh: expression >>
        ( Expression::Binop(op.to_string(), Box::new(lh), Box::new(rh))))));

named!(terminal_expression<Expression>,
    ws!(alt!(
        complete!(parens) => {|v| v} |
        complete!(variable) => {|v| Expression::Variable(v)} |
        complete!(value)   => {|v| Expression::Value(v)})));

named!(expression<Expression>,
    ws!(alt!(binop | terminal_expression)));

named!(
    array_type<DataType>,
    ws!(delimited!(
        tag!("["),
        map!(separated_pair!(data_type, tag!(";"), expression), |ab| {
            DataType::Array {
                data_type: Box::new(ab.0),
                length: ab.1,
            }
        }),
        tag!("]")
    ))
);

named!(
    data_type<DataType>,
    ws!(alt!(
        complete!(array_type)
            | complete!(choose_type)
            | complete!(message_type)
            | complete!(many_combinator_type)
            | complete!(rest_combinator_type)
            | map!(complete!(symbol), |s| DataType::Value(s.to_string()))
    ))
);

named!(
    assign_expression<Expression>,
    ws!(do_parse!(_equals: tag!("=") >> value: expression >> (value))));

named!(
    constraints<Vec<Expression>>,
    ws!(do_parse!(
      tag!("|") >>
      tag!("[") >>
      cs: separated_nonempty_list!(tag!(","), expression) >>
      tag!("]") >> ( cs ))));

named!(
    assign_value<Value>,
    ws!(do_parse!(_equals: tag!("=") >> value: value >> (value))));

named!(
    field<Field>,
    ws!(do_parse!(
        many0!(br) >>
        public: opt!(tag!("public"))
            >> variable: opt!(tag!("@"))
            >> name: symbol
            >> _colon: tag!(":")
            >> apply_to: opt!(complete!(apply))
            >> data_type: data_type
            >> value: opt!(assign_expression)
            >> constraints: opt!(constraints)
            >> _semicolon: tag!(";") >> (Field {
            public: public.is_some(),
            variable: variable.is_some(),
            name: name.trim().to_string(),
            apply_to,
            data_type,
            value,
            constraints: constraints,
        })
    ))
);

named!(
    args<Vec<Arg>>,
    ws!(delimited!(
        tag!("("),
        separated_list!(
            tag!(","),
            do_parse!(
                    public: opt!(tag!("public"))
                    >> _sigil: tag!("$")
                    >> name: symbol
                    >> _colon: tag!(":")
                    >> data_type: data_type
                    >> value: opt!(assign_value) >> (Arg {
                    public: public.is_some(),
                    name: name.to_string(),
                    data_type,
                    value,
                })
            )
        ),
        tag!(")")
    ))
);

named!(pub message<Message>,
    do_parse!(
      many0!(br) >>
      name: symbol >>
      many0!(br) >>
      args: opt!(complete!(args)) >>
      many0!(br) >>
      _eq: tag!("=") >>
      many0!(br) >>
      fields: delimited!(tag!("{"), ws!(many0!(complete!(field))), tag!("}")) >>
      (
        Message {
            name: name.trim().to_string(),
            args: args.unwrap_or(vec![]),
            fields,
        }
      )));

named!(pub source_file<Vec<Message>>,
    do_parse!(
        many0!(br) >>
        messages: separated_list!(complete!(many1!(br)), complete!(message)) >>
        opt!(complete!(many0!(br))) >>
        (
             messages
        )));
