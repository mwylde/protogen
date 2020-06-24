#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::backend::*;
    use crate::rust::*;

    #[test]
    fn test_to_camel_case() {
        assert_eq!("ThisIsMyWord", to_camel_case("this_is_my_word", true));
        assert_eq!("thisIsMyWord", to_camel_case("this_is_my_word", false));
    }

    #[test]
    fn test_data_type() {
        fn t(prefix: &str, dt: &DataType) -> String {
            Generator::render_data_type(prefix, &mut vec![], dt)
        }

        assert_eq!("u8", t("", &DataType::Value("u8".to_string())));
        assert_eq!(
            "Vec<u8>",
            t(
                "",
                &DataType::Array {
                    data_type: Box::new(DataType::Value("u8".to_string())),
                    length: Expression::Value(Value::Number(8)),
                }
            )
        );

        assert_eq!(
            "HciCommand",
            t(
                "",
                &DataType::Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                }
            )
        );

        assert_eq!(
            "Vec<Vec<HciCommand>>",
            t(
                "",
                &DataType::Array {
                    length: Expression::Value(Value::Number(8)),
                    data_type: Box::new(DataType::Array {
                        length: Expression::Value(Value::Number(8)),
                        data_type: Box::new(DataType::Message {
                            name: "hci_command".to_string(),
                            args: vec![],
                        }),
                    }),
                }
            )
        );

        let dt = DataType::Choose(vec![
            ChooseVariant {
                name: "HciCommand".to_string(),
                data_type: DataType::Array {
                    data_type: Box::new(DataType::Value("u8".to_string())),
                    length: Expression::Value(Value::Number(8)),
                },
            },
            ChooseVariant {
                name: "HciData".to_string(),
                data_type: DataType::Choose(vec![ChooseVariant {
                    name: "Something".to_string(),
                    data_type: DataType::Value("String".to_string()),
                }]),
            },
        ]);

        let mut enums = vec![];
        assert_eq!(
            "HciMessage_Message",
            Generator::render_data_type("HciMessage_Message", &mut enums, &dt)
        );

        let expected = vec![
            Enum {
                name: "HciMessage_Message_HciData".to_string(),
                variants: vec![EnumVariant {
                    name: "Something".to_string(),
                    data_type: "String".to_string(),
                }],
            },
            Enum {
                name: "HciMessage_Message".to_string(),
                variants: vec![
                    EnumVariant {
                        name: "HciCommand".to_string(),
                        data_type: "Vec<u8>".to_string(),
                    },
                    EnumVariant {
                        name: "HciData".to_string(),
                        data_type: "HciMessage_Message_HciData".to_string(),
                    },
                ],
            },
        ];

        assert_eq!(expected, enums);
    }

    #[test]
    #[ignore]
    fn test_render_struct() {
        let expected = r#"
use protogen::*;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct HciCommand {
    _type: u8,
    _public_arg: u8,
    _ocf: u8,
    _length: u8,
    _message: HciCommand_Message,
}

impl HciCommand {
    pub fn get_public_arg(&self) -> u8 {
        self._public_arg
    }

    pub fn get_ocf(&self) -> u8 {
        self._ocf
    }

    pub fn parse<'a>(_s0: State<'a>, _type: u8, _with_value: u16, _public_arg: u8) -> PResult<(State<'a>, HciCommand)> {
        if 0xA != _with_value {
            return Err(protogen::Error { error: protogen::ErrorType::Failure, position: _s0.offset * 8 + _s0.bit_offset });
        }
        let (_s1, _ocf) = call!(_s0, read_u8_le)?;
        let (_s2, _length) = call!(_s1, read_u8_le)?;
        let _data: u8 = (0xA) as u8;
        let (_s3, _message) = call!(_s2, choose!(
            map!(call!(count!(8, call!(read_u8_le))), |v| HciCommand_Message::SomeMessage(v))
    ))?;
        Ok((_s3, HciCommand { _type, _public_arg, _ocf, _length, _message }))
    }

    fn write_bytes(&self, buf: &mut buffer::BitBuffer) {
        buf.push_u8(self._ocf);
        buf.push_u8(self._length);
        match &self._message {
            HciCommand_Message::SomeMessage(v) => v.write_bytes(buf),
        }
    }

    pub fn to_vec(&self) -> Vec<u8> {
        let mut buf = buffer::BitBuffer::new();
        self.write_bytes(&mut buf);
        buf.into_vec()
    }

}


#[allow(non_camel_case_types)]
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum HciCommand_Message {
    SomeMessage(Vec<u8>),
}
"#;

        assert_eq!(
            expected.trim(),
            Generator::from_protocol(Protocol {
                messages: vec![Message {
                    name: "hci_command".to_string(),
                    args: vec![
                        Arg {
                            public: false,
                            name: "type".to_string(),
                            data_type: DataType::Value("u8".to_string()),
                            value: None
                        },
                        Arg {
                            public: false,
                            name: "with_value".to_string(),
                            data_type: DataType::Value("u16".to_string()),
                            value: Some(Value::Number(10u64)),
                        },
                        Arg {
                            public: true,
                            name: "public_arg".to_string(),
                            data_type: DataType::Value("u8".to_string()),
                            value: None,
                        }
                    ],
                    fields: vec![
                        Field {
                            public: true,
                            variable: true,
                            name: "ocf".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: None,
                            constraints: None
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
                        Field {
                            public: false,
                            variable: false,
                            name: "data".to_string(),
                            apply_to: None,
                            data_type: DataType::Value("u8".to_string()),
                            value: Some(Expression::Value(Value::Number(10))),
                            constraints: None
                        },
                        Field {
                            public: false,
                            variable: false,
                            name: "message".to_string(),
                            apply_to: None,
                            data_type: DataType::Choose(vec![ChooseVariant {
                                name: "SomeMessage".to_string(),
                                data_type: DataType::Array {
                                    data_type: Box::new(DataType::Value("u8".to_string())),
                                    length: Expression::Value(Value::Number(8)),
                                },
                            }]),
                            value: None,
                            constraints: None,
                        },
                    ],
                }]
            })
            .unwrap()
            .to_string()
            .trim()
        );
    }

    #[test]
    fn test_struct_field() {
        assert_eq!(
            "length: u8",
            format!(
                "{}",
                StructField {
                    name: "length".to_string(),
                    data_type: "u8".to_string()
                }
            )
        );
    }

    #[test]
    fn test_struct() {
        let s = Struct {
            name: "HciCommand".to_string(),
            fields: vec![
                StructField {
                    name: "length".to_string(),
                    data_type: "u8".to_string(),
                },
                StructField {
                    name: "name".to_string(),
                    data_type: "String".to_string(),
                },
            ],
        };

        let expected = r#"
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct HciCommand {
    length: u8,
    name: String,
}"#;

        assert_eq!(expected.trim(), format!("{}", s));
    }

    #[test]
    fn test_enum() {
        let e = Enum {
            name: "SetEventFilter_Filter".to_string(),
            variants: vec![
                EnumVariant {
                    name: "ClearAllFilter".to_string(),
                    data_type: "ClearAllFilter".to_string(),
                },
                EnumVariant {
                    name: "InquiryResult".to_string(),
                    data_type: "String".to_string(),
                },
            ],
        };

        let expected = r#"
#[allow(non_camel_case_types)]
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum SetEventFilter_Filter {
    ClearAllFilter(ClearAllFilter),
    InquiryResult(String),
}"#;

        assert_eq!(expected.trim(), format!("{}", e));
    }
}
