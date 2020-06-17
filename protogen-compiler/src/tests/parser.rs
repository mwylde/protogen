#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::parser::grammar::*;

    #[test]
    fn simple() {
        let text = " hci_command =  { }";

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![],
            })
        );
    }

    #[test]
    fn single_field() {
        let text = " hci_command =  { public @ocf : u8; }";

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
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
            })
        );
    }

    #[test]
    fn multiple_fields() {
        let text = r#"
hci_command = {
  public @ocf: u8;
  length: u8;
}"#;

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
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
            })
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
            MessageParser::new().parse(text),
            Ok(Message {
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
            })
        );
    }

    #[test]
    fn message_args() {
        let text = " hci_command ($type: u8, public $name: String) = { }";

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
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
            })
        );
    }

    #[test]
    fn message_args_with_values() {
        let text = " hci_command ($type: u8 = 0xFF, $name: String = \"hello world!\") = { }";

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
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
            })
        );
    }

    #[test]
    fn field_hex_value() {
        let text = " hci_command =  { public @ocf : u8 = 0x22; }";

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
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
            })
        );
    }

    #[test]
    fn other_combinators() {
        let text = " hci_command = {
        public @rest: rest!();
        many: many!(u8);
        }";

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "rest".to_string(),
                        apply_to: None,
                        data_type: DataType::RestCombinator,
                        value: None,
                        constraints: None,
                    },
                    Field {
                        public: false,
                        variable: false,
                        name: "many".to_string(),
                        apply_to: None,
                        data_type: DataType::ManyCombinator {
                            data_type: Box::new(DataType::Value("u8".to_string())),
                        },
                        value: None,
                        constraints: None,
                    }
                ],
            })
        );
    }

    #[test]
    fn field_constraints() {
        let text = " hci_command =  { \
                    header: [u8; 3] | [b\"wtf\"]; \
                    public @ocf : u8 | [0x22 + 10, 5]; }";

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
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
                            "wtf".as_bytes().to_vec()
                        ))]),
                    },
                    Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        apply_to: None,
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                        constraints: Some(vec![
                            Expression::Binary(
                                BinOp::Plus,
                                Box::new(ex_num(0x22)),
                                Box::new(ex_num(10))
                            ),
                            ex_num(5)
                        ]),
                    }
                ],
            })
        );
    }

    #[test]
    fn field_string_value() {
        let text = " hci_command =  { public @name : String = \"hello world!\" ; }";

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
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
            })
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(NumberParser::new().parse("0x10"), Ok(0x10));
        assert_eq!(NumberParser::new().parse("0X5235"), Ok(0x5235));
        assert_eq!(NumberParser::new().parse("1241"), Ok(1241));
        assert_eq!(NumberParser::new().parse("0b100000010011001"), Ok(16537));
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
            MessageParser::new().parse(text),
            Ok(Message {
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
                        value: Some(Expression::Value(Value::String(
                            "this is a string.".to_string()
                        ))),
                        constraints: None,
                    },
                ],
            })
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
            MessageParser::new().parse(text),
            Ok(Message {
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
                                        Expression::Variable("$name".to_string())
                                    ],
                                },
                            },
                        ]),
                        value: None,
                        constraints: None,
                    },
                ],
            })
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
            MessageParser::new().parse(text),
            Ok(Message {
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
                            args: vec![Expression::Variable("@len".to_string())]
                        },
                        value: None,
                        constraints: None,
                    }
                ]
            })
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
            MessageParser::new().parse(text),
            Ok(Message {
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
                            length: Expression::Binary(
                                BinOp::Divide,
                                Box::new(ex_var("@length")),
                                Box::new(ex_num(2))
                            )
                        },
                        value: None,
                        constraints: None,
                    },
                ],
            })
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
            MessageParser::new().parse(text),
            Ok(Message {
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
            })
        );
    }

    #[test]
    fn message_type() {
        let text = r#"
inquiry_result = {
  public condition: filter_condition();
}"#;

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
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
            })
        );
    }

    #[test]
    fn string_parser() {
        assert_eq!(
            ValueParser::new().parse("\"hello world\""),
            Ok(Value::String("hello world".to_string()))
        )
    }

    #[test]
    fn test_messages() {
        let text = r#"
        hci_command = { }
        hci_message = {}
        "#;

        assert_eq!(
            ProtocolParser::new().parse(text),
            Ok(Protocol {
                messages: vec![
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
            })
        );
    }

    #[test]
    fn expressions() {
        let text = "@var * 5";
        assert_eq!(
            ExpressionParser::new().parse(text),
            Ok(Expression::Binary(
                BinOp::Multiply,
                Box::new(ex_var("@var")),
                Box::new(ex_num(5))
            ))
        );

        let text = "(@var * 5)";
        assert_eq!(
            ExpressionParser::new().parse(text),
            Ok(Expression::Binary(
                BinOp::Multiply,
                Box::new(ex_var("@var")),
                Box::new(ex_num(5))
            ))
        );

        fn var_minus_five() -> Box<Expression> {
            Box::new(Expression::Binary(
                BinOp::Minus,
                Box::new(ex_var("@var")),
                Box::new(ex_num(5)),
            ))
        }

        let text = "(@var - 5) * 6";
        assert_eq!(
            ExpressionParser::new().parse(text),
            Ok(Expression::Binary(
                BinOp::Multiply,
                var_minus_five(),
                Box::new(ex_num(6))
            ))
        );

        let text = "6 - @var * 5";
        assert_eq!(
            ExpressionParser::new().parse(text),
            Ok(Expression::Binary(
                BinOp::Minus,
                Box::new(ex_num(6)),
                Box::new(Expression::Binary(
                    BinOp::Multiply,
                    Box::new(ex_var("@var")),
                    Box::new(ex_num(5))
                ))
            ))
        );
    }

    #[test]
    fn expression_value() {
        let text = r#"
hci_command = {
  @opcode: u16;
  public ocf: u8 = @opcode + 5;
}"#;

        assert_eq!(
            MessageParser::new().parse(text),
            Ok(Message {
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
                        value: Some(Expression::Binary(
                            BinOp::Plus,
                            Box::new(Expression::Variable("@opcode".to_string())),
                            Box::new(Expression::Value(Value::Number(5)))
                        )),
                        constraints: None,
                    },
                ],
            })
        );
    }
}
