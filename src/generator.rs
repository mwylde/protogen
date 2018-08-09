use parser::{DataType, Field, Message, source_file};
use std::fmt;
use std::collections::HashMap;

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Expression;
    use parser::Value;
    use parser::ChooseVariant;

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
        assert_eq!("Vec<u8>", t("", &DataType::Array {
                data_type: Box::new(DataType::Value("u8".to_string())),
                length: Expression::Value(Value::Number(8)),
            }));

        assert_eq!("HciCommand", t("", &DataType::Message {
            name: "hci_command".to_string(),
            args: vec![],
        }));

        assert_eq!("Vec<u8>", t("", &DataType::Apply {
            source: "data".to_string(),
            data_type: Box::new(DataType::Array {
                data_type: Box::new(DataType::Value("u8".to_string())),
                length: Expression::Value(Value::Number(8)),
            })
        }));

        assert_eq!("Vec<Vec<HciCommand>>", t("", &DataType::Array {
            length: Expression::Value(Value::Number(8)),
            data_type: Box::new(DataType::Array {
                length: Expression::Value(Value::Number(8)),
                data_type: Box::new(DataType::Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                })
            })
        }));

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
                data_type: DataType::Choose(vec![
                    ChooseVariant {
                        name: "Something".to_string(),
                        data_type: DataType::Value("String".to_string()),
                    }
                ])
            }
        ]);

        let mut enums = vec![];
        assert_eq!("HciMessage_Message", Generator::render_data_type(
            "HciMessage_Message", &mut enums, &dt));

        let expected = vec![
            Enum {
                name: "HciMessage_Message_HciData".to_string(),
                variants: vec![
                    EnumVariant {
                        name: "Something".to_string(),
                        data_type: "String".to_string()
                    }
                ]
            },
            Enum {
                name: "HciMessage_Message".to_string(),
                variants: vec![
                    EnumVariant {
                        name: "HciCommand".to_string(),
                        data_type: "Vec<u8>".to_string()
                    },
                    EnumVariant {
                        name: "HciData".to_string(),
                        data_type: "HciMessage_Message_HciData".to_string()
                    }
                ]
            }
        ];

        assert_eq!(expected, enums);
    }

    #[test]
    fn test_render_struct() {
        let expected = r#"
pub struct HciCommand {
    ocf: u8,
    length: u8,
    message: HciCommand_Message,
}

pub enum HciCommand_Message {
    SomeMessage(Vec<u8>),
}
"#;

        assert_eq!(expected.trim(), Generator::from_messages(vec![Message {
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
                },
                Field {
                    public: false,
                    variable: false,
                    name: "data".to_string(),
                    data_type: DataType::Value("u8".to_string()),
                    value: Some(Value::Number(10)),
                },
                Field {
                    public: false,
                    variable: false,
                    name: "message".to_string(),
                    data_type: DataType::Choose(vec![
                        ChooseVariant {
                            name: "SomeMessage".to_string(),
                            data_type: DataType::Array {
                                data_type: Box::new(DataType::Value("u8".to_string())),
                                length: Expression::Value(Value::Number(8)),
                            },
                        }]),
                    value: None,
                }
            ] }]).unwrap().to_string().trim());
    }

    #[test]
    fn test_struct_field() {
       assert_eq!("length: u8",
                  format!("{}", StructField { name: "length".to_string(), data_type: "u8".to_string() }));
    }

    #[test]
    fn test_struct() {
        let s = Struct {
            name: "HciCommand".to_string(),
            fields: vec![
                StructField { name: "length".to_string(), data_type: "u8".to_string() },
                StructField { name: "name".to_string(), data_type: "String".to_string()},
            ]
        };

        let expected = r#"
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
                EnumVariant { name: "ClearAllFilter".to_string(), data_type: "ClearAllFilter".to_string()},
                EnumVariant { name: "InquiryResult".to_string(), data_type: "String".to_string()},
            ]
        };

        let expected = r#"
pub enum SetEventFilter_Filter {
    ClearAllFilter(ClearAllFilter),
    InquiryResult(String),
}"#;

        assert_eq!(expected.trim(), format!("{}", e));
    }

    #[test]
    fn test_end_to_end() {
        let source: &str = include_str!("../examples/hci_message.pg");
        let messages = source_file(source.trim().as_bytes()).unwrap().1;

        let generator = Generator::from_messages(messages).unwrap();
        println!("{}", generator);
    }

}

//pub fn generate(message: Message) {
//    let tokens = quote! {
//         struct #message_name {
//
//         }
//    };
//}

pub fn to_camel_case(s: &str, initial_cap: bool) -> String {
    let mut result = String::new();

    let mut prev: Option<char> = None;

    for c in s.chars() {
        if c == '_' {
            // skip
        } else if (prev.is_none() && initial_cap)
            || prev.iter().any(|c| *c == '_') {
            result.push_str(&c.to_uppercase().to_string());
        } else {
            result.push(c);
        }
        prev = Some(c);
    }

    result
}


#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
struct StructField {
    name: String,
    data_type: String,
}

impl fmt::Display for StructField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.data_type)
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
struct Struct {
    name: String,
    fields: Vec<StructField>
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "pub struct {} {{\n", self.name)?;

        for field in &self.fields {
            write!(f, "    {},\n", field)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
struct EnumVariant {
    name: String,
    data_type: String,
}

impl fmt::Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.name, self.data_type)
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
struct Enum {
    name: String,
    variants: Vec<EnumVariant>
}

impl fmt::Display for Enum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "pub enum {} {{\n", self.name)?;

        for field in &self.variants {
            write!(f, "    {},\n", field)?;
        }

        write!(f, "}}")
    }
}

pub struct Generator {
    messages: Vec<Message>,
    structs: HashMap<String, Struct>,
    enums: HashMap<String, Enum>,
}

impl Generator {
    fn render_data_type(prefix: &str, enums: &mut Vec<Enum>, data_type: &DataType) -> String {
        match data_type {
            DataType::Value(ref s) => s.clone(),
            DataType::Array { ref data_type, .. } => {
                format!("Vec<{}>", Generator::render_data_type(prefix, enums, &*data_type))
            },
            DataType::Message { ref name, .. } => to_camel_case(name, true),
            DataType::Apply { ref data_type, .. } => {
                Generator::render_data_type(prefix, enums, &*data_type)
            },
            DataType::Choose(variants) => {
                let e = Enum {
                    name: prefix.to_string(),
                    variants: variants.iter().map(|v| {
                        EnumVariant {
                            name: v.name.clone(),
                            data_type: Generator::render_data_type(
                                &[prefix, &to_camel_case(&v.name, true)].join("_"),
                                enums,
                                &v.data_type),
                        }
                    }).collect(),
                };

                enums.push(e);
                prefix.to_string()
            }
        }
    }

    pub fn from_messages(messages: Vec<Message>) -> Result<Generator, String> {
        let mut structs = HashMap::new();
        let mut enums: Vec<Enum> = vec![];
        for message in &messages {
            let mut s = Struct {
                name: to_camel_case(&message.name, true),
                fields: vec![],
            };

            for f in &message.fields {
                let prefix = &[&s.name[..], &to_camel_case(&f.name, true)].join("_");
                let data_type = Generator::render_data_type(prefix, &mut enums, &f.data_type);

                if f.value.is_none() {
                    s.fields.push(StructField {
                        name: f.name.clone(), data_type,
                    });
                }
            }

            if structs.contains_key(&s.name) {
                return Err(format!("duplicate struct type {}", s.name));
            }

            structs.insert(s.name.clone(), s);
        }

        let mut enum_map = HashMap::new();
        for e in enums {
            if enum_map.contains_key(&e.name) {
                return Err(format!("duplicate enum type {}", e.name));
            }
            enum_map.insert(e.name.clone(), e);
        }

        // TODO: validate that all referenced message types exist

        Ok(Generator { messages, structs, enums: enum_map })
    }
}
impl fmt::Display for Generator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for s in self.structs.values() {
            write!(f, "{}\n\n", s)?;
        }

        for e in self.enums.values() {
            write!(f, "{}\n\n", e)?;
        }

        Ok(())
    }
}
