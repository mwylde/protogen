use parser::{DataType, Field, Message, source_file};

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Expression;
    use parser::Value;

    #[test]
    fn test_to_camel_case() {
        assert_eq!("ThisIsMyWord", to_camel_case("this_is_my_word", true));
        assert_eq!("thisIsMyWord", to_camel_case("this_is_my_word", false));
    }

    #[test]
    fn test_data_type() {
        assert_eq!("u8", render_data_type(&DataType::Value("u8".to_string())));

        assert_eq!("Vec<u8>", render_data_type(&DataType::Array {
                data_type: Box::new(DataType::Value("u8".to_string())),
                length: Expression::Value(Value::Number(8)),
            }));

        assert_eq!("HciCommand", render_data_type(&DataType::Message {
            name: "hci_command".to_string(),
            args: vec![],
        }));

        assert_eq!("Vec<u8>", render_data_type(&DataType::Apply {
            source: "data".to_string(),
            data_type: Box::new(DataType::Array {
                data_type: Box::new(DataType::Value("u8".to_string())),
                length: Expression::Value(Value::Number(8)),
            })
        }));

        assert_eq!("Vec<Vec<HciCommand>>", render_data_type(&DataType::Array {
            length: Expression::Value(Value::Number(8)),
            data_type: Box::new(DataType::Array {
                length: Expression::Value(Value::Number(8)),
                data_type: Box::new(DataType::Message {
                    name: "hci_command".to_string(),
                    args: vec![],
                })
            })
        }));

//        assert_eq!("HciCommand_Command", render_data_type(&DataType::Choose(
//            vec![]
//        )));
    }

    #[test]
    fn test_field() {
        assert_eq!(Some("    data: u8,".to_string()), render_struct_field(&Field {
            public: true,
            variable: false,
            name: "data".to_string(),
            data_type: DataType::Value("u8".to_string()),
            value: None,
        }));

        assert_eq!(None, render_struct_field(&Field {
            public: true,
            variable: false,
            name: "data".to_string(),
            data_type: DataType::Value("u8".to_string()),
            value: Some(Value::Number(10)),
        }));
    }

    #[test]
    fn test_render_struct() {
        let expected = r#"
pub struct HciCommand {
    ocf: u8,
    length: u8,
}"#;

        assert_eq!(expected.trim(), render_struct(&Message {
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
                }
            ] }));
    }

    #[test]
    fn test_end_to_end() {
        let source: &str = include_str!("../examples/hci_message.pg");
        let messages = source_file(source.trim().as_bytes()).unwrap().1;

        for m in messages {
            println!("{}", render_struct(&m));
        }
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

fn render_data_type(data_type: &DataType) -> String {
    match *data_type {
        DataType::Value(ref s) => s.clone(),
        DataType::Array { ref data_type, .. } => {
            format!("Vec<{}>", render_data_type(&*data_type))
        },
        DataType::Message { ref name, .. } => to_camel_case(name, true),
        DataType::Apply { ref data_type, .. } => {
            render_data_type(&*data_type)
        },
        _ => "unsupported".to_string()
    }
}

fn render_struct_field(field: &Field) -> Option<String> {
    if field.value.is_none() {
        Some(format!("    {}: {},", field.name, render_data_type(&field.data_type)))
    } else {
        None
    }
}

fn render_struct(message: &Message) -> String {
    let fields = message.fields.iter()
        .map(render_struct_field)
        .flat_map(|s| s)
        .collect::<Vec<String>>()
        .join("\n");

    format!("pub struct {} {{\n{}\n}}",
            to_camel_case(&message.name, true),
            fields)
}
