use parser::{DataType, Message};
use std::collections::HashMap;
use std::fmt;
use parser::Expression;
use parser::Value;
use parser::Field;
use std::collections::HashSet;

#[cfg(test)]
mod tests {
    use super::*;
    use parser::{source_file, ChooseVariant, Expression, Field, Value};

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
                    length: Expression::Number(8),
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
                    length: Expression::Number(8),
                    data_type: Box::new(DataType::Array {
                        length: Expression::Number(8),
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
                    length: Expression::Number(8),
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

    // #[test]
    #[allow(dead_code)]
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

        assert_eq!(
            expected.trim(),
            Generator::from_messages(vec![Message {
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
                    },
                    Field {
                        public: false,
                        variable: false,
                        name: "length".to_string(),
                        apply_to: None,
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                    },
                    Field {
                        public: false,
                        variable: false,
                        name: "data".to_string(),
                        apply_to: None,
                        data_type: DataType::Value("u8".to_string()),
                        value: Some(Value::Number(10)),
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
                                length: Expression::Number(8),
                            },
                        }]),
                        value: None,
                    },
                ],
            }]).unwrap()
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
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
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
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum SetEventFilter_Filter {
    ClearAllFilter(ClearAllFilter),
    InquiryResult(String),
}"#;

        assert_eq!(expected.trim(), format!("{}", e));
    }

    #[test]
    fn test_end_to_end() {
        let source: &str = include_str!("../../protogen-examples/src/hci_message.protogen");
        let messages = source_file(source.trim().as_bytes()).unwrap().1;

        let generator = Generator::from_messages(messages).unwrap();
        println!("{}", generator);
    }

}

pub fn to_camel_case(s: &str, initial_cap: bool) -> String {
    let mut result = String::new();

    let mut prev: Option<char> = None;

    for c in s.chars() {
        if c == '_' {
            // skip
        } else if (prev.is_none() && initial_cap) || prev.iter().any(|c| *c == '_') {
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
    fields: Vec<StructField>,
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]\n")?;
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
    variants: Vec<EnumVariant>,
}

impl fmt::Display for Enum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#[allow(non_camel_case_types)]\n")?;
        write!(f, "#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]\n")?;
        write!(f, "pub enum {} {{\n", self.name)?;

        for field in &self.variants {
            write!(f, "    {},\n", field)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
struct Function {
    name: String,
    public: bool,
    args: Vec<String>,
    return_type: Option<String>,
    body: Vec<String>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}fn {}({})",
            if self.public { "pub " } else { "" },
            self.name,
            self.args.join(", ")
        )?;

        if let Some(t) = &self.return_type {
            write!(f, " -> {}", t)?;
        }

        write!(f, " {{\n")?;

        for l in &self.body {
            write!(f, "    {}\n", l)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
struct Impl {
    struct_name: String,
    trait_name: Option<String>,
    functions: Vec<Function>,
}

impl fmt::Display for Impl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "impl ")?;
        if let Some(t) = &self.trait_name {
            write!(f, "{} for ", t)?;
        }

        write!(f, "{} {{\n", self.struct_name)?;

        for function in &self.functions {
            let s = format!("{}", function);
            for l in s.split("\n") {
                write!(f, "    {}\n", l)?;
            }
            write!(f, "\n")?;
        }

        write!(f, "}}")
    }
}

pub struct Generator {
    messages: Vec<Message>,
    imports: HashSet<String>,
    structs: HashMap<String, Struct>,
    impls: HashMap<String, Vec<Impl>>,
    enums: Vec<Enum>,
}

impl Generator {
    fn render_data_type(prefix: &str, enums: &mut Vec<Enum>, data_type: &DataType) -> String {
        match data_type {
            DataType::Value(ref s) => s.clone(),
            DataType::Array { ref data_type, .. } => format!(
                "Vec<{}>",
                Generator::render_data_type(prefix, enums, &*data_type)
            ),
            DataType::Message { ref name, .. } => to_camel_case(name, true),
            DataType::Choose(variants) => {
                let e = Enum {
                    name: prefix.to_string(),
                    variants: variants
                        .iter()
                        .map(|v| EnumVariant {
                            name: v.name.clone(),
                            data_type: Generator::render_data_type(
                                &[prefix, &to_camel_case(&v.name, true)].join("_"),
                                enums,
                                &v.data_type,
                            ),
                        })
                        .collect(),
                };

                enums.push(e);
                prefix.to_string()
            }
        }
    }

    fn parser_for_data_type(prefix: &str, data_type: &DataType) -> Result<String, String> {
        Ok(match data_type {
            DataType::Value(ref s) => {
                match s.as_ref() {
                    "u8"  => "le_u8".to_string(),
                    "u16" => "le_u16".to_string(),
                    "u32" => "le_u32".to_string(),
                    "u64" => "le_u64".to_string(),
                    "i8"  => "le_i8".to_string(),
                    "i16" => "le_i16".to_string(),
                    "i32" => "le_i32".to_string(),
                    "i64" => "le_i64".to_string(),
                    t => return Err(format!("Unknown type {} in {}", t, prefix)),
                }
            },
            DataType::Message { ref name, ref args } => {
                let fun = to_camel_case(name, true);
                if args.is_empty() {
                    format!("{}::parse", fun)
                } else {
                    let args: Vec<String> = args.iter().map(|arg| {
                        match arg {
                            Expression::Variable(v) => format!("_{}", &v[1..]),
                            Expression::Number(n) => format!("{:X}", n),
                        }
                    }).collect();
                    format!("call!({}::parse, {})", fun, args.join(", "))
                }
            },
            DataType::Choose(ref variants) => {
                let vs: Vec<String> = variants.iter().map(|v| {
                    format!("        {} => {{|v| {}::{}(v)}}",
                            Generator::parser_for_data_type(
                                &[prefix, &to_camel_case(&v.name, true)].join("_"),
                                &v.data_type).unwrap(),
                        prefix, v.name)
                }).collect();

                format!("alt!(\n{}\n)", vs.join(" |\n"))
            },
            DataType::Array { ref data_type, ref length } => {
                let subparser = Generator::parser_for_data_type(prefix, data_type)?;

                let l = match length {
                    Expression::Number(n) => {
                        format!("{}", n)
                    },
                    Expression::Variable(v) => {
                        format!("_{} as usize", &v[1..])
                    }
                };

                format!("count!({}, {})", subparser, l)
            }
        })
    }

    fn arg_type(data_type: &DataType) -> Result<String, String> {
        Ok(match data_type {
            DataType::Value(ref v) => v.clone(),
            DataType::Array { data_type, ..} => {
                format!("Vec<{}>", Generator::arg_type(&*data_type)?)
            }
            _ => {
                return Err(format!("Data type {:?} is not supported as an argument", data_type))
            }
        })
    }

    fn render_value(value: &Value) -> String {
        match value {
            Value::String(s) => format!(r#""{}""#, s),
            Value::Number(n) => format!("0x{:X}", n),
        }
    }

    fn parse_fn(message: &Message) -> Result<Function, String> {
        let mut fun = Function {
            name: "parse".to_string(),
            public: true,
            args: vec!["_i0: &[u8]".to_string()],
            return_type: Some(format!("IResult<&[u8], {}>", to_camel_case(&message.name, true))),
            body: vec![],
        };

        // add arguments to the function
        for arg in &message.args {
            fun.args.push(format!("_{}: {}", arg.name, Generator::arg_type(&arg.data_type)?));

            // if the argument has a value, we also need to add predicates at the beginning to check
            // the value matches
            if let Some(v) = &arg.value {
                fun.body.push(
                    format!("if _{} != {} {{
        return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));\n    }}",
                      arg.name, Generator::render_value(v)));
            }
        }

        let message_type = to_camel_case(&message.name, true);

        let mut io: HashMap<&str, (String, String, &Field)> = HashMap::new();

        let mut input_idx = 0;
        let mut output_idx = 1;
        for f in &message.fields {
            let v = if let Some(ref target) = f.apply_to {
                let (i, _, tf) = io.get(&target[1..]).ok_or(format!(
                    "Could not find stream {} for {} in {}", target, f.name, message.name))?;

                if let DataType::Array { data_type, length } = &tf.data_type {
                    if let DataType::Value(ref v) = **data_type {
                       if v == "u8" {
                           let l = match length {
                               Expression::Number(n) => n.to_string(),
                               Expression::Variable(s) => format!("_{}", &s[1..]),
                           };

                           (format!("&{}[..{} as usize]", i, l), "_".to_string(), f)
                       } else {
                           return Err(format!("Stream source {} for {} in {} is not a byte array",
                                              target, f.name, message.name));
                       }
                    } else {
                        return Err(format!("Stream source {} for {} in {} is not a byte array",
                                           target, f.name, message.name));
                    }
                } else {
                    return Err(format!("Stream source {} for {} in {} is not a byte array",
                                       target, f.name, message.name));
                }
            } else {
                let v = (format!("_i{}", input_idx), format!("_i{}", output_idx), f);
                input_idx += 1;
                output_idx += 1;
                v
            };

            if io.insert(&f.name[..], v).is_some() {
                return Err(format!("duplicate field {} in {}", f.name, message.name));
            }
        };

        let mut final_output = "_i0";
        for f in &message.fields {
            let prefix = [&message_type[..], &to_camel_case(&f.name, true)].join("_");

            let (input, output, _) = io.get(&f.name[..])
                .expect("missing i/o info for field");

            if let Some(_) = &f.value {
                unimplemented!("parser values have not been implemented yet");
            } else {
                fun.body.push(format!("let ({}, _{}) = try_parse!({}, {});", output, f.name,
                                      input, Generator::parser_for_data_type(&prefix, &f.data_type)?));
            }

            if output != "_" {
                final_output = output;
            }
        }

        let construct_args: Vec<String> = message.fields.iter().filter(|f| f.value.is_none())
            .map(|f| format!("_{}", f.name)).collect();

        fun.body.push(format!("Ok(({}, {} {{ {} }}))", final_output, message_type,
                              construct_args.join(", ")));

        Ok(fun)
    }

    pub fn from_messages(messages: Vec<Message>) -> Result<Generator, String> {
        let mut structs = HashMap::new();
        let mut enums: Vec<Enum> = vec![];
        let mut impls: HashMap<String, Vec<Impl>> = HashMap::new();
        let imports: HashSet<String> = ["nom".to_string(), "nom::*".to_string()]
            .iter().cloned().collect();

        for message in &messages {
            let mut s = Struct {
                name: to_camel_case(&message.name, true),
                fields: vec![],
            };

            let mut imp = Impl {
                struct_name: s.name.clone(),
                trait_name: None,
                functions: vec![],
            };

            for f in &message.fields {
                let prefix = &[&s.name[..], &to_camel_case(&f.name, true)].join("_");
                let data_type = Generator::render_data_type(prefix, &mut enums, &f.data_type);

                let mut getter = Function {
                    name: format!("get_{}", f.name),
                    public: true,
                    args: vec![],
                    // TODO: if this is a copy type, don't use a reference
                    return_type: Some(format!("&{}", data_type)),
                    body: vec![],
                };

                if let Some(v) = &f.value {
                    getter.body.push(format!("&{:?}", v));
                } else {
                    s.fields.push(StructField {
                        name: format!("_{}", f.name),
                        data_type,
                    });
                    getter.args.push("&self".to_string());
                    getter.body.push(format!("&self._{}", f.name));
                }

                if f.public {
                    imp.functions.push(getter);
                }
            }


            imp.functions.push(Generator::parse_fn( message)?);

            if structs.contains_key(&s.name) {
                return Err(format!("duplicate struct type {}", s.name));
            }

            impls.entry(s.name.clone()).or_insert_with(|| vec![])
                .push(imp);
            structs.insert(s.name.clone(), s);
        }

        // check for duplicate enums
        {
            let mut enum_map = HashMap::new();
            for e in &enums {
                if enum_map.contains_key(&e.name) {
                    return Err(format!("duplicate enum type {}", e.name));
                }
                enum_map.insert(e.name.clone(), e);
            }
        }

        Ok(Generator {
            messages,
            imports,
            structs,
            enums,
            impls,
        })
    }
}

impl fmt::Display for Generator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for import in &self.imports {
            write!(f, "use {};\n", import)?;
        }

        write!(f, "\n")?;

        for message in &self.messages {
            let name = to_camel_case(&message.name, true);
            // write the struct definition
            write!(f, "{}\n\n", self.structs.get(&name).expect("missing struct"))?;

            // write any associated impls
            for imp in self.impls.get(&name).unwrap_or(&vec![]) {
                write!(f, "{}\n\n", imp)?;
            }

            write!(f, "\n")?;
        }

        for e in &self.enums {
            // write the enum definition
            write!(f, "{}\n\n", e)?;

            // write any associated impls
            for imp in self.impls.get(&e.name).unwrap_or(&vec![]) {
                write!(f, "{}\n\n", imp)?;
            }

            write!(f, "\n")?;
        }

        Ok(())
    }
}
