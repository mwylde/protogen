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
    use parser::{ChooseVariant, Expression, Field, Value};
    use parser::Arg;

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
    fn test_render_struct() {
        let expected = r#"
use nom;
use nom::*;

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

    pub fn parse<'a>(_i0: &'a [u8], _type: u8, _with_value: u16, _public_arg: u8) -> IResult<&'a [u8], HciCommand> {
        if 0xA != _with_value {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        let (_i1, _ocf) = try_parse!(_i0, le_u8);
        let (_i2, _length) = try_parse!(_i1, le_u8);
        let _data: u8 = (0xA) as u8;
        let (_i3, _message) = try_parse!(_i2, alt!(
            count!(le_u8, 8) => {|v| HciCommand_Message::SomeMessage(v)}
    ));
        Ok((_i3, HciCommand { _type, _public_arg, _ocf, _length, _message }))
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
            Generator::from_messages(vec![Message {
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
//
//    #[test]
//    fn test_end_to_end() {
//        let source: &str = include_str!(hci_message.protogen);
//        let messages = source_file(source.trim().as_bytes()).unwrap().1;
//
//        let generator = Generator::from_messages(messages).unwrap();
//        println!("{}", generator);
//    }

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
        write!(f, "#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]\n")?;
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
        write!(f, "#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]\n")?;
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
    generics: Vec<String>,
    args: Vec<String>,
    return_type: Option<String>,
    body: Vec<String>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}fn {}{}({})",
            if self.public { "pub " } else { "" },
            self.name,
            if !self.generics.is_empty() { format!("<{}>", self.generics.join(", "))}
                else { String::new() },
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
    helpers: HashMap<String, Function>,
    messages: Vec<Message>,
    imports: HashSet<String>,
    structs: HashMap<String, Struct>,
    impls: HashMap<String, Vec<Impl>>,
    enums: Vec<Enum>,
}

impl Generator {
    fn render_data_type(prefix: &str, enums: &mut Vec<Enum>, data_type: &DataType) -> String {
        match data_type {
            DataType::Value(v) if v == "cstring" => "String".to_string(),
            DataType::Value(v)  => v.clone(),
            DataType::Array { ref data_type, .. } => format!(
                "Vec<{}>",
                Generator::render_data_type(prefix, enums, &*data_type)
            ),
            DataType::Message { ref name, ..} if name == "str_utf8" => "String".to_string(),
            DataType::Message { ref name, .. } => to_camel_case(name, true),
            DataType::ManyCombinator { ref data_type } => {
                format!("Vec<{}>", Generator::render_data_type(prefix, enums, &*data_type))
            }
            DataType::RestCombinator => "Vec<u8>".to_string(),
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

    fn render_expression(variable_context: &str, ex: &Expression) -> String {
        match ex {
            Expression::Value(Value::String(s)) => format!("\"{}\"", s),
            Expression::Value(Value::ByteArray(ba)) => {
                let elements: Vec<String> = ba.iter().map(|b| format!("{}u8", b)).collect();
                format!("&[{}][..]", elements.join(", "))
            }
            Expression::Value(Value::Number(n)) => format!("0x{:X}", n),
            Expression::Variable(v) => format!("{}_{}", variable_context, &v[1..]),
            Expression::Binop(op, lh, rh) => format!("({} {} {})",
                Generator::render_expression(variable_context, &*lh),
                op,
                Generator::render_expression(variable_context, &*rh)),
        }
    }

    fn parser_for_data_type(prefix: &str, field_types: &HashMap<String, DataType>,
                            data_type: &DataType) -> Result<String, String> {
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
                    "cstring" => "map_res!(take_until!(\"\\0\"), |v: &[u8]| String::from_utf8(v.to_owned()))".to_string(),
                    t => return Err(format!("Unknown type {} in {}", t, prefix)),
                }
            },
            DataType::Message { ref name, ref args} if name == "str_utf8" => {
                if args.len() != 1 {
                    return Err(format!("Expected one argument to str_utf8, found {}", args.len()));
                }

                format!("map_res!(take!({}), |v: &[u8]| String::from_utf8(v.to_owned()))",
                        Generator::render_expression("", &args.get(0).unwrap()))
            },
            DataType::Message { ref name, ref args } => {
                let fun = to_camel_case(name, true);
                if args.is_empty() {
                    format!("{}::parse", fun)
                } else {
                    let args: Vec<String> = args.iter().map(|e| {
                        let expr = Generator::render_expression("", e);
                        // TODO: this is super hacky
                        let var_type = field_types.get(&expr[1..]);
                        let is_ref = var_type.map(|dt| Generator::use_ref(dt)).unwrap_or(false);
                        if is_ref {
                            format!("&{}", expr)
                        } else {
                            expr
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
                                field_types,
                                &v.data_type).unwrap(),
                        prefix, v.name)
                }).collect();

                format!("alt!(\n{}\n)", vs.join(" |\n"))
            },
            DataType::Array { ref data_type, ref length } => {
                let subparser = Generator::parser_for_data_type(prefix, field_types,data_type)?;

                let l = match length {
                    Expression::Value(Value::String(_)) => {
                        return Err("Strings cannot be array lengths".to_string());
                    },
                    Expression::Value(Value::ByteArray(_)) => {
                        return Err("Byte arrays cannot be array lengths".to_string());
                    }
                    Expression::Value(Value::Number(n)) => {
                        format!("{}", n)
                    },
                    Expression::Variable(v) => {
                        format!("_{} as usize", &v[1..])
                    },
                    expr @ Expression::Binop(..) => {
                        format!("({}) as usize", Generator::render_expression("", &expr))
                    }
                };

                format!("count!({}, {})", subparser, l)
            }
            DataType::ManyCombinator { ref data_type } => {
                let subparser = Generator::parser_for_data_type(prefix, field_types, data_type)?;
                format!("many0!(complete!({}))", subparser)
            }
            DataType::RestCombinator => {
                "rest".to_string()
            }
        })
    }

    fn use_ref(data_type: &DataType) -> bool {
        match data_type {
            DataType::Value(ref v) => {
                match v.as_ref() {
                    "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" => false,
                    _ => true
                }
            }
            _ => true
        }
    }

    fn arg_type(data_type: &DataType) -> Result<String, String> {
        Ok(match data_type {
            DataType::Value(ref v) => v.clone(),
            DataType::Array { data_type, ..} => {
                format!("&[{}]", Generator::arg_type(&*data_type)?)
            }
            _ => {
                return Err(format!("Data type {:?} is not supported as an argument", data_type))
            }
        })
    }

    fn add_helper(data_type: &DataType, helpers: &mut HashMap<String, Function>) {
        match data_type {
            DataType::RestCombinator => {
                if !helpers.contains_key("rest") {
                    helpers.insert("rest".to_string(), Function {
                        name: "rest".to_string(),
                        public: false,
                        generics: vec![],
                        args: vec!["i: &[u8]".to_string()],
                        return_type: Some("IResult<&[u8], Vec<u8>>".to_string()),
                        body: vec![
                            "Ok((&[][..], i.to_vec()))".to_string()
                        ]
                    });
                }
            }
            _ => {}
        }
    }

    fn render_value(value: &Value) -> String {
        match value {
            Value::String(s) => format!(r#""{}""#, s),
            Value::ByteArray(b) => {
                let v: Vec<String> = b.iter().map(|b|
                    format!("{}u8", b)).collect();
                format!("vec![{}]", v.join(", "))
            },
            Value::Number(n) => format!("0x{:X}", n),
        }
    }

    fn parse_fn(message: &Message,
                helpers: &mut HashMap<String, Function>,
                imports: &mut HashSet<String>) -> Result<Function, String> {
        let mut fun = Function {
            name: "parse".to_string(),
            public: true,
            generics: vec!["'a".to_string()],
            args: vec!["_i0: &'a [u8]".to_string()],
            return_type: Some(format!("IResult<&'a [u8], {}>", to_camel_case(&message.name, true))),
            body: vec![],
        };

        let mut field_types: HashMap<String, DataType> = HashMap::new();

        // add arguments to the function
        for arg in &message.args {
            fun.args.push(format!("_{}: {}", arg.name, Generator::arg_type(&arg.data_type)?));

            field_types.insert(arg.name.clone(), arg.data_type.clone());

            // if the argument has a value, we also need to add predicates at the beginning to check
            // the value matches
            if let Some(v) = &arg.value {
                fun.body.push(
                    format!("if {} != _{} {{
        return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));\n    }}",
                      Generator::render_value(v), arg.name));
                imports.insert("nom".to_string());
            }
        }

        let message_type = to_camel_case(&message.name, true);

        let mut io: HashMap<&str, (String, String, &Field)> = HashMap::new();

        let mut input_idx = 0;
        let mut output_idx = 1;

        for f in &message.fields {
            field_types.insert(f.name.clone(), f.data_type.clone());

            if f.value.is_some() {
                continue;
            }

            let v = if let Some(ref target) = f.apply_to {
                // TODO: support applying to arguments
                let (i, _, tf) = io.get(&target[1..]).ok_or(format!(
                    "Could not find stream {} for {} in {}", target, f.name, message.name))?;

                if let DataType::Array { data_type, length } = &tf.data_type {
                    if let DataType::Value(ref v) = **data_type {
                       if v == "u8" {
                           let l = match length {
                               Expression::Value(Value::Number(n)) => n.to_string(),
                               Expression::Variable(s) => format!("_{}", &s[1..]),
                               _ => unimplemented!()
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

            Generator::add_helper(&f.data_type, helpers);
        };

        let mut final_output = "_i0";
        for f in &message.fields {
            let prefix = [&message_type[..], &to_camel_case(&f.name, true)].join("_");

            if let Some(ex) = &f.value {
                let data_type = Generator::render_data_type(&prefix, &mut vec![], &f.data_type);
                fun.body.push(format!("let _{}: {} = ({}) as {};", f.name,
                                      data_type, Generator::render_expression("", ex), data_type));
            } else {
                let (input, output, _) = io.get(&f.name[..])
                    .expect("missing i/o info for field");

                fun.body.push(format!("let ({}, _{}) = try_parse!({}, {});", output, f.name,
                                      input, Generator::parser_for_data_type(&prefix, &field_types, &f.data_type)?));

                if let Some(ref constraints) = f.constraints {
                    let cs: Vec<String> = constraints.iter().map(|c|
                        format!("_{} == {}", f.name, Generator::render_expression("", c))
                    ).collect();

                    fun.body.push(format!("if !({}) {{
      return Err(nom::Err::Error(nom::Context::Code({}, nom::ErrorKind::Tag)));
    }}", cs.join(" || "), input));
                }

                if output != "_" {
                    final_output = output;
                }
            }
        }

        let mut construct_args: Vec<String> = message.args.iter()
            .filter(|a| a.value.is_none())
            .map(|a| {
                match a.data_type {
                    // TODO: probably need to support other stuff here
                    DataType::Array { .. } => {
                        format!("_{}: _{}.to_vec()", a.name, a.name)
                    }
                    _ => format!("_{}", a.name)
                }
            })
            .collect();

        construct_args.extend(message.fields.iter()
            .filter(|f| f.value.is_none())
            .map(|f| format!("_{}", f.name)));

        fun.body.push(format!("Ok(({}, {} {{ {} }}))", final_output, message_type,
                              construct_args.join(", ")));

        Ok(fun)
    }

    fn add_field(public: bool, name: &str, data_type: &DataType,
                 value: Option<&Expression>, st: &mut Struct, imp: &mut Impl, enums: &mut Vec<Enum>) {
        let prefix = &[&st.name[..], &to_camel_case(name, true)].join("_");
        let data_type_string = Generator::render_data_type(prefix, enums, data_type);

        let use_ref = Generator::use_ref(data_type);

        let return_type = if value.is_some() || !use_ref {
            data_type_string.to_string()
        } else {
            // TODO: this is very hacky
            if data_type_string.starts_with("Vec<") && data_type_string.ends_with(">") {
                format!("&[{}]", &data_type_string[4..data_type_string.len() - 1])
            } else {
                format!("&{}", data_type_string)
            }
        };

        let mut getter = Function {
            name: format!("get_{}", name),
            public: true,
            generics: vec![],
            args: vec!["&self".to_string()],
            return_type: Some(return_type),
            body: vec![],
        };

        if let Some(v) = value {
            getter.body.push(format!("({}) as {}",
                                     Generator::render_expression("self.", v),
                                     data_type_string));
        } else {
            st.fields.push(StructField {
                name: format!("_{}", name),
                data_type: data_type_string,
            });
            if use_ref {
                getter.body.push(format!("&self._{}", name));
            } else {
                getter.body.push(format!("self._{}", name));
            }
        }

        if public {
            imp.functions.push(getter);
        }
    }

    pub fn from_messages(messages: Vec<Message>) -> Result<Generator, String> {
        let mut helpers = HashMap::new();
        let mut structs = HashMap::new();
        let mut enums: Vec<Enum> = vec![];
        let mut impls: HashMap<String, Vec<Impl>> = HashMap::new();
        let mut imports: HashSet<String> = ["nom::*".to_string()]
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

            for arg in &message.args {
                let value = arg.value.as_ref().map(|v| Expression::Value(v.clone()));
                Generator::add_field(arg.public, &arg.name, &arg.data_type, value.as_ref(),
                                     &mut s, &mut imp, &mut enums);
            }

            for f in &message.fields {
                Generator::add_field(f.public, &f.name, &f.data_type, f.value.as_ref(),
                                     &mut s, &mut imp, &mut enums);
            }

            imp.functions.push(Generator::parse_fn(&message, &mut helpers, &mut imports)?);

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
            helpers,
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
        let mut ordered_imports: Vec<&String> = self.imports.iter().collect();
        ordered_imports.sort();
        for import in ordered_imports {
            write!(f, "use {};\n", import)?;
        }

        write!(f, "\n")?;

        for helper in &self.helpers {
            write!(f, "{}\n", helper.1)?;
        }

        if !self.helpers.is_empty() {
            write!(f, "\n")?;
        }

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
