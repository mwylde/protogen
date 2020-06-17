use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

use crate::ast::*;
use crate::rust::*;

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

fn method(target: &str, name: &str, parameters: Vec<RustExpression>) -> RustExpression {
    RustExpression::MethodCall {
        target: Box::new(var(target)),
        name: name.to_string(),
        parameters,
    }
}

fn num(d: i64) -> RustExpression {
    RustExpression::Value(RustValue::Number(d))
}

fn var(s: &str) -> RustExpression {
    RustExpression::Value(RustValue::Variable(s.to_string()))
}

fn binop(op: &str, lh: RustExpression, rh: RustExpression) -> RustExpression {
    RustExpression::BinOp {
        op: op.to_string(),
        lh: Box::new(lh),
        rh: Box::new(rh),
    }
}

pub struct Generator {
    pub messages: Vec<Message>,
    pub imports: HashSet<String>,
    pub structs: HashMap<String, Struct>,
    pub impls: HashMap<String, Vec<Impl>>,
    pub enums: Vec<Enum>,
}

impl Generator {
    pub fn render_data_type(prefix: &str, enums: &mut Vec<Enum>, data_type: &DataType) -> String {
        match data_type {
            DataType::Value(v) if v == "cstring" => "String".to_string(),
            DataType::Value(v) => {
                if let Ok((sign, _, width)) = Generator::parse_int_type(v) {
                    format!("{}{}", sign, width)
                } else {
                    v.clone()
                }
            }
            DataType::Array { ref data_type, .. } => format!(
                "Vec<{}>",
                Generator::render_data_type(prefix, enums, &*data_type)
            ),
            DataType::Message { ref name, .. } if name == "str_utf8" => "String".to_string(),
            DataType::Message { ref name, .. } => to_camel_case(name, true),
            DataType::ManyCombinator { ref data_type } => format!(
                "Vec<{}>",
                Generator::render_data_type(prefix, enums, &*data_type)
            ),
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

    fn parse_int_type(s: &str) -> Result<(String, usize, usize), String> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"([iu])(\d{1,2})").unwrap();
        }

        if let Some(captures) = RE.captures(s) {
            let size = usize::from_str(&captures[2]).unwrap();
            if size == 0 {
                return Err(format!("Bit size cannot be 0{}", size));
            }

            let width = if size <= 8 {
                8
            } else if size <= 16 {
                16
            } else if size <= 32 {
                32
            } else if size <= 64 {
                64
            } else {
                return Err(format!("Unsupported bit size {}", size));
            };

            Ok((captures[1].to_string(), size, width))
        } else {
            return Err(format!("Unknown type {}", s));
        }
    }

    fn render_value(value: &Value) -> RustExpression {
        match value {
            Value::String(s) => RustExpression::Value(RustValue::String(s.clone())),
            Value::ByteArray(ba) => {
                let elements: Vec<RustExpression> = ba
                    .iter()
                    .map(|b| RustExpression::Value(RustValue::Byte(*b)))
                    .collect();
                RustExpression::ArrayRef(Box::new(RustExpression::Array(elements)))
            }
            Value::Number(n) => RustExpression::Value(RustValue::Number(*n as i64)),
        }
    }

    fn render_expression(variable_context: &str, ex: &Expression) -> RustExpression {
        match ex {
            Expression::Value(v) => Self::render_value(v),
            Expression::Variable(v) => RustExpression::Value(RustValue::Variable(format!(
                "{}_{}",
                variable_context,
                &v[1..]
            ))),
            Expression::Binary(op, lh, rh) => RustExpression::BinOp {
                op: format!("{}", op),
                lh: Box::new(Generator::render_expression(variable_context, &*lh)),
                rh: Box::new(Generator::render_expression(variable_context, &*rh)),
            },
            Expression::Unary(UnaryOp::Len, arg) => RustExpression::MethodCall {
                target: Box::new(Generator::render_expression(variable_context, &arg)),
                name: "len".to_string(),
                parameters: vec![],
            },
            Expression::Unary(UnaryOp::Serialize, arg) => RustExpression::MethodCall {
                target: Box::new(Generator::render_expression(variable_context, &arg)),
                name: "to_bytes".to_string(),
                parameters: vec![],
            },
        }
    }

    fn parser_for_data_type(
        prefix: &str,
        field_types: &HashMap<String, DataType>,
        data_type: &DataType,
    ) -> Result<String, String> {
        Ok(match data_type {
            DataType::Value(ref s) => match s.as_ref() {
                "u8" => "read_u8_le".to_string(),
                "u16" => "read_u16_le".to_string(),
                "u32" => "read_u32_le".to_string(),
                "u64" => "read_u64_le".to_string(),
                "i8" => "read_i8_le".to_string(),
                "i16" => "read_i16_le".to_string(),
                "i32" => "read_i32_le".to_string(),
                "i64" => "read_i64_le".to_string(),
                "cstring" => "map_res!(many!(call!(not, 0)), |v| String::from_utf8(v))".to_string(),
                t => {
                    let (sign, size, width) = Generator::parse_int_type(t)?;

                    let parser = format!("call!(read_bits_u{}, {})", width, size);

                    if sign == "u" {
                        parser
                    } else {
                        format!("map!({}, |x| x as i{})", parser, width)
                    }
                }
            },
            DataType::Message { ref name, ref args } if name == "str_utf8" => {
                if args.len() != 1 {
                    return Err(format!(
                        "Expected one argument to str_utf8, found {}",
                        args.len()
                    ));
                }

                format!(
                    "map_res!(call!(read_bytes, {} as usize), |v| String::from_utf8(v))",
                    Generator::render_expression("", &args.get(0).unwrap())
                )
            }
            DataType::Message { ref name, ref args } => {
                let fun = to_camel_case(name, true);
                if args.is_empty() {
                    format!("{}::parse", fun)
                } else {
                    let args: Vec<String> = args
                        .iter()
                        .map(|e| {
                            let expr = Generator::render_expression("", e);
                            // TODO: this is super hacky
                            let var_type = field_types.get(&expr[1..]);
                            let is_ref = var_type.map(|dt| Generator::use_ref(dt)).unwrap_or(false);
                            if is_ref {
                                format!("&{}", expr)
                            } else {
                                expr
                            }
                        })
                        .collect();
                    format!("call!({}::parse, {})", fun, args.join(", "))
                }
            }
            DataType::Choose(ref variants) => {
                let vs: Vec<String> = variants
                    .iter()
                    .map(|v| {
                        format!(
                            "        map!(call!({}), |v| {}::{}(v))",
                            Generator::parser_for_data_type(
                                &[prefix, &to_camel_case(&v.name, true)].join("_"),
                                field_types,
                                &v.data_type
                            )
                            .unwrap(),
                            prefix,
                            v.name
                        )
                    })
                    .collect();

                format!("choose!(\n{}\n)", vs.join(" |\n"))
            }
            DataType::Array {
                ref data_type,
                ref length,
            } => {
                let subparser = Generator::parser_for_data_type(prefix, field_types, data_type)?;

                let l = match length {
                    Expression::Value(Value::String(_)) => {
                        return Err("Strings cannot be array lengths".to_string());
                    }
                    Expression::Value(Value::ByteArray(_)) => {
                        return Err("Byte arrays cannot be array lengths".to_string());
                    }
                    Expression::Value(Value::Number(n)) => format!("{}", n),
                    Expression::Variable(v) => format!("_{} as usize", &v[1..]),
                    expr @ Expression::Binary(..) => {
                        format!("({}) as usize", Generator::render_expression("", &expr))
                    }
                    Expression::Unary(..) => unimplemented!(),
                };

                format!("count!({}, call!({}))", l, subparser)
            }
            DataType::ManyCombinator { ref data_type } => {
                let subparser = Generator::parser_for_data_type(prefix, field_types, data_type)?;
                format!("many!(call!({}))", subparser)
            }
            DataType::RestCombinator => "rest".to_string(),
        })
    }

    fn use_ref(data_type: &DataType) -> bool {
        match data_type {
            DataType::Value(ref v) => match v.as_ref() {
                "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" => false,
                v => Generator::parse_int_type(v).is_err(),
            },
            _ => true,
        }
    }

    fn arg_type(data_type: &DataType) -> Result<String, String> {
        Ok(match data_type {
            DataType::Value(ref v) => v.clone(),
            DataType::Array { data_type, .. } => {
                format!("&[{}]", Generator::arg_type(&*data_type)?)
            }
            _ => {
                return Err(format!(
                    "Data type {:?} is not supported as an argument",
                    data_type
                ))
            }
        })
    }

    fn parse_fn(message: &Message) -> Result<Function, String> {
        let mut fun = Function {
            name: "parse".to_string(),
            public: true,
            generics: vec!["'a".to_string()],
            args: vec!["_s0: State<'a>".to_string()],
            return_type: Some(format!(
                "PResult<(State<'a>, {})>",
                to_camel_case(&message.name, true)
            )),
            body: vec![],
        };

        let mut field_types: HashMap<String, DataType> = HashMap::new();

        // add arguments to the function
        for arg in &message.args {
            fun.args.push(format!(
                "_{}: {}",
                arg.name,
                Generator::arg_type(&arg.data_type)?
            ));

            field_types.insert(arg.name.clone(), arg.data_type.clone());

            // if the argument has a value, we also need to add predicates at the beginning to check
            // the value matches
            if let Some(v) = &arg.value {
                fun.body.push(RustExpression::If {
                    condition: Box::new(binop(
                        "==",
                        Generator::render_value(v),
                        RustExpression::Value(RustValue::Variable(format!("_{}", arg.name))),
                    )),
                    true_block: Box::new(RustExpression::Return(Box::new(
                        RustExpression::Struct {
                            name: "protogen::Error".to_string(),
                            fields: vec![
                                ("error".to_string(), var("protogen::ErrorType::Failure")),
                                (
                                    "position".to_string(),
                                    binop(
                                        "+",
                                        binop("*", method("_s0", "offset", vec![]), num(8)),
                                        method("_s0", "bit_offset", vec![]),
                                    ),
                                ),
                            ],
                        },
                    ))),
                    false_block: None,
                });
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
                    "Could not find stream {} for {} in {}",
                    target, f.name, message.name
                ))?;

                if let DataType::Array { data_type, length } = &tf.data_type {
                    if let DataType::Value(ref v) = **data_type {
                        if v == "u8" {
                            let l = match length {
                                Expression::Value(Value::Number(n)) => n.to_string(),
                                Expression::Variable(s) => format!("_{}", &s[1..]),
                                _ => unimplemented!(),
                            };

                            (
                                format!("{}.range_to(..{} as usize)", i, l),
                                "_".to_string(),
                                f,
                            )
                        } else {
                            return Err(format!(
                                "Stream source {} for {} in {} is not a byte array",
                                target, f.name, message.name
                            ));
                        }
                    } else {
                        return Err(format!(
                            "Stream source {} for {} in {} is not a byte array",
                            target, f.name, message.name
                        ));
                    }
                } else {
                    return Err(format!(
                        "Stream source {} for {} in {} is not a byte array",
                        target, f.name, message.name
                    ));
                }
            } else {
                let v = (format!("_s{}", input_idx), format!("_s{}", output_idx), f);
                input_idx += 1;
                output_idx += 1;
                v
            };

            if io.insert(&f.name[..], v).is_some() {
                return Err(format!("duplicate field {} in {}", f.name, message.name));
            }
        }

        let mut final_output = "_s0";
        for f in &message.fields {
            let prefix = [&message_type[..], &to_camel_case(&f.name, true)].join("_");

            if let Some(ex) = &f.value {
                let data_type = Generator::render_data_type(&prefix, &mut vec![], &f.data_type);
                fun.body.push(format!(
                    "let _{}: {} = ({}) as {};",
                    f.name,
                    data_type,
                    Generator::render_expression("", ex),
                    data_type
                ));
            } else {
                let (input, output, _) = io.get(&f.name[..]).expect("missing i/o info for field");

                fun.body.push(format!(
                    "let ({}, _{}) = call!({}, {})?;",
                    output,
                    f.name,
                    input,
                    Generator::parser_for_data_type(&prefix, &field_types, &f.data_type)?
                ));

                if let Some(ref constraints) = f.constraints {
                    let cs: Vec<String> = constraints
                        .iter()
                        .map(|c| format!("_{} == {}", f.name, Generator::render_expression("", c)))
                        .collect();

                    fun.body.push(format!("if !({}) {{
      return Err(protogen::Error {{ error: protogen::ErrorType::Failure, position: {}.offset * 8 + {}.bit_offset }})
    }}", cs.join(" || "), input, input));
                }

                if output != "_" {
                    final_output = output;
                }
            }
        }

        let mut construct_args: Vec<String> = message
            .args
            .iter()
            .filter(|a| a.value.is_none())
            .map(|a| {
                match a.data_type {
                    // TODO: probably need to support other stuff here
                    DataType::Array { .. } => format!("_{}: _{}.to_vec()", a.name, a.name),
                    _ => format!("_{}", a.name),
                }
            })
            .collect();

        construct_args.extend(
            message
                .fields
                .iter()
                .filter(|f| f.value.is_none() && !f.variable)
                .map(|f| format!("_{}", f.name)),
        );

        fun.body.push(format!(
            "Ok(({}, {} {{ {} }}))",
            final_output,
            message_type,
            construct_args.join(", ")
        ));

        Ok(fun)
    }

    fn writer_for_field(
        message: &Message,
        field_name: &str,
        var: &str,
        data_type: &DataType,
    ) -> String {
        let vref = format!(
            "{}{}",
            if Generator::use_ref(data_type) {
                "&"
            } else {
                ""
            },
            var
        );

        match data_type {
            DataType::Value(ref v) => {
                match v.as_ref() {
                    "u8" => format!("buf.push_u8({});", vref),
                    "i8" => format!("buf.push_i8({});", vref),
                    w @ "u16" | w @ "u32" | w @ "u64" | w @ "i16" | w @ "i32" | w @ "i64" => {
                        format!("buf.push_{}_le({});", w, vref)
                    }
                    "cstring" => format!("buf.push_bytes({});", vref),
                    "u1" => format!("buf.push_bit({} == 1);", vref),
                    v => {
                        if let Ok((sign, size, _width)) = Generator::parse_int_type(v) {
                            if sign == "i" {
                                // not really clear what to do here... does this even make sense??
                                "unimplemented();".to_string()
                            } else {
                                format!(
                                    "for _v in 0..{} {{ buf.push_bit({} & (1 << _v) > 0) }}",
                                    size, vref
                                )
                            }
                        } else {
                            "unimplemented!();".to_string()
                        }
                    }
                }
            }
            DataType::Array { data_type, .. } => match data_type.as_ref() {
                DataType::Value(v) if *v == "u8" => format!("buf.push_bytes({});", vref),
                dt => format!(
                    "for v in {} {{ {} }}",
                    vref,
                    Generator::writer_for_field(message, field_name, "*v", dt)
                ),
            },
            DataType::Message { name: ref m, .. } if m == "str_utf8" => {
                format!("buf.push_bytes({}.as_bytes());", var)
            }
            DataType::Message { .. } => format!("({}).write_bytes(buf);", var),
            DataType::ManyCombinator { data_type } => Generator::writer_for_field(
                message,
                field_name,
                var,
                &DataType::Array {
                    data_type: data_type.clone(),
                    length: Expression::Value(Value::Number(0)),
                },
            ),
            DataType::RestCombinator => format!("buf.push_bytes({});", vref),
            DataType::Choose(variants) => {
                let matches: Vec<String> = variants
                    .iter()
                    .map(|var| {
                        format!(
                            "        {}_{}::{}(v) => v.write_bytes(buf),",
                            to_camel_case(&message.name, true),
                            to_camel_case(&field_name, true),
                            var.name
                        )
                    })
                    .collect();

                format!("match {} {{\n{}\n    }}", vref, matches.join("\n"))
            }
        }
    }

    fn write_bytes_fn(_message: &Message) -> Function {
        let body = vec![];

        //        let graph = message.graph();
        //
        //        for field in &message.fields {
        //            if field.value.is_none() && field.apply_to.is_none() {
        //                let name = if field.variable {
        //                    let deps = graph.get(&field.name).unwrap();
        //                    if deps.len() != 1 {
        //                        format!(
        //                            "panic!(\"expected exactly one upstream for {}, found {}\")",
        //                            field.name,
        //                            deps.len()
        //                        )
        //                    } else {
        //                        match &deps[0].1 {
        //                            Edge::Expression(expr) => {
        //                                format!("({})", Generator::render_expression("self.", &expr))
        //                            }
        //                            Edge::ApplyTo => format!("(self._{}.to_vec())", deps[0].0),
        //                        }
        //                    }
        //                } else {
        //                    format!("self._{}", field.name)
        //                };
        //
        //                body.push(Generator::writer_for_field(
        //                    message,
        //                    &field.name,
        //                    &name,
        //                    &field.data_type,
        //                ));
        //            }
        //        }

        let buf_name = if body.is_empty() { "_buf" } else { "buf" };

        Function {
            name: "write_bytes".to_string(),
            public: false,
            generics: vec![],
            args: vec![
                "&self".to_string(),
                format!("{}: &mut buffer::BitBuffer", buf_name),
            ],
            return_type: None,
            body,
        }
    }

    fn add_field(
        public: bool,
        name: &str,
        data_type: &DataType,
        value: Option<&Expression>,
        is_variable: bool,
        st: &mut Struct,
        imp: &mut Impl,
        enums: &mut Vec<Enum>,
    ) {
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
            getter.body.push(format!(
                "({}) as {}",
                Generator::render_expression("self.", v),
                data_type_string
            ));
        } else if !is_variable {
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
        let mut structs = HashMap::new();
        let mut enums: Vec<Enum> = vec![];
        let mut impls: HashMap<String, Vec<Impl>> = HashMap::new();
        let imports: HashSet<String> = ["protogen::*".to_string()].iter().cloned().collect();

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
                Generator::add_field(
                    arg.public,
                    &arg.name,
                    &arg.data_type,
                    value.as_ref(),
                    false,
                    &mut s,
                    &mut imp,
                    &mut enums,
                );
            }

            for f in &message.fields {
                Generator::add_field(
                    f.public,
                    &f.name,
                    &f.data_type,
                    f.value.as_ref(),
                    f.variable,
                    &mut s,
                    &mut imp,
                    &mut enums,
                );
            }

            imp.functions.push(Generator::parse_fn(&message)?);

            imp.functions.push(Generator::write_bytes_fn(&message));

            imp.functions.push(Function {
                name: "to_vec".to_string(),
                public: true,
                generics: vec![],
                args: vec!["&self".to_string()],
                return_type: Some("Vec<u8>".to_string()),
                body: vec![
                    "let mut buf = buffer::BitBuffer::new();".to_string(),
                    "self.write_bytes(&mut buf);".to_string(),
                    "buf.into_vec()".to_string(),
                ],
            });

            if structs.contains_key(&s.name) {
                return Err(format!("duplicate struct type {}", s.name));
            }

            impls
                .entry(s.name.clone())
                .or_insert_with(|| vec![])
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
        let mut ordered_imports: Vec<&String> = self.imports.iter().collect();
        ordered_imports.sort();
        for import in ordered_imports {
            write!(f, "use {};\n", import)?;
        }

        write!(f, "\n")?;

        for message in &self.messages {
            let name = to_camel_case(&message.name, true);
            // write the struct definition
            write!(
                f,
                "{}\n\n",
                self.structs.get(&name).expect("missing struct")
            )?;

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
