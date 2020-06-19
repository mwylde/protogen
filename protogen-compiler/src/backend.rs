use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

use crate::ast::*;
use crate::rust::*;

type Parser = Box<dyn Fn(RustExpression) -> RustExpression>;

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

fn field(target: RustExpression, name: &str) -> RustExpression {
    RustExpression::Field {
        target: Box::new(target),
        name: name.to_string(),
    }
}

fn num(d: i64) -> RustExpression {
    RustExpression::Value(RustValue::Number(d))
}

fn var(s: &str) -> RustExpression {
    RustExpression::Value(RustValue::Variable(s.to_string()))
}

fn struct_name(s: String) -> (String, RustExpression) {
    (s.clone(), RustExpression::Value(RustValue::Variable(s)))
}

fn cast(expr: RustExpression, typ: &str) -> RustExpression {
    RustExpression::Cast {
        expression: Box::new(expr),
        typ: typ.to_string(),
    }
}

fn binop(op: &str, lh: RustExpression, rh: RustExpression) -> RustExpression {
    RustExpression::BinOp {
        op: op.to_string(),
        lh: Box::new(lh),
        rh: Box::new(rh),
    }
}

fn let_(is_mut: bool, name: &str, value: RustExpression) -> RustExpression {
    RustExpression::Let {
        is_mut,
        name: name.to_string(),
        typ: None,
        value: Box::new(value),
    }
}

fn qm(e: RustExpression) -> RustExpression {
    RustExpression::Postfix("?", Box::new(e))
}

fn ok(e: RustExpression) -> RustExpression {
    RustExpression::FunctionCall {
        name: "Ok".to_string(),
        parameters: vec![e],
    }
}

fn assign(lh: &str, rh: RustExpression) -> RustExpression {
    RustExpression::Assign(Box::new(var(lh)), Box::new(rh))
}

fn count(subparser: Parser, n: RustExpression) -> Parser {
    Box::new(move |s| RustExpression::Block {
        expressions: vec![
            let_(true, "s", s),
            let_(true, "v", var("vec![]")),
            RustExpression::ForIn {
                i: Box::new(var("_")),
                iter: Box::new(RustExpression::Range(Box::new(num(0)), Box::new(n.clone()))),
                body: Box::new(RustExpression::Block {
                    expressions: vec![
                        RustExpression::TupleLet {
                            is_mut: false,
                            names: vec!["s1".to_string(), "item".to_string()],
                            types: None,
                            value: Box::new(qm(subparser(var("s")))),
                        },
                        RustExpression::Assign(Box::new(var("s")), Box::new(var("s1"))),
                        method("v", "push", vec![var("item")]),
                    ],
                    terminated: true,
                }),
            },
            RustExpression::FunctionCall {
                name: "Ok".to_string(),
                parameters: vec![RustExpression::Tuple(vec![var("s"), var("v")])],
            },
        ],
        terminated: false,
    })
}

fn many(subparser: Parser) -> Parser {
    /*
    let mut s = {s};
    let mut v = vec![];
    loop {
        match {subparser}(s) {
            Ok((s1, r)) => {
                v.push(r);
                s = s1;
            }
            Err => break;
        }
    }
    Ok((s, v))
     */
    Box::new(move |s| RustExpression::Block {
        expressions: vec![
            let_(true, "s", s),
            let_(true, "v", var("vec![]")),
            RustExpression::Loop(Box::new(RustExpression::Match(
                Box::new(subparser(var("s"))),
                vec![
                    (
                        Destructurer::TupleStruct("Ok".to_string(), vec!["(s1, r)".to_string()]),
                        RustExpression::Block {
                            expressions: vec![
                                method("v", "push", vec![var("r")]),
                                RustExpression::Assign(Box::new(var("s")), Box::new(var("s1"))),
                            ],
                            terminated: true,
                        },
                    ),
                    (
                        Destructurer::TupleStruct("Err".to_string(), vec!["_".to_string()]),
                        RustExpression::Break,
                    ),
                ],
            ))),
            RustExpression::FunctionCall {
                name: "Ok".to_string(),
                parameters: vec![RustExpression::Tuple(vec![var("s"), var("v")])],
            },
        ],
        terminated: false,
    })
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
            DataType::Value(v) if v == "cstring" => "Vec<u8>".to_string(),
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

    fn parser_for_data_type<'a>(
        prefix: &'a str,
        field_types: &'a HashMap<String, DataType>,
        data_type: &'a DataType,
    ) -> Result<Parser, String> {
        fn f(name: &'static str) -> Box<dyn Fn(RustExpression) -> RustExpression> {
            Box::new(move |input| RustExpression::FunctionCall {
                name: name.to_string(),
                parameters: vec![input],
            })
        }

        Ok(match data_type {
            DataType::Value(ref s) => match s.as_ref() {
                "u8" => f("read_u8_le"),
                "u16" => f("read_u16_le"),
                "u32" => f("read_u32_le"),
                "u64" => f("read_u64_le"),
                "i8" => f("read_i8_le"),
                "i16" => f("read_i16_le"),
                "i32" => f("read_i32_le"),
                "i64" => f("read_i64_le"),
                "cstring" => f("read_cstring"),
                t => {
                    let (sign, size, width) = Generator::parse_int_type(t)?;

                    Box::new(move |s| {
                        let f = RustExpression::FunctionCall {
                            name: format!("read_bits_u{}", width),
                            parameters: vec![s, num(size as i64)],
                        };
                        if sign == "u" {
                            f
                        } else {
                            RustExpression::Block {
                                expressions: vec![
                                    RustExpression::TupleLet {
                                        is_mut: false,
                                        names: vec!["__s".to_string(), "__v".to_string()],
                                        types: None,
                                        value: Box::new(f),
                                    },
                                    RustExpression::Tuple(vec![
                                        var("__s"),
                                        RustExpression::Cast {
                                            expression: Box::new(var("__v")),
                                            typ: format!("i{}", width),
                                        },
                                    ]),
                                ],
                                terminated: false,
                            }
                        }
                    })
                }
            },
            DataType::Message { ref name, ref args } if name == "str_utf8" => {
                if args.len() != 1 {
                    return Err(format!(
                        "Expected one argument to str_utf8, found {}",
                        args.len()
                    ));
                }
                let expr = Generator::render_expression("", args.get(0).unwrap());
                Box::new(move |s| RustExpression::FunctionCall {
                    name: "read_str_utf8".to_string(),
                    parameters: vec![s, cast(expr.clone(), "usize")],
                })
            }
            DataType::Message { ref name, ref args } => {
                let fun = to_camel_case(name, true);
                let parameters: Vec<RustExpression> = args
                    .iter()
                    .map(|e| {
                        let expr = Generator::render_expression("", e);
                        let is_ref = match &expr {
                            RustExpression::Value(RustValue::Variable(var)) => {
                                // TODO: find a better way to handle variables
                                field_types
                                    .get(&var[1..])
                                    .map(|dt| Generator::use_ref(dt))
                                    .unwrap_or(true)
                            }
                            _ => true,
                        };

                        if is_ref {
                            RustExpression::Ref(Box::new(expr))
                        } else {
                            expr
                        }
                    })
                    .collect();

                Box::new(move |s| {
                    let mut ps = vec![s];
                    ps.extend_from_slice(&parameters);
                    RustExpression::FunctionCall {
                        name: format!("{}::parse", fun),
                        parameters: ps,
                    }
                })
            }
            DataType::Choose(ref variants) => {
                let mut v_ps = vec![];
                for v in variants {
                    v_ps.push((
                        v.name.clone(),
                        Self::parser_for_data_type(
                            &[prefix, &to_camel_case(&v.name, true)].join("_"),
                            field_types,
                            &v.data_type,
                        )?,
                    ));
                }

                let prefix = prefix.to_string();

                Box::new(move |s| {
                    let mut choose = Self::return_parse_error(s.clone());
                    for (name, parser) in v_ps.iter().rev() {
                        choose = RustExpression::Match(
                            Box::new(parser(s.clone())),
                            vec![
                                (
                                    Destructurer::TupleStruct(
                                        "Ok".to_string(),
                                        vec!["(__s, r)".to_string()],
                                    ),
                                    ok(RustExpression::Tuple(vec![
                                        var("__s"),
                                        RustExpression::FunctionCall {
                                            name: format!("{}::{}", prefix, name),
                                            parameters: vec![var("r")],
                                        },
                                    ])),
                                ),
                                (
                                    Destructurer::TupleStruct(
                                        "Err".to_string(),
                                        vec!["_".to_string()],
                                    ),
                                    choose,
                                ),
                            ],
                        );
                    }

                    choose
                })
            }
            DataType::Array {
                ref data_type,
                ref length,
            } => {
                let subparser = Generator::parser_for_data_type(prefix, field_types, data_type)?;

                let len = match length {
                    Expression::Value(Value::String(_)) => {
                        return Err("Strings cannot be array lengths".to_string());
                    }
                    Expression::Value(Value::ByteArray(_)) => {
                        return Err("Byte arrays cannot be array lengths".to_string());
                    }
                    Expression::Value(Value::Number(n)) => num(*n as i64),
                    Expression::Variable(v) => var(&format!("_{}", &v[1..])),
                    expr @ Expression::Binary(..) => Generator::render_expression("", &expr),
                    Expression::Unary(..) => unimplemented!(),
                };

                count(subparser, len)
            }
            DataType::ManyCombinator { ref data_type } => {
                let subparser = Generator::parser_for_data_type(prefix, field_types, data_type)?;
                many(subparser)
            }
            DataType::RestCombinator => Box::new(|s| RustExpression::FunctionCall {
                name: "rest".to_string(),
                parameters: vec![s],
            }),
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

    fn return_parse_error(input: RustExpression) -> RustExpression {
        let error = RustExpression::Struct {
            name: "protogen::Error".to_string(),
            fields: vec![
                ("error".to_string(), var("protogen::ErrorType::Failure")),
                (
                    "position".to_string(),
                    binop(
                        "+",
                        binop("*", field(input.clone(), "offset"), num(8)),
                        field(input, "bit_offset"),
                    ),
                ),
            ],
        };

        RustExpression::Return(Box::new(RustExpression::FunctionCall {
            name: "Err".to_string(),
            parameters: vec![error],
        }))
    }

    fn parse_fn(message: &Message) -> Result<Function, String> {
        let mut fun_args = vec!["_s0: State<'a>".to_string()];
        let mut fun_body = vec![];

        let mut field_types: HashMap<String, DataType> = HashMap::new();

        // add arguments to the function
        for arg in &message.args {
            fun_args.push(format!(
                "_{}: {}",
                arg.name,
                Generator::arg_type(&arg.data_type)?
            ));

            field_types.insert(arg.name.clone(), arg.data_type.clone());

            // if the argument has a value, we also need to add predicates at the beginning to check
            // the value matches
            if let Some(v) = &arg.value {
                fun_body.push(RustExpression::If {
                    condition: Box::new(binop(
                        "!=",
                        Generator::render_value(v),
                        RustExpression::Value(RustValue::Variable(format!("_{}", arg.name))),
                    )),
                    true_block: Box::new(Self::return_parse_error(var("_s0"))),
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
                fun_body.push(RustExpression::Let {
                    is_mut: false,
                    name: format!("_{}", f.name),
                    typ: Some(data_type.clone()),
                    value: Box::new(RustExpression::Cast {
                        expression: Box::new(Generator::render_expression("", ex)),
                        typ: data_type,
                    }),
                });
            } else {
                let (input, output, _) = io.get(&f.name[..]).expect("missing i/o info for field");

                fun_body.push(RustExpression::TupleLet {
                    is_mut: false,
                    names: vec![output.clone(), format!("_{}", f.name)],
                    types: None,
                    value: Box::new(qm(Generator::parser_for_data_type(
                        &prefix,
                        &field_types,
                        &f.data_type,
                    )?(var(input)))),
                });

                if let Some(ref constraints) = f.constraints {
                    let mut cs = constraints
                        .iter()
                        .map(|c| {
                            binop(
                                "!=",
                                var(&format!("_{}", f.name)),
                                Generator::render_expression("", c),
                            )
                        })
                        .into_iter();

                    // parser guarantees we have at least one element so unwrap is safe
                    let first = cs.next().unwrap();

                    let condition = cs.fold(first, |acc, c| binop("||", acc, c));

                    fun_body.push(RustExpression::If {
                        condition: Box::new(condition),
                        true_block: Box::new(Self::return_parse_error(var(input))),
                        false_block: None,
                    });
                }

                if output != "_" {
                    final_output = output;
                }
            }
        }

        let mut construct_args: Vec<(String, RustExpression)> = message
            .args
            .iter()
            .filter(|a| a.value.is_none())
            .map(|a| {
                match a.data_type {
                    // TODO: probably need to support other stuff here
                    DataType::Array { .. } => (
                        format!("_{}", a.name),
                        RustExpression::MethodCall {
                            target: Box::new(var(&format!("_{}", a.name))),
                            name: "to_vec".to_string(),
                            parameters: vec![],
                        },
                    ),
                    _ => struct_name(format!("_{}", a.name)),
                }
            })
            .collect();

        construct_args.extend(
            message
                .fields
                .iter()
                .filter(|f| f.value.is_none() && !f.variable)
                .map(|f| struct_name(format!("_{}", f.name))),
        );

        fun_body.push(ok(RustExpression::Tuple(vec![
            var(final_output),
            RustExpression::Struct {
                name: message_type,
                fields: construct_args,
            },
        ])));

        Ok(Function {
            name: "parse".to_string(),
            public: true,
            generics: vec!["'a".to_string()],
            args: fun_args,
            return_type: Some(format!(
                "PResult<(State<'a>, {})>",
                to_camel_case(&message.name, true)
            )),
            body: RustExpression::Block {
                expressions: fun_body,
                terminated: false,
            },
        })
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
        let body: Vec<RustExpression> = vec![];

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
            body: RustExpression::FunctionCall {
                name: "unimplemented!".to_string(),
                parameters: vec![],
            },
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

        let mut body = vec![];

        if let Some(v) = value {
            body.push(RustExpression::Cast {
                expression: Box::new(Generator::render_expression("self.", v)),
                typ: data_type_string,
            });
        } else if !is_variable {
            st.fields.push(StructField {
                name: format!("_{}", name),
                data_type: data_type_string,
            });

            let expr = var(&format!("self._{}", name));
            if use_ref {
                body.push(RustExpression::Ref(Box::new(expr)));
            } else {
                body.push(expr);
            }
        } else if public {
            unimplemented!("public variables are not yet supported");
        }

        let getter = Function {
            name: format!("get_{}", name),
            public: true,
            generics: vec![],
            args: vec!["&self".to_string()],
            return_type: Some(return_type),
            body: RustExpression::Block {
                expressions: body,
                terminated: false,
            },
        };

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
                body: RustExpression::Block {
                    expressions: vec![
                        RustExpression::Let {
                            is_mut: true,
                            name: "buf".to_string(),
                            typ: None,
                            value: Box::new(RustExpression::FunctionCall {
                                name: "buffer::BitBuffer::new".to_string(),
                                parameters: vec![],
                            }),
                        },
                        RustExpression::MethodCall {
                            target: Box::new(var("self")),
                            name: "write_bytes".to_string(),
                            parameters: vec![var("&mut buf")],
                        },
                        RustExpression::MethodCall {
                            target: Box::new(var("buf")),
                            name: "into_vec".to_string(),
                            parameters: vec![],
                        },
                    ],
                    terminated: false,
                },
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
