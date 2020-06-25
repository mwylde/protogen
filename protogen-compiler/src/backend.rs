use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::str::FromStr;

use crate::ast::*;
use crate::intermediate::IR;
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
    pub protocol: Protocol,
    pub imports: HashSet<String>,
    pub structs: HashMap<String, Struct>,
    pub impls: HashMap<String, Vec<Impl>>,
    pub enums: Vec<Enum>,
}

impl Generator {
    pub fn render_data_type(prefix: &str, enums: &mut Vec<Enum>, data_type: &DataType) -> String {
        match data_type {
            DataType::Value(v) if v == "cstring" => "Vec<u8>".to_string(),
            DataType::Value(v) if v == "u1" => "bool".to_string(),
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
            Err(format!("Unknown type {}", s))
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

    pub fn render_expression(ex: &Expression) -> RustExpression {
        match ex {
            Expression::Value(v) => Self::render_value(v),
            Expression::Variable(v) => RustExpression::Value(RustValue::Variable(v.clone())),
            Expression::Parameter(v) => RustExpression::Value(RustValue::Variable(v.clone())),
            Expression::Binary(op, lh, rh) => RustExpression::BinOp {
                op: format!("{}", op),
                lh: Box::new(Generator::render_expression(&*lh)),
                rh: Box::new(Generator::render_expression(&*rh)),
            },
            Expression::Unary(UnaryOp::Len, arg) => RustExpression::MethodCall {
                target: Box::new(Generator::render_expression(&arg)),
                name: "len".to_string(),
                parameters: vec![],
            },
            Expression::Unary(UnaryOp::Serialize, arg) => RustExpression::MethodCall {
                target: Box::new(Generator::render_expression(&arg)),
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
                "u1" => f("read_bit"),
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
                let expr = Generator::render_expression(args.get(0).unwrap());
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
                        let expr = Generator::render_expression(e);
                        let is_ref = match &expr {
                            RustExpression::Value(RustValue::Variable(var)) => {
                                // TODO: find a better way to handle variables
                                field_types
                                    .get(&var[..])
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
                    Expression::Variable(v) => var(&v),
                    Expression::Parameter(v) => var(&v),
                    expr @ Expression::Binary(..) => Generator::render_expression(&expr),
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
                "{}: {}",
                arg.name,
                Generator::arg_type(&arg.data_type)?
            ));

            field_types.insert(arg.name.clone(), arg.data_type.clone());

            // if the argument has a value, we also need to add predicates at the beginning to check
            // the value matches
            if let Some(v) = &arg.value {
                fun_body.push(RustExpression::If {
                    condition: Box::new(binop("!=", Generator::render_value(v), var(&arg.name))),
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
                let (i, _, tf) = io.get(&target[..]).ok_or(format!(
                    "Could not find stream {} for {} in {}",
                    target, f.name, message.name
                ))?;

                if let DataType::Array { data_type, length } = &tf.data_type {
                    if let DataType::Value(ref v) = **data_type {
                        if v == "u8" {
                            let l = match length {
                                Expression::Value(Value::Number(n)) => n.to_string(),
                                Expression::Variable(s) => s.clone(),
                                Expression::Parameter(s) => s.clone(),
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
                    name: f.name.clone(),
                    typ: Some(data_type.clone()),
                    value: Box::new(RustExpression::Cast {
                        expression: Box::new(Generator::render_expression(ex)),
                        typ: data_type,
                    }),
                });
            } else {
                let (input, output, _) = io.get(&f.name[..]).expect("missing i/o info for field");

                fun_body.push(RustExpression::TupleLet {
                    is_mut: false,
                    names: vec![output.clone(), f.name.clone()],
                    types: None,
                    value: Box::new(qm(Generator::parser_for_data_type(
                        &prefix,
                        &field_types,
                        &f.data_type,
                    )?(var(input)))),
                });

                if let Some(ref constraints) = f.constraints {
                    let mut cs = constraints.iter().map(|c| {
                        binop("!=", var(&f.name.clone()), Generator::render_expression(c))
                    });

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
            .filter(|a| a.public)
            .map(|a| {
                match a.data_type {
                    // TODO: probably need to support other stuff here
                    DataType::Array { .. } => (
                        format!("_{}", a.name),
                        RustExpression::MethodCall {
                            target: Box::new(var(&a.name)),
                            name: "to_vec".to_string(),
                            parameters: vec![],
                        },
                    ),
                    _ => (format!("_{}", a.name), var(&a.name)),
                }
            })
            .collect();

        construct_args.extend(
            message
                .fields
                .iter()
                .filter(|f| f.public)
                .map(|f| (format!("_{}", f.name), var(&f.name))),
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
            // TODO: This shouldn't be necessary
            annotations: vec!["allow(unused_variables)".to_string()],
        })
    }

    fn writer_for_field(
        message: &Message,
        field_name: &str,
        data: RustExpression,
        data_type: &DataType,
    ) -> RustExpression {
        match data_type {
            DataType::Value(ref v) => {
                let typ = Self::render_data_type("", &mut vec![], data_type);
                let data = RustExpression::Cast {
                    expression: Box::new(data),
                    typ,
                };
                match v.as_ref() {
                    "u8" => method("buf", "push_u8", vec![data]),
                    "i8" => method("buf", "push_i8", vec![data]),
                    w @ "u16" | w @ "u32" | w @ "u64" | w @ "i16" | w @ "i32" | w @ "i64" => {
                        method("buf", &format!("push_{}_le", w), vec![data])
                    }
                    "cstring" => method("buf", "push_bytes", vec![data]),
                    "u1" => method("buf", "push_bit", vec![data]),
                    v => {
                        if let Ok((sign, size, _width)) = Generator::parse_int_type(v) {
                            if sign == "i" {
                                // not really clear what to do here... does this even make sense??
                                RustExpression::FunctionCall {
                                    name: "unimplemented!".to_string(),
                                    parameters: vec![],
                                }
                            } else {
                                let iter = RustExpression::Range(
                                    Box::new(num(0)),
                                    Box::new(num(size as i64)),
                                );
                                let rev_iter = RustExpression::MethodCall {
                                    target: Box::new(iter),
                                    name: "rev".to_string(),
                                    parameters: vec![],
                                };
                                RustExpression::ForIn {
                                    i: Box::new(var("_v")),
                                    iter: Box::new(rev_iter),
                                    body: Box::new(method(
                                        "buf",
                                        "push_bit",
                                        vec![
                                            // buf.push_bit(<data> & (1 << _v) > 0)
                                            binop(
                                                ">",
                                                binop("&", data, binop("<<", num(1), var("_v"))),
                                                num(0),
                                            ),
                                        ],
                                    )),
                                }
                            }
                        } else {
                            RustExpression::FunctionCall {
                                name: "unimplemented!".to_string(),
                                parameters: vec![],
                            }
                        }
                    }
                }
            }
            DataType::Array { data_type, .. } => match data_type.as_ref() {
                DataType::Value(v) if *v == "u8" => method("buf", "push_bytes", vec![data]),
                dt => RustExpression::ForIn {
                    i: Box::new(var("__v")),
                    iter: Box::new(data),
                    body: Box::new(Generator::writer_for_field(
                        message,
                        field_name,
                        var("*__v"),
                        dt,
                    )),
                },
            },
            DataType::Message { name: ref m, .. } if m == "str_utf8" => method(
                "buf",
                "push_bytes",
                vec![RustExpression::MethodCall {
                    target: Box::new(data),
                    name: "as_bytes".to_string(),
                    parameters: vec![],
                }],
            ),
            DataType::Message { .. } => RustExpression::MethodCall {
                target: Box::new(data),
                name: "write_bytes".to_string(),
                parameters: vec![var("buf")],
            },
            DataType::ManyCombinator { data_type } => Generator::writer_for_field(
                message,
                field_name,
                data,
                &DataType::Array {
                    data_type: data_type.clone(),
                    length: Expression::Value(Value::Number(0)),
                },
            ),
            DataType::RestCombinator => method("buf", "push_bytes", vec![data]),
            DataType::Choose(variants) => {
                let matches: Vec<(Destructurer, RustExpression)> = variants
                    .iter()
                    .map(|variant| {
                        (
                            Destructurer::TupleStruct(
                                format!(
                                    "{}_{}::{}",
                                    to_camel_case(&message.name, true),
                                    to_camel_case(&field_name, true),
                                    variant.name
                                ),
                                vec!["v".to_string()],
                            ),
                            method("v", "write_bytes", vec![var("buf")]),
                        )
                    })
                    .collect();

                RustExpression::Match(Box::new(data), matches)
            }
        }
    }

    fn write_bytes_fn(message: &Message, ir: &IR) -> Result<Function, String> {
        let mut body: Vec<RustExpression> = vec![];

        for field in message.fields.iter().filter(|f| f.value.is_none()) {
            let data = if field.public {
                let v = var(&format!("self._{}", field.name));
                if Self::use_ref(&field.data_type) {
                    RustExpression::Ref(Box::new(v))
                } else {
                    v
                }
            } else {
                Self::render_expression(&ir.expr_for_field(&message.name, &field.name)?)
            };

            body.push(Self::writer_for_field(
                message,
                &field.name,
                data,
                &field.data_type,
            ));
        }

        let buf_name = if body.is_empty() { "_buf" } else { "buf" };

        Ok(Function {
            name: "write_bytes".to_string(),
            public: false,
            generics: vec![],
            args: vec![
                "&self".to_string(),
                format!("{}: &mut buffer::BitBuffer", buf_name),
            ],
            return_type: None,
            body: RustExpression::Block {
                expressions: body,
                terminated: true,
            },
            annotations: vec![],
        })
    }

    fn add_field(
        public: bool,
        name: &str,
        data_type: &DataType,
        value: Option<&Expression>,
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
            if data_type_string.starts_with("Vec<") && data_type_string.ends_with('>') {
                format!("&[{}]", &data_type_string[4..data_type_string.len() - 1])
            } else {
                format!("&{}", data_type_string)
            }
        };

        let mut body = vec![];

        if public {
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
            annotations: vec![],
        };

        if public {
            imp.functions.push(getter);
        }
    }

    pub fn from_protocol(protocol: Protocol) -> Result<Generator, String> {
        let mut structs = HashMap::new();
        let mut enums: Vec<Enum> = vec![];
        let mut impls: HashMap<String, Vec<Impl>> = HashMap::new();
        let imports: HashSet<String> = ["protogen::*".to_string()].iter().cloned().collect();

        let ir = IR::from_ast(&protocol);
        let messages = &protocol.messages;

        for message in messages {
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
                    &mut s,
                    &mut imp,
                    &mut enums,
                );
            }

            imp.functions.push(Generator::parse_fn(&message)?);

            match &ir {
                Ok(ir) => {
                    imp.functions.push(Generator::write_bytes_fn(&message, ir)?);

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
                        annotations: vec![],
                    });
                }
                Err(err) => {
                    eprintln!("Failed to construct generator function: {}", err);
                }
            }

            if structs.contains_key(&s.name) {
                return Err(format!("duplicate struct type {}", s.name));
            }

            impls
                .entry(s.name.clone())
                .or_insert_with(Vec::new)
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
            protocol,
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
            writeln!(f, "use {};", import)?;
        }

        writeln!(f)?;

        for message in &self.protocol.messages {
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

            writeln!(f)?;
        }

        for e in &self.enums {
            // write the enum definition
            write!(f, "{}\n\n", e)?;

            // write any associated impls
            for imp in self.impls.get(&e.name).unwrap_or(&vec![]) {
                write!(f, "{}\n\n", imp)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}
