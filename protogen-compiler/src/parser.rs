use crate::ast::*;
use nom::*;
use std::str;
use std::str::FromStr;

fn is_symbol_char(i: u8) -> bool {
    is_alphanumeric(i) || i == b'_'
}

fn is_string_char(i: u8) -> bool {
    i != b'\"'
}

named!(
    comment<()>,
    do_parse!(tag!("//") >> take_until_and_consume!("\n") >> ())
);

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
    map!(take_while_m_n!(1, 16, is_hex_digit), convert_hex)
);

named!(
    bin_u64<u64>,
    map!(
        take_while_m_n!(1, 64, |c| c == b'0' || c == b'1'),
        convert_bin
    )
);

named!(
    hex_number<u64>,
    do_parse!(_0x: alt_complete!(tag!("0x") | tag!("0X")) >> digits: hex_u64 >> (digits))
);

named!(
    bin_number<u64>,
    do_parse!(_0x: alt_complete!(tag!("0b") | tag!("0B")) >> digits: bin_u64 >> (digits))
);

named!(
    dec_number<u64>,
    map_res!(
        map_res!(take_while1!(is_digit), str::from_utf8),
        FromStr::from_str
    )
);

named!(
    number<u64>,
    alt_complete!(hex_number | bin_number | dec_number)
);

named!(pub number_value<Value>, map!(number, Value::Number));

named!(
    byte_array_value<Value>,
    do_parse!(tag!("b") >> string: string >> (Value::ByteArray(string.as_bytes().to_vec())))
);

named!(
    variable<String>,
    do_parse!(
        sigil: map_res!(alt!(tag!("@") | tag!("$")), str::from_utf8)
            >> name: symbol
            >> ([sigil, name].join("").to_string())
    )
);

named!(
    value<Value>,
    alt!(byte_array_value | string_value | number_value)
);

named!(
    message_type<DataType>,
    ws!(do_parse!(
        name: symbol
            >> args: delimited!(tag!("("), separated_list!(tag!(","), expression), tag!(")"))
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
    ))
);

named!(
    rest_combinator_type<DataType>,
    ws!(do_parse!(tag!("rest!()") >> (DataType::RestCombinator {})))
);

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
                            name: symbol
                                >> _eq: tag!("=")
                                >> data_type: data_type
                                >> (ChooseVariant {
                                    name: name.to_string(),
                                    data_type,
                                })
                        )
                    ),
                    tag!("}")
                )
            >> (DataType::Choose(variants))
    ))
);

named!(
    apply<String>,
    ws!(do_parse!(tag!("apply") >> source: variable >> (source)))
);

named!(
    parens<Expression>,
    ws!(delimited!(char!('('), expression, char!(')')))
);

named!(
    binop<Expression>,
    ws!(do_parse!(
        lh: terminal_expression
            >> op: alt!(tag!("+") => {|_| BinOp::Plus } |
                        tag!("-") => {|_| BinOp::Minus } |
                        tag!("*") => {|_| BinOp::Multiply } |
                        tag!("/") => {|_| BinOp::Divide })
            >> rh: expression
            >> (Expression::Binary(op, Box::new(lh), Box::new(rh)))
    ))
);

named!(
    terminal_expression<Expression>,
    ws!(alt!(
        complete!(parens) => {|v| v} |
        complete!(variable) => {|v| Expression::Variable(v)} |
        complete!(value)   => {|v| Expression::Value(v)}))
);

named!(
    pub expression<Expression>,
    ws!(alt!(binop | terminal_expression))
);

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
    ws!(do_parse!(
        _equals: tag!("=") >> value: expression >> (value)
    ))
);

named!(
    constraints<Vec<Expression>>,
    ws!(do_parse!(
        tag!("|")
            >> tag!("[")
            >> cs: separated_nonempty_list!(tag!(","), expression)
            >> tag!("]")
            >> (cs)
    ))
);

named!(
    assign_value<Value>,
    ws!(do_parse!(_equals: tag!("=") >> value: value >> (value)))
);

named!(
    field<Field>,
    ws!(do_parse!(
        many0!(br)
            >> public: opt!(tag!("public"))
            >> variable: opt!(tag!("@"))
            >> name: symbol
            >> _colon: tag!(":")
            >> apply_to: opt!(complete!(apply))
            >> data_type: data_type
            >> value: opt!(assign_expression)
            >> constraints: opt!(constraints)
            >> _semicolon: tag!(";")
            >> (Field {
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
                    >> value: opt!(assign_value)
                    >> (Arg {
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
