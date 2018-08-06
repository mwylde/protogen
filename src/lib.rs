#[macro_use]
extern crate nom;

use nom::{is_alphanumeric, is_hex_digit, is_digit};
use std::str::FromStr;
use std::str;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn multiple_fields() {
        let text = r#"
hci_command = {
  public @ocf: u8;
  length: u8;
}!"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[33][..], Message {
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
                    }
                ] })));
    }

    #[test]
    fn simple() {
        let text = " hci_command =  { }!";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[33][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![] })));
    }

    #[test]
    fn single_field() {
        let text = " hci_command =  { public @ocf : u8; }!";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[33][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                    }
                ]
            })));
    }

    #[test]
    fn message_args() {
        let text = " hci_command (type: u8, name: String) = { }!";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[33][..], Message {
                name: "hci_command".to_string(),
                args: vec![
                    Arg {
                        name: "type".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: None,
                    },
                    Arg {
                        name: "name".to_string(),
                        data_type: DataType::Value("String".to_string()),
                        value: None
                    }
                ],
                fields: vec![]
            })));
    }

    #[test]
    fn message_args_with_values() {
        let text = " hci_command (type: u8 = 0xFF, name: String = \"hello world!\") = { }!";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[33][..], Message {
                name: "hci_command".to_string(),
                args: vec![
                    Arg {
                        name: "type".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: Some(Value::Number(0xFF)),
                    },
                    Arg {
                        name: "name".to_string(),
                        data_type: DataType::Value("String".to_string()),
                        value: Some(Value::String("hello world!".to_string())),
                    }
                ],
                fields: vec![]
            })));
    }

    #[test]
    fn field_hex_value() {
        let text = " hci_command =  { public @ocf : u8 = 0x22; }!";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[33][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: Some(Value::Number(0x22u64)),
                    }
                ]
            })));
    }

    #[test]
    fn field_string_value() {
        let text = " hci_command =  { public @name : String = \"hello world!\" ; }!";

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[33][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "name".to_string(),
                        data_type: DataType::Value("String".to_string()),
                        value: Some(Value::String("hello world!".to_string())),
                    }
                ]
            })));
    }

    #[test]
    fn test_number() {
        assert_eq!(number(b"0x10;"), Ok((&[b';'][..], Value::Number(0x10))));
        assert_eq!(number(b"0X5235;"), Ok((&[b';'][..], Value::Number(0x5235))));
        assert_eq!(number(b"1241;"), Ok((&[b';'][..], Value::Number(1241))));
    }

    #[test]
    fn multiple_fields_with_values() {
        let text = r#"
hci_command = {
  public @ocf: u8 = 0x2214;
  length: u32 = 55;
  @name: String = "this is a string.";
}!"#;

        assert_eq!(
            message(text.as_bytes()),
            Ok((&[33][..], Message {
                name: "hci_command".to_string(),
                args: vec![],
                fields: vec![
                    Field {
                        public: true,
                        variable: true,
                        name: "ocf".to_string(),
                        data_type: DataType::Value("u8".to_string()),
                        value: Some(Value::Number(0x2214)),
                    },
                    Field {
                        public: false,
                        variable: false,
                        name: "length".to_string(),
                        data_type: DataType::Value("u32".to_string()),
                        value: Some(Value::Number(55)),
                    },
                    Field {
                        public: false,
                        variable: true,
                        name: "name".to_string(),
                        data_type: DataType::Value("String".to_string()),
                        value: Some(Value::String("this is a string.".to_string())),
                    }
                ] })));
    }

    #[test]
    fn string_parser() {
        assert_eq!(string("\"hello world\"".as_bytes()),
        Ok((&[][..], Value::String("hello world".to_string()))))
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum DataType {
    //    U8(Option<u8>),
//    U16(Option<u16>),
//    String(Option<String>),
//    ByteArray(Option<Vec<u8>>),
    Value(String),
    Message(MessageCall),
    Choose(Vec<MessageCall>),
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Value {
    String(String),
    Number(u64),
    ByteArray(Vec<u8>),
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Field {
    public: bool,
    variable: bool,
    name: String,
    data_type: DataType,
    value: Option<Value>,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct MessageCall {
    name: String,
    args: Vec<DataType>
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Arg {
    name: String,
    data_type: DataType,
    value: Option<Value>,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Message {
    name: String,
    args: Vec<Arg>,
    fields: Vec<Field>,
}

fn is_symbol_char(i: u8) -> bool {
    is_alphanumeric(i) || i == b'_'
}

fn is_string_char(i: u8) -> bool {
    i != b'\"'
}

named!(symbol<&str>, map_res!(take_while1!(is_symbol_char), str::from_utf8));

named!(pub string<Value>,
    map!(delimited!(tag!("\""),
        map_res!(
            // escaped!(call!(is_string_char), '\\', one_of!("\"n\\")),
            take_while!(is_string_char),
            str::from_utf8),
        tag!("\"")), |s| Value::String(s.to_string())));

fn convert_hex(bytes: &[u8]) -> u64 {
    bytes.iter()
        .rev()
        .enumerate()
        .map(|(k, &v)| {
            ((v as char).to_digit(16).unwrap_or(0) << (k * 4)) as u64
        })
        .sum()
}

named!(hex_u64<u64>,
    map!(take_while_m_n!(1, 16, is_hex_digit), convert_hex));

named!(hex_number<u64>,
    do_parse!(
        _0x: alt_complete!(tag!("0x") | tag!("0X")) >>
        digits: hex_u64 >>
        ( digits )));

named!(dec_number<u64>,
    map_res!(map_res!(take_while1!(is_digit), str::from_utf8),
        FromStr::from_str));

named!(number<Value>,
    map!(alt_complete!(hex_number | dec_number), Value::Number));

named!(value<Value>,
    ws!(do_parse!(
        _equals: tag!("=") >>
        value: alt!(string | number) >>
        ( value ))));

named!(field<Field>,
    ws!(do_parse!(
        public: opt!(tag!("public")) >>
        variable: opt!(tag!("@")) >>
        name: symbol >>
        _colon: tag!(":") >>
        type_name: symbol >>
        value: opt!(value) >>
        _semicolon: tag!(";") >>
        (
            Field {
               public: public.is_some(),
               variable: variable.is_some(),
               name: name.trim().to_string(),
               data_type: DataType::Value(type_name.trim().to_string()),
               value
            }
        ))));

named!(args<Vec<Arg>>,
    ws!(delimited!(
        tag!("("),
        separated_list!(tag!(","),
            do_parse!(
                name: symbol >>
                _colon: tag!(":") >>
                type_name: symbol >>
                value: opt!(value) >>
                (
                   Arg {
                      name: name.to_string(),
                      data_type: DataType::Value(type_name.to_string()),
                      value: value
                   }
                )
            )),
        tag!(")")))
);

named!(pub message<Message>,
    ws!(do_parse!(
      name: symbol >>
      args: opt!(complete!(args)) >>
      _eq: tag!("=") >>
      fields: delimited!(tag!("{"), many0!(complete!(field)), tag!("}")) >>
      (
        Message {
            name: name.trim().to_string(),
            args: args.unwrap_or(vec![]),
            fields,
        }
      ))));
