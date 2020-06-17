use crate::ast::Protocol;
use std::io;
lalrpop_mod!(pub grammar); // synthesized by LALRPOP

pub fn parse(contents: &str, _filename: &str) -> io::Result<Protocol> {
    grammar::ProtocolParser::new()
        .parse(contents)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("Parse error: {:?}", err)))
}
