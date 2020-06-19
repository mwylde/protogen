use crate::ast::Protocol;
use std::io;

#[cfg(test)]
mod tests {
    #[test]
    fn parse_bstring() {
        assert_eq!(&b"test", &super::parse_bstring("test").unwrap().as_slice());
        assert_eq!(&b"\0", &super::parse_bstring("\\0").unwrap().as_slice());
        assert_eq!(
            &b"this\n",
            &super::parse_bstring("this\\n").unwrap().as_slice()
        );
        assert_eq!(
            &b"hex\xF4",
            &super::parse_bstring("hex\\xF4").unwrap().as_slice()
        );
        assert_eq!(
            &b"hex\xaa",
            &super::parse_bstring("hex\\xaa").unwrap().as_slice()
        );
    }
}

fn hex_value(c: u8) -> Option<u8> {
    if c.is_ascii_digit() {
        Some(c - b'0')
    } else if c >= b'A' && c <= b'F' {
        Some((c - b'A') + 10)
    } else if c >= b'a' && c <= b'f' {
        Some((c - b'a') + 10)
    } else {
        None
    }
}

static HEX_ERROR: &str = "\\x must be followed by 2 hex digits in byte string";

fn parse_bstring(s: &str) -> Result<Vec<u8>, &'static str> {
    let mut b = Vec::with_capacity(s.len());
    let mut iter = s.as_bytes().iter();
    loop {
        match iter.next() {
            Some(c) => {
                let c = *c;
                if c == b'\\' {
                    match iter.next() {
                        Some(c) => match c {
                            b'n' => b.push(b'\n'),
                            b'r' => b.push(b'\r'),
                            b't' => b.push(b'\t'),
                            b'\\' => b.push(b'\\'),
                            b'0' => b.push(0),
                            b'x' | b'X' => {
                                let d1: u8 = *iter.next().ok_or(HEX_ERROR)?;
                                let d2: u8 = *iter.next().ok_or(HEX_ERROR)?;

                                let h1 = hex_value(d1).ok_or(HEX_ERROR)?;
                                let h2 = hex_value(d2).ok_or(HEX_ERROR)?;

                                b.push(h1 * 16 + h2);
                            }
                            _ => {
                                return Err("invalid escape sequence in byte string");
                            }
                        },
                        None => {
                            return Err("invalid escape sequence in byte string");
                        }
                    }
                } else {
                    b.push(c);
                }
            }
            None => {
                break;
            }
        }
    }

    Ok(b)
}

lalrpop_mod!(pub grammar); // synthesized by LALRPOP

pub fn parse(contents: &str, _filename: &str) -> io::Result<Protocol> {
    grammar::ProtocolParser::new()
        .parse(contents)
        .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("Parse error: {:?}", err)))
}
