extern crate nom;

use nom::IResult;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_u16_le() {
        let mut buf = vec![];
        write_u16_le(&mut buf, 3523);
        assert_eq!(buf, &[195, 13]);
    }

    #[test]
    fn test_u32_le() {
        let mut buf = vec![];
        write_u32_le(&mut buf, 41284875);
        assert_eq!(buf, &[11, 245, 117, 2]);
    }

    #[test]
    fn test_u64_le() {
        let mut buf = vec![];
        write_u64_le(&mut buf, 17923220538470316381);
        assert_eq!(buf, &[93, 57, 130, 82, 30, 18, 188, 248]);
    }

    #[test]
    fn test_i8() {
        let mut buf = vec![];
        write_i8(&mut buf, -30);
        assert_eq!(buf, &[226]);
    }

    #[test]
    fn test_i16_le() {
        let mut buf = vec![];
        write_i16_le(&mut buf, -32350);
        assert_eq!(buf, &[162, 129]);
    }
}


pub fn rest(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    Ok((&[][..], i.to_vec()))
}

pub fn write_u16_le(buf: &mut Vec<u8>, v: u16) {
    buf.push((v & 0xFF) as u8);
    buf.push((v >> 8) as u8);
}

pub fn write_u32_le(buf: &mut Vec<u8>, v: u32) {
    buf.push((v & 0xFF) as u8);
    buf.push(((v >> 8) & 0xFF) as u8);
    buf.push(((v >> 16) & 0xFF) as u8);
    buf.push(((v >> 24) & 0xFF) as u8);
}

pub fn write_u64_le(buf: &mut Vec<u8>, v: u64) {
    buf.push((v & 0xFF) as u8);
    buf.push(((v >> 8) & 0xFF) as u8);
    buf.push(((v >> 16) & 0xFF) as u8);
    buf.push(((v >> 24) & 0xFF) as u8);
    buf.push(((v >> 32) & 0xFF) as u8);
    buf.push(((v >> 40) & 0xFF) as u8);
    buf.push(((v >> 48) & 0xFF) as u8);
    buf.push(((v >> 56) & 0xFF) as u8);
}

pub fn write_i8(buf: &mut Vec<u8>, v: i8) {
    buf.push(v as u8);
}

pub fn write_i16_le(buf: &mut Vec<u8>, v: i16) {
    write_u16_le(buf, v as u16);
}

pub fn write_i32_le(buf: &mut Vec<u8>, v: i32) {
    write_u32_le(buf, v as u32);
}

pub fn write_i64_le(buf: &mut Vec<u8>, v: i64) {
    write_u64_le(buf, v as u64);
}
