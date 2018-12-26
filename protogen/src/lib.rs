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

    #[test]
    fn read_u8() {
        let buf = vec![100u8, 200u8];
        let state = State::from_slice(&buf);
        let (state, x) = read_u8_le(&state).unwrap();
        let (state, y) = read_u8_le(&state).unwrap();
        let err = read_u8_le(&state);
        assert_eq!(100, x);
        assert_eq!(200, y);
        assert_eq!(Err(Error {
            error: ErrorType::Incomplete(8),
            position: 16,
        }), err);

        let state = State {
            data: &buf,
            offset: 0,
            bit_offset: 4
        };

        let (state, x) = read_u8_le(&state).unwrap();
        assert_eq!(76, x);
        assert_eq!(8, state.offset);
        assert_eq!(4, state.bit_offset);
    }

    #[test]
    fn test_read_bits_u8() {
        let buf = vec![100u8, 200u8];
        let state = State::from_slice(&buf);
        let (state, x) = read_bits_u8(&state, 4).unwrap();
        let (state, y) = read_bits_u8(&state, 6).unwrap();
        let (state, z) = read_bits_u8(&state, 6).unwrap();
        let err = read_bits_u8(&state, 1);
        assert_eq!(6, x);
        assert_eq!(19, y);
        assert_eq!(8, z);
        assert_eq!(Err(Error {
            error: ErrorType::Incomplete(1),
            position: 16,
        }), err);
    }

    #[test]
    fn read_u16() {
        let buf = vec![3u8, 170u8];
        let state = State::from_slice(&buf);
        let (state, x) = read_u16_le(&state).unwrap();
        let err = read_u16_le(&state);
        assert_eq!(43523, x);
        assert_eq!(Err(Error {
            error: ErrorType::Incomplete(16),
            position: 16,
        }), err);

        let buf = vec![5u8, 108, 10, 55];
        let state = State {
            data: &buf,
            offset: 0,
            bit_offset: 4
        };

        let (state, x) = read_u16_le(&state).unwrap();
        assert_eq!(49238, x);
        assert_eq!(2, state.offset);
        assert_eq!(4, state.bit_offset);
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

#[derive(Debug, PartialOrd, PartialEq)]
pub enum ErrorType {
    Incomplete(usize),
    Failure,
}

#[derive(Debug, PartialOrd, PartialEq)]
pub struct Error {
    error: ErrorType,
    position: usize
}

#[derive(Debug, PartialOrd, PartialEq)]
pub struct State<'a> {
    data: &'a [u8],
    offset: usize,
    bit_offset: usize,
}

impl <'a> State<'a> {
    pub fn from_slice(slice: &[u8]) -> State {
        State {
            data: slice,
            offset: 0,
            bit_offset: 0,
        }
    }
}

fn expect(state: &State, num_bits: usize) -> Result<(), Error> {
    let diff = num_bits as i64 - ((state.data.len() as i64 - state.offset as i64) * 8 -
        state.bit_offset as i64);
    if diff > 0 {
        Err(Error { error: ErrorType::Incomplete(diff as usize),
            position: state.offset * 8 + state.bit_offset })
    } else {
        Ok(())
    }
}

type PResult<T> = Result<T, Error>;

//fn read_bits(state: State, num_bits: u64) -> PResult<(State, Vec<u8>)> {
//    expect(state, num_bits)?;
//
//    if state.bit_offset == 0 {
//
//    } else {
//        unimplemented!();
//    }
//
//}


#[macro_export(local_innner_macros)]
macro_rules! read_bits_width (
  ($name:ident, $t:ty) => (
  pub fn $name<'a>(state: &'a State, num_bits: usize) -> PResult<(State<'a>, $t)> {
    expect(state, num_bits)?;
    let mut acc: $t = 0u8.into();
    let mut bit_offset = state.bit_offset;
    let mut remaining = num_bits;
    let mut end_offset = 0usize;
    let count = (num_bits + bit_offset) / 8;

    for byte in state.data[state.offset..].iter().take(count + 1) {
        if remaining == 0 {
            break;
        }

        let val: $t = if bit_offset == 0 {
            (*byte as u8).into()
        } else {
            (((*byte as u8) << bit_offset) as u8 >> bit_offset).into()
        };

        if remaining < 8 - bit_offset {
            acc += val >> (8 - bit_offset - remaining);
            end_offset = remaining + bit_offset;
            println!("acc = {}, {}, {}, {} (final)", acc, val, bit_offset, remaining);
            break;
        } else {
            acc += val << (remaining - (8 - bit_offset));
            remaining -= 8 - bit_offset;
            bit_offset = 0;
        }
        println!("acc = {}", acc);
    }

    Ok((State {
        data: &state.data,
        offset: state.offset + count,
        bit_offset: end_offset
    }, acc))
  }
));

read_bits_width!(read_bits_u8, u8);
pub fn read_u8_le<'a>(state: &'a State) -> PResult<(State<'a>, u8)> {
    expect(state, 8)?;
    if state.bit_offset == 0 {
        Ok((State {
            data: &state.data,
            offset: state.offset + 1,
            bit_offset: 0,
        }, state.data[state.offset]))
    } else {
        read_bits_u8(state, 8)
    }
}

read_bits_width!(read_bits_u16, u16);
pub fn read_u16_le<'a>(state: &'a State) -> PResult<(State<'a>, u16)> {
    expect(state, 16)?;
    if state.bit_offset == 0 {
        let v = ((state.data[state.offset + 1] as u16) << 8) + state.data[state.offset] as u16;
        Ok((State {
            data: &state.data,
            offset: state.offset + 2,
            bit_offset: 0,
        }, v))
    } else {
        let (s1, b2) = read_bits_u8(state, 8)?;
        let (s2, b1) = read_bits_u8(&s1, 8)?;
        Ok((State {
            data: &state.data,
            offset: s2.offset,
            bit_offset: s2.bit_offset
        }, ((b1 as u16) << 8) + b2 as u16))
    }
}

read_bits_width!(read_bits_u32, u32);
read_bits_width!(read_bits_u64, u64);
