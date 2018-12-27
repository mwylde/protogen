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
        let (state, x) = read_u8_le(state).unwrap();
        let (state, y) = read_u8_le(state).unwrap();
        let err = read_u8_le(state);
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

        let (state, x) = read_u8_le(state).unwrap();
        assert_eq!(76, x);
        assert_eq!(1, state.offset);
        assert_eq!(4, state.bit_offset);
    }

    #[test]
    fn test_read_bits_u8() {
        let buf = vec![100u8, 200u8];
        let state = State::from_slice(&buf);
        let (state, x) = read_bits_u8(state, 4).unwrap();
        let (state, y) = read_bits_u8(state, 6).unwrap();
        let (state, z) = read_bits_u8(state, 6).unwrap();
        let err = read_bits_u8(state, 1);
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
        let (state, x) = read_u16_le(state).unwrap();
        let err = read_u16_le(state);
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

        let (state, x) = read_u16_le(state).unwrap();
        assert_eq!(49238, x);
        assert_eq!(2, state.offset);
        assert_eq!(4, state.bit_offset);
    }

    #[test]
    fn read_u32() {
        let buf = vec![226u8, 41, 8, 223];
        let state = State::from_slice(&buf);
        let (state, x) = read_u32_le(state).unwrap();
        let err = read_u32_le(state);
        assert_eq!(3741854178, x);
        assert_eq!(Err(Error {
            error: ErrorType::Incomplete(32),
            position: 32,
        }), err);

        let buf = vec![109u8, 169, 171, 169, 215];
        let state = State {
            data: &buf,
            offset: 0,
            bit_offset: 4
        };

        let (state, x) = read_u32_le(state).unwrap();
        assert_eq!(2646252250, x);
        assert_eq!(4, state.offset);
        assert_eq!(4, state.bit_offset);
    }

    #[test]
    fn test_bytes() {
        let buf = vec![171u8, 57, 63, 220];
        let state = State::from_slice(&buf);

        let (state, x) = read_bytes(state, 2).unwrap();
        assert_eq!(vec![171u8, 57], x);

        let (state, x) = read_bytes(state, 2).unwrap();
        assert_eq!(vec![63u8, 220], x);

        let err = read_bytes(state, 1);
        assert_eq!(Err(Error {
            error: ErrorType::Incomplete(8),
            position: 32,
        }), err);

        let state = State {
            data: &buf,
            offset: 0,
            bit_offset: 5
        };

        let (_, x) = read_bytes(state, 2).unwrap();
        assert_eq!(vec![103, 39], x);
    }

    #[test]
    fn test_tag() {
        let buf = "hello".as_bytes();
        let state = State::from_slice(buf);

        let (state, _) = tag(state, "hel".as_bytes()).unwrap();

        let err = tag(state, "loo".as_bytes());
        assert_eq!(Err(Error {
            error: ErrorType::Incomplete(8),
            position: 24,
        }), err);

        let err = tag(state, "er".as_bytes());
        assert_eq!(Err(Error {
            error: ErrorType::Failure,
            position: 24,
        }), err);
    }


    #[test]
    fn test_many() {
        let buf = "buffalobuffalobuffaloxx".as_bytes();
        let state = State::from_slice(buf);
        let (state, v) = many!(state, None, None, tag("buffalo".as_bytes())).unwrap();
        assert_eq!(3, v.len());
        assert_eq!(21, state.offset);
        assert_eq!(0, state.bit_offset);
    }

    #[test]
    fn test_choose() {

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

#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
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

fn expect(state: State, num_bits: usize) -> Result<(), Error> {
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

macro_rules! read_bits_width (
  ($name:ident, $t:ty) => (
  pub fn $name(state: State, num_bits: usize) -> PResult<(State, $t)> {
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
read_bits_width!(read_bits_u16, u16);
read_bits_width!(read_bits_u32, u32);
read_bits_width!(read_bits_u64, u64);

pub fn read_u8_le(state: State) -> PResult<(State, u8)> {
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


pub fn read_u16_le(state: State) -> PResult<(State, u16)> {
    expect(state, 16)?;
    if state.bit_offset == 0 {
        let v = ((state.data[state.offset + 1] as u16) << 8) + state.data[state.offset] as u16;
        Ok((State {
            data: &state.data,
            offset: state.offset + 2,
            bit_offset: 0,
        }, v))
    } else {
        let (state, b2) = read_bits_u8(state, 8)?;
        let (state, b1) = read_bits_u8(state, 8)?;
        Ok((State {
            data: &state.data,
            offset: state.offset,
            bit_offset: state.bit_offset
        }, ((b1 as u16) << 8) + b2 as u16))
    }
}

pub fn read_u32_le(s: State) -> PResult<(State, u32)> {
    expect(s, 32)?;
    if s.bit_offset == 0 {
        let v = ((s.data[s.offset + 3] as u32) << 24) +
            ((s.data[s.offset + 2] as u32) << 16) +
            ((s.data[s.offset + 1] as u32) << 8) +
            s.data[s.offset] as u32;
        Ok((State {
            data: &s.data,
            offset: s.offset + 4,
            bit_offset: 0,
        }, v))
    } else {
        let (s, b4) = read_bits_u8(s, 8)?;
        let (s, b3) = read_bits_u8(s, 8)?;
        let (s, b2) = read_bits_u8(s, 8)?;
        let (s, b1) = read_bits_u8(s, 8)?;
        Ok((State {
            data: &s.data,
            offset: s.offset,
            bit_offset: s.bit_offset
        }, (((b1 as u32) << 24) +
            ((b2 as u32) << 16) +
            ((b3 as u32) << 8) +
            b4 as u32)))
    }
}

pub fn read_bytes(state: State, n: usize) -> PResult<(State, Vec<u8>)> {
    expect(state, n * 8)?;

    if state.bit_offset == 0 {
        Ok((State {
            data: &state.data,
            offset: state.offset + n,
            bit_offset: 0,
        }, state.data[state.offset..state.offset + n].to_vec()))
    } else {
        let mut v = Vec::with_capacity(n);

        let mut s = state;
        for _ in 0..n {
            let (s1, b) = read_bits_u8(s, 8)?;
            s = s1;
            v.push(b);
        }

        Ok((s, v))
    }
}

#[inline]
fn fail(state: State) -> Error {
    Error {
        error: ErrorType::Failure,
        position: state.offset * 8 + state.bit_offset
    }
}

pub fn tag<'a>(state: State<'a>, tag: &[u8]) -> PResult<(State<'a>, ())> {
    let (s2, bytes) = read_bytes(state, tag.len())?;

    if bytes == tag {
       Ok((s2, ()))
    } else {
        Err(fail(state))
    }
}

#[macro_export(local_inner_macros)]
macro_rules! many (
($state: expr, $max: expr, $min: expr, $parser:ident( $($args:tt)* )) => (
    {
        let min: std::option::Option<usize> = $min;
        let max: std::option::Option<usize> = $max;
        let mut v = std::vec![];
        let mut _s = $state;
        let mut error: std::option::Option<$crate::Error> = None;
        loop {
            match $parser(_s, $($args)*) {
                Ok((s1, x)) => {
                    if s1.offset == $state.offset && s1.bit_offset == $state.bit_offset {
                      break;
                    }
                    _s = s1;
                    v.push(x);
                    if max.is_some() && v.len() > max.unwrap() {
                       error = Some(fail($state));
                       break;
                    }
                }
                Err(err) => {
                    if min.is_some() && v.len() < min.unwrap() {
                        error = Some(err);
                    }
                    break;
                }
            }
        }

        if error.is_none() {
          Ok((_s, v))
        } else {
          Err(error.unwrap())
        }
    }
));

//#[macro_export(local_inner_macros)]
//macro_rules! choose (
//($state: expr, $)
//pub fn choose<'a, T>(state: State<'a>, parsers: [Parser<T>; 2]) -> PResult<(State<'a>, T)> {
//    for parser in &parsers {
//        match parser(state) {
//            Ok(r) => return Ok(r),
//            Err( Error { error: ErrorType::Incomplete(i), position } ) =>
//                return Err(Error { error: ErrorType::Incomplete(i), position }),
//            _ => (),
//        }
//    };
//    Err(fail(state))
//}
