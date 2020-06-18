pub mod buffer;

use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeTo;
use std::string::FromUtf8Error;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ranges() {
        let buf = vec![1u8, 2, 3, 4];
        let state = State {
            data: &buf,
            offset: 1,
            bit_offset: 4,
        };

        assert_eq!(
            State {
                data: &buf[..3],
                offset: 2,
                bit_offset: 4
            },
            state.range(1..2)
        );

        assert_eq!(
            State {
                data: &buf,
                offset: 3,
                bit_offset: 4
            },
            state.range_from(2..)
        );

        assert_eq!(
            State {
                data: &buf[..4],
                offset: 1,
                bit_offset: 4
            },
            state.range_to(..3)
        );
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
        assert_eq!(
            Err(Error {
                error: ErrorType::Incomplete(8),
                position: 16,
            }),
            err
        );

        let state = State {
            data: &buf,
            offset: 0,
            bit_offset: 4,
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
        assert_eq!(
            Err(Error {
                error: ErrorType::Incomplete(1),
                position: 16,
            }),
            err
        );
    }

    #[test]
    fn read_u16() {
        let buf = vec![3u8, 170u8];
        let state = State::from_slice(&buf);
        let (state, x) = read_u16_le(state).unwrap();
        let err = read_u16_le(state);
        assert_eq!(43523, x);
        assert_eq!(
            Err(Error {
                error: ErrorType::Incomplete(16),
                position: 16,
            }),
            err
        );

        let buf = vec![5u8, 108, 10, 55];
        let state = State {
            data: &buf,
            offset: 0,
            bit_offset: 4,
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
        assert_eq!(
            Err(Error {
                error: ErrorType::Incomplete(32),
                position: 32,
            }),
            err
        );

        let buf = vec![109u8, 169, 171, 169, 215];
        let state = State {
            data: &buf,
            offset: 0,
            bit_offset: 4,
        };

        let (state, x) = read_u32_le(state).unwrap();
        assert_eq!(2646252250, x);
        assert_eq!(4, state.offset);
        assert_eq!(4, state.bit_offset);
    }

    #[test]
    fn read_u64() {
        let buf = vec![136u8, 79, 185, 252, 210, 65, 5, 35];
        let state = State::from_slice(&buf);
        let (state, x) = read_u64_le(state).unwrap();
        let err = read_u64_le(state);
        assert_eq!(2523495540649971592, x);
        assert_eq!(
            Err(Error {
                error: ErrorType::Incomplete(64),
                position: 64,
            }),
            err
        );

        let buf = vec![202u8, 136, 79, 185, 252, 210, 65, 5, 35];
        let state = State {
            data: &buf,
            offset: 0,
            bit_offset: 4,
        };

        let (state, x) = read_u64_le(state).unwrap();
        assert_eq!(5913266776308417704, x);
        assert_eq!(8, state.offset);
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
        assert_eq!(
            Err(Error {
                error: ErrorType::Incomplete(8),
                position: 32,
            }),
            err
        );

        let state = State {
            data: &buf,
            offset: 0,
            bit_offset: 5,
        };

        let (_, x) = read_bytes(state, 2).unwrap();
        assert_eq!(vec![103, 39], x);
    }

    #[test]
    fn test_call() {
        let buf = vec![10u8];
        let state = State::from_slice(&buf);

        let (_, x) = call!(state, read_u8_le).unwrap();
        assert_eq!(10, x);

        let (_, x) = call!(state, read_bytes, 1).unwrap();
        assert_eq!(vec![10], x);

        let (_, x) = call!(state, call!(read_bytes, 1)).unwrap();
        assert_eq!(vec![10], x);
    }

    #[test]
    fn test_tag() {
        let buf = "hello".as_bytes();
        let state = State::from_slice(buf);

        let (state, v) = tag(state, "hel".as_bytes()).unwrap();
        assert_eq!("hel".as_bytes(), v);

        let err = tag(state, "loo".as_bytes());
        assert_eq!(
            Err(Error {
                error: ErrorType::Incomplete(8),
                position: 24,
            }),
            err
        );

        let err = tag(state, "er".as_bytes());
        assert_eq!(
            Err(Error {
                error: ErrorType::Failure,
                position: 24,
            }),
            err
        );
    }

    #[test]
    fn test_count() {
        let buf = [1u8, 2, 3, 4];
        let state = State::from_slice(&buf);
        let (state, v) = count!(state, 3, call!(read_u8_le)).unwrap();
        assert_eq!(vec![1, 2, 3], v);

        let err = count!(state, 3, call!(read_u8_le));
        assert_eq!(
            Err(Error {
                error: ErrorType::Incomplete(8),
                position: 32,
            }),
            err
        );
    }

    #[test]
    fn test_many() {
        let buf = "buffalobuffalobuffaloxx".as_bytes();
        let state = State::from_slice(buf);
        let (state, v) = many!(state, call!(tag, "buffalo".as_bytes())).unwrap();
        assert_eq!(3, v.len());
        assert_eq!(21, state.offset);
        assert_eq!(0, state.bit_offset);

        let buf = [104u8, 101, 108, 108, 111, 0, 10, 11];
        let state = State::from_slice(&buf);
        let (_, v) = many!(state, None, None, call!(not, 0)).unwrap();
        assert_eq!(v, [104u8, 101, 108, 108, 111]);
    }

    #[test]
    fn test_map() {
        let buf = [1u8];
        let state = State::from_slice(&buf);
        let (_, x) = map!(state, call!(read_u8_le), |x| x * 2).unwrap();
        assert_eq!(2, x);
    }

    #[test]
    fn test_map_res() {
        let buf = "hello".as_bytes();
        let state = State::from_slice(&buf);

        let (_, string) = map_res!(state, call!(read_bytes, 5), |v| String::from_utf8(v)).unwrap();
        assert_eq!("hello".to_string(), string);
    }

    #[test]
    fn test_choose() {
        let buf = "abc".as_bytes();
        let state = State::from_slice(buf);
        let (_state, v) = choose!(
            state,
            call!(tag, "xyza".as_bytes()) | call!(tag, "ab".as_bytes())
        )
        .unwrap();
        assert_eq!("ab".as_bytes(), v);
    }

    #[test]
    fn test_messages() {
        struct Message {
            f1: u8,
            f2: u16,
            text: String,
        }

        fn parser(s: State) -> PResult<(State, Message)> {
            let (s, _) = tag(s, "msg1".as_bytes())?;
            let (s, f1) = read_u8_le(s)?;
            let (s, f2) = read_u16_le(s)?;
            let (s, text) = map_res!(s, many!(None, None, call!(not, 0)), {
                |v| String::from_utf8(v)
            })?;
            let (s, _) = tag(s, &[0])?;

            Ok((s, Message { f1, f2, text }))
        }

        let buf = [109u8, 115, 103, 49, 10, 60, 91, 104, 101, 108, 108, 111, 0];
        let state = State::from_slice(&buf);
        let (state, message) = parser(state).unwrap();
        assert_eq!(10, message.f1);
        assert_eq!(23356, message.f2);
        assert_eq!("hello".to_string(), message.text);
        assert_eq!(13, state.offset);
    }
}

pub fn rest(s: State) -> PResult<(State, Vec<u8>)> {
    if s.bit_offset == 0 {
        let v = s.data[s.offset..].to_vec();
        Ok((
            State {
                data: s.data,
                offset: s.data.len(),
                bit_offset: 0,
            },
            v,
        ))
    } else {
        unimplemented!();
    }
}

#[derive(Debug, PartialOrd, PartialEq)]
pub enum ErrorType {
    Incomplete(usize),
    Failure,
}

#[derive(Debug, PartialOrd, PartialEq)]
pub struct Error {
    pub error: ErrorType,
    pub position: usize,
}

#[derive(Debug, PartialOrd, PartialEq, Copy, Clone)]
pub struct State<'a> {
    pub data: &'a [u8],
    pub offset: usize,
    pub bit_offset: usize,
}

impl<'a> State<'a> {
    pub fn from_slice(slice: &[u8]) -> State {
        State {
            data: slice,
            offset: 0,
            bit_offset: 0,
        }
    }

    pub fn range(&self, index: Range<usize>) -> State<'a> {
        State {
            data: &self.data[..self.offset + index.end],
            offset: self.offset + index.start,
            bit_offset: self.bit_offset,
        }
    }

    pub fn range_from(&self, index: RangeFrom<usize>) -> State<'a> {
        State {
            data: &self.data,
            offset: self.offset + index.start,
            bit_offset: self.bit_offset,
        }
    }

    pub fn range_to(&self, index: RangeTo<usize>) -> State<'a> {
        State {
            data: &self.data[..self.offset + index.end],
            offset: self.offset,
            bit_offset: self.bit_offset,
        }
    }
}

fn expect(state: State, num_bits: usize) -> Result<(), Error> {
    let diff = num_bits as i64
        - ((state.data.len() as i64 - state.offset as i64) * 8 - state.bit_offset as i64);
    if diff > 0 {
        Err(Error {
            error: ErrorType::Incomplete(diff as usize),
            position: state.offset * 8 + state.bit_offset,
        })
    } else {
        Ok(())
    }
}

pub type PResult<T> = Result<T, Error>;

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
        Ok((
            State {
                data: &state.data,
                offset: state.offset + 1,
                bit_offset: 0,
            },
            state.data[state.offset],
        ))
    } else {
        read_bits_u8(state, 8)
    }
}

pub fn read_i8_le(state: State) -> PResult<(State, i8)> {
    read_u8_le(state).map(|(s, i)| (s, i as i8))
}

pub fn read_u16_le(state: State) -> PResult<(State, u16)> {
    expect(state, 16)?;
    if state.bit_offset == 0 {
        let v = ((state.data[state.offset + 1] as u16) << 8) + state.data[state.offset] as u16;
        Ok((
            State {
                data: &state.data,
                offset: state.offset + 2,
                bit_offset: 0,
            },
            v,
        ))
    } else {
        let (state, b2) = read_bits_u8(state, 8)?;
        let (state, b1) = read_bits_u8(state, 8)?;
        Ok((
            State {
                data: &state.data,
                offset: state.offset,
                bit_offset: state.bit_offset,
            },
            ((b1 as u16) << 8) + b2 as u16,
        ))
    }
}

pub fn read_i16_le(state: State) -> PResult<(State, i16)> {
    read_u8_le(state).map(|(s, i)| (s, i as i16))
}

pub fn read_u32_le(s: State) -> PResult<(State, u32)> {
    expect(s, 32)?;
    if s.bit_offset == 0 {
        let v = ((s.data[s.offset + 3] as u32) << 24)
            + ((s.data[s.offset + 2] as u32) << 16)
            + ((s.data[s.offset + 1] as u32) << 8)
            + s.data[s.offset] as u32;
        Ok((
            State {
                data: &s.data,
                offset: s.offset + 4,
                bit_offset: 0,
            },
            v,
        ))
    } else {
        let (s, b4) = read_bits_u8(s, 8)?;
        let (s, b3) = read_bits_u8(s, 8)?;
        let (s, b2) = read_bits_u8(s, 8)?;
        let (s, b1) = read_bits_u8(s, 8)?;
        Ok((
            State {
                data: &s.data,
                offset: s.offset,
                bit_offset: s.bit_offset,
            },
            (((b1 as u32) << 24) + ((b2 as u32) << 16) + ((b3 as u32) << 8) + b4 as u32),
        ))
    }
}

pub fn read_i32_le(state: State) -> PResult<(State, i32)> {
    read_u8_le(state).map(|(s, i)| (s, i as i32))
}

pub fn read_u64_le(s: State) -> PResult<(State, u64)> {
    expect(s, 64)?;
    if s.bit_offset == 0 {
        let v = ((s.data[s.offset + 7] as u64) << 56)
            + ((s.data[s.offset + 6] as u64) << 48)
            + ((s.data[s.offset + 5] as u64) << 40)
            + ((s.data[s.offset + 4] as u64) << 32)
            + ((s.data[s.offset + 3] as u64) << 24)
            + ((s.data[s.offset + 2] as u64) << 16)
            + ((s.data[s.offset + 1] as u64) << 8)
            + s.data[s.offset] as u64;
        Ok((
            State {
                data: &s.data,
                offset: s.offset + 8,
                bit_offset: 0,
            },
            v,
        ))
    } else {
        let (s, b8) = read_bits_u8(s, 8)?;
        let (s, b7) = read_bits_u8(s, 8)?;
        let (s, b6) = read_bits_u8(s, 8)?;
        let (s, b5) = read_bits_u8(s, 8)?;
        let (s, b4) = read_bits_u8(s, 8)?;
        let (s, b3) = read_bits_u8(s, 8)?;
        let (s, b2) = read_bits_u8(s, 8)?;
        let (s, b1) = read_bits_u8(s, 8)?;
        Ok((
            State {
                data: &s.data,
                offset: s.offset,
                bit_offset: s.bit_offset,
            },
            (((b1 as u64) << 56)
                + ((b2 as u64) << 48)
                + ((b3 as u64) << 40)
                + ((b4 as u64) << 32)
                + ((b5 as u64) << 24)
                + ((b6 as u64) << 16)
                + ((b7 as u64) << 8)
                + b8 as u64),
        ))
    }
}

pub fn read_i64_le(state: State) -> PResult<(State, i64)> {
    read_u8_le(state).map(|(s, i)| (s, i as i64))
}

pub fn read_bytes(state: State, n: usize) -> PResult<(State, Vec<u8>)> {
    expect(state, n * 8)?;

    if state.bit_offset == 0 {
        Ok((
            State {
                data: &state.data,
                offset: state.offset + n,
                bit_offset: 0,
            },
            state.data[state.offset..state.offset + n].to_vec(),
        ))
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

/// Reads a c-style string (with terminating NULL byte), preserving the NULL
pub fn read_cstring(state: State) -> PResult<(State, Vec<u8>)> {
    let mut v = Vec::new();
    let mut s = state;
    loop {
        let (s1, b) = read_u8_le(s)?;
        s = s1;
        v.push(b);
        if b == 0 {
            break;
        }
    }
    Ok((s, v))
}

pub fn read_str_utf8(state: State, len: usize) -> PResult<(State, String)> {
    let (s1, bs) = read_bytes(state: State, len);
    match String::from_utf8(bs) {
        Ok(res) => Ok((s1, res)),
        Err(_) => Error {
            error: ErrorType::Failure,
            position: state.offset * 8 + state.bit_offset
        },
    }
}

#[inline]
fn fail(state: State) -> Error {
    Error {
        error: ErrorType::Failure,
        position: state.offset * 8 + state.bit_offset,
    }
}

pub fn tag<'a, 'b>(state: State<'a>, tag: &'b [u8]) -> PResult<(State<'a>, &'b [u8])> {
    let (s2, bytes) = read_bytes(state, tag.len())?;

    if bytes == tag {
        Ok((s2, tag))
    } else {
        Err(fail(state))
    }
}

pub fn not(state: State, byte: u8) -> PResult<(State, u8)> {
    let (s1, b) = read_u8_le(state)?;
    if b == byte {
        Err(fail(state))
    } else {
        Ok((s1, b))
    }
}

#[macro_export(local_inner_macros)]
macro_rules! call(
    ($state:expr, $parser:ident! ($($args:tt)*)) => (
        $parser! ($state, $($args)*)
    );

    ($state:expr, $parser:expr) => (
        $parser( $state )
    );

    ($state:expr, $parser:expr, $($args:expr),* ) => (
        $parser($state, $($args),*)
    );
);

#[macro_export(local_inner_macros)]
macro_rules! map_res(
  ($state: expr, $parser:ident!( $($args:tt)* ), $map:expr) => (
    {
        let res = $parser!($state, $($args)*);
        match res {
          Ok((s, v)) => {
            match $map(v) {
              Ok(vp) => Ok((s, vp)),
              Err(_) => {
                Err($crate::Error {
                  error: $crate::ErrorType::Failure,
                  position: $state.offset * 8 + $state.bit_offset,
                })
              }
            }
          },
          Err(err) => Err(err),
        }
    }
  );
);

#[macro_export(local_inner_macros)]
macro_rules! map(
  ($state: expr, $parser:ident!( $($args:tt)* ), $map:expr) => (
    {
        let res = $parser!($state, $($args)*);
        match res {
          Ok((s, v)) => Ok((s, $map(v))),
          Err(err) => Err(err),
        }
    }
  );
);

#[macro_export(local_inner_macros)]
macro_rules! many(
($state: expr, $max: expr, $min: expr, $parser:ident!( $($args:tt)* )) => (
    {
        let min: std::option::Option<usize> = $min;
        let max: std::option::Option<usize> = $max;
        let mut v = std::vec![];
        let mut _s = $state;
        let mut error: std::option::Option<$crate::Error> = None;
        loop {
            match $parser!(_s, $($args)*) {
                Ok((s1, x)) => {
                    if s1.offset == $state.offset && s1.bit_offset == $state.bit_offset {
                      break;
                    }
                    _s = s1;
                    v.push(x);
                    if max.is_some() && v.len() > max.unwrap() {
                       error = Some(Error {
         error: ErrorType::Failure,
        position: $state.offset * 8 + $state.bit_offset
    }
                       );
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
);
($state:expr, $parser:ident!( $($args:tt)*)) => (
   many!($state, None, None, $parser!($($args)*))
);
);

#[macro_export(local_inner_macros)]
macro_rules! choose (
($state: expr, $subrule:ident!( $($args:tt)*) | $($rest:tt)*) => (
  {
    let res = $subrule!($state, $($args)*);
    match res {
      Ok(_) => res,
      Err(_) => choose!($state, $($rest)*),
    }
  }
);

($state: expr, $subrule:ident!( $($args:tt)*)) => (
  {
    let res = $subrule!($state, $($args)*);
    match res {
      Ok(_) => res,
      Err(_) => Err($crate::Error {
          error: $crate::ErrorType::Failure,
          position: $state.offset * 8 + $state.bit_offset
      }),
    }
  }
)
);

#[macro_export(local_inner_macros)]
macro_rules! count (
($state:expr, $count:expr, $parser:ident!( $($args:tt)*)) => (
{
    let _count: usize = $count;
    let mut v = Vec::with_capacity(_count);
    let mut s = $state;
    let mut err = None;
    for _ in 0.._count {
        let res = $parser!(s, $($args)*);
        match res {
            Ok((s1, x)) => {
                s = s1;
                v.push(x);
            }
            Err(e) => {
               err = Some(e);
               break;
            }
        }
    }
    if err.is_some() {
       Err(err.unwrap())
    }  else {
       Ok((s, v))
    }
})
);
