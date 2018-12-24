extern crate nom;

use nom::IResult;

pub fn rest(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    Ok((&[][..], i.to_vec()))
}

