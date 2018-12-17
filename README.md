# Protogen

[![Build Status](https://travis-ci.org/mwylde/protogen.svg?branch=master)](https://travis-ci.org/mwylde/protogen)
[![Crates.io Version](https://img.shields.io/crates/v/protogen.svg)](https://crates.io/crates/protogen)
[![docs](https://docs.rs/protogen/badge.svg)](https://docs.rs/protogen)

Protogen is a protocol description language for generating safe and efficient
parsers and (eventually) generators for data protocols.  

Currently both the language and (Rust) reference implementations are very alpha, 
and likely to change significantly.

See [hci-rs](https://github.com/mwylde/hci-rs) for a large-scale protocol
implemented in protogen. 

A simple .wav parser looks like this:
```protogen
wave = {
  chunk_id: [u8; 4] | [b"RIFF"];
  @chunk_size: u32;
  public format: [u8; 4] | [b"WAVE"];
  @data_size: u32 = @chunk_size - 8;
  @data: [u8; @data_size];
  public chunks: apply @data many!(subchunk());
}

subchunk = {
  @id: [u8; 4];
  @size: u32;
  @data: [u8; @size];
  public subchunk: apply @data choose {
    FormatSubchunk = fmt_subchunk(@id) |
    DataSubchunk = data_subchunk(@id) |
    OtherSubchunk = other_subchunk(@id)
  };
}

fmt_subchunk ($id: [u8; 4] = b"fmt ") = {
  public audio_format: u16;
  public num_channels: u16;
  public sample_rate: u32;
  public byte_rate: u32;
  public block_align: u16;
  public bits_per_sample: u16;
}

data_subchunk ($id: [u8; 4] = b"data") = {
  public data: rest!();
}

other_subchunk (public $id: [u8; 4]) = {
  public data: rest!();
}
```

And can be used in a rust program as:

```rust
use wave::*;
use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::str;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = Path::new(args[1]);

    let mut file = File::open(&path).unwrap();
    let mut buf: Vec<u8> = vec![];
    file.read_to_end(&mut buf).unwrap();

    for chunk in parsed.get_chunks() {
        println!("Chunk: {:?}", chunk);
    }
}
```
