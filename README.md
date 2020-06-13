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

    let (_, wav) = Wave::parse(State::from_slice(&buf)).unwrap();

    for chunk in wav.get_chunks() {
        println!("Chunk: {:?}", chunk);
    }
}
```

## Language

### Messages

A protogen definition is made up of a series of _messages_, defined at
the top level of a .protogen file. The simplest message looks like
this:

```protogen
my_message = {}
```

This isn't very interesting, and won't actually parse any
bytes. Actual message definitions will be composed of a series of
_fields_, separated by semicolons.

### Fields

Fields look like this:

``` protogen
id: [u8; 4]
```

Here we have a name (id), followed by a colon, then the field's
parser. In this case, an id is made up of an array with type u8
(unsigned byte) and a length of 4.

Fields have two annotations that can be applied. `public` marks the
field as part of the public API for the type, i.e., it's something
that will be directly exposed to users of the parser. Non-public data
is not made available for use, but is often needed to handle protocol
features like magic bytes, or fields needed to interpret other parts
of the protocol.

The second annotation is `@`, which when placed before a field name
marks it as a `variable` field. Variables can be used in expressions
elsewhere in the message definition.

For example, it's common in binary formats to store variable-length
fields as a length followed by the data. In protogen, we'd describe
that like so:

``` protogen
message var_len_ex = {
  @size: u32;
  public data: [u8; @size];
}
```

Note that instead of a number to control the size of the array as we
had earlier, we use a variable. In fact, we could use arbitrary
arithmetic expressions. For example, in some formats the size will be
for the entire message including the size field itself, so the data
array should be of length `@size - 4`:

``` protogen
message var_len_ex2 = {
  @size: u32;
  public data: [u8; @size - 4];
}
```

Instead of being parsed from the data, fields can also be constructed
from other fields, which can provide more useful views on the data for
users, or abstract out a common calculation. So we could have also
written the above message like this:

``` protogen
message var_len_ex3 = {
  @size: u32;
  @data_size: u32 = @size - 4;
  public data: [u8; @data_size];
}
```

So in general, a field has the form

```
[public] [@]name: parser [= value];
```

Number literals can be expressed as decimal (e.g., `65`), hex (`0x41`),
and binary (`0b1000001`). Arrays can be expressed as sequences of
comma-separated numbers  (`[0x68, 0x65, 0x6c, 0x6c, 0x6f`]) or as
ASCII strings (`[b"hello"]`).

## Parser Combinators

Protogen is heavily influenced by [Parser
combinators](https://en.wikipedia.org/wiki/Parser_combinator). So far
we've been talking about the built-in parsers, but the key of parser
combinators is the ability to define your own parsers. How do we
construct custom parsers? We already know: they're just messages!

Let's walk through the various parse combinators available in
protogen.

### Numbers

Protogen supports all of the numerical values you would expect,
include arbitrarily-sized (up to 64 bits) signed and unsigned
integers. An unsigned integer is parsed by `u<x>` where `x` can be any
number of bits, while a signed integer is `i<x>`. Examples include
`u6`, `i32`, etc.

### Arrays

Array parser look like

```
[<subparser>; <size expression>]
```

Some examples:

```
[u8; 16] // 16 bytes
[u32; @size * 8] // (size * 8) unsigned ints
[[u8; 16]; @size] // @size elements of 16 byte arrays
```

There is also a special array type `cstring` which takes all bytes up
to and including a null byte.

### Messages

As mentioned above, messages allow us to create our own parsers which
can be combined with any of the built-in parser combinators. As a
simple example
