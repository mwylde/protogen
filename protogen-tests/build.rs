extern crate protogen_compiler;

use std::env;
use std::path::Path;

fn main() {
    let mut path = env::current_dir().unwrap();
    path.push("src");
    path.push("protogen");
    protogen_compiler::process_dir(&path).unwrap();
}
