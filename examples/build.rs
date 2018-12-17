extern crate protogen;

use std::path::Path;

fn example(name: &str) {
    protogen::process_file(&Path::new(
        &format!("src/protogen/{}.protogen", name)
    ), None).unwrap();
}

fn main() {
    example("tcp");
    example("wave");
}
