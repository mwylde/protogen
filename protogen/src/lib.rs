#[macro_use]
extern crate nom;

use std::path::Path;
use std::env;
use std::fs;
use std::io;
use parser::Message;

pub mod generator;
pub mod parser;

pub fn process_current_dir() -> io::Result<()> {
    process_dir(&env::current_dir()?)
}

fn convert_error<T>(r: Result<T, String>) -> Result<T, io::Error> {
    r.map_err(|s| io::Error::new(io::ErrorKind::Other, s))
}

pub fn process_dir(path: &Path) -> io::Result<()> {
    let out_dir = match env::var_os("OUT_DIR") {
        Some(var) => var,
        None => return convert_error(Err("missing OUT_DIR variable".to_string())),
    };

    let out_dir = Path::new(&out_dir);

    fs::create_dir_all(out_dir)?;

    let messages = parse_dir_int(path)?;

    // let name = path.file_stem().expect("not a file");
    let name = path.components().last().expect("not a valid path");
    let out_file = out_dir.join(Path::new(&name)).with_extension("rs");

    process(messages, &out_file)?;

    Ok(())
}

fn parse_dir_int(path: &Path) -> io::Result<Vec<Message>> {
    println!("process dir {}", path.to_string_lossy());

    let mut messages = vec![];

    for entry in fs::read_dir(path)? {
        let entry = entry?;

        if entry.file_type()?.is_dir() {
            messages.extend(parse_dir_int(&entry.path())?);
        } else if entry.path().extension().iter().any(|ext| *ext == "protogen") {
            messages.extend(parse_file(&entry.path())?);
        }
    };

    Ok(messages)
}

fn parse_file(path: &Path) -> io::Result<Vec<Message>> {
    let data = fs::read(path)?;

    match parser::source_file(&data) {
        Ok((rest, messages)) => {
            let rest = String::from_utf8_lossy(&rest);

            if rest.trim().is_empty() {
                return Ok(messages);
            } else {
                return Err(io::Error::new(io::ErrorKind::Other,
                                          format!("Parse error at {}", rest)));
            }
        }
        Err(e) => {
            return Err(io::Error::new(io::ErrorKind::Other,
                                      format!("Parse error {:?}", e)));
        }
    }
}

fn process(messages: Vec<Message>, out_path: &Path) -> io::Result<()>  {
    // let name = path.file_stem().expect("not a file");
    // let out_file = out_dir.join(Path::new(name)).with_extension("rs");
    fs::write(&out_path, convert_error(
        generator::Generator::from_messages(messages))?.to_string())?;

    Ok(())
}
