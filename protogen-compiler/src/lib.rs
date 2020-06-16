#![allow(dead_code)]

#[macro_use]
extern crate lazy_static;

mod ast;
pub mod backend;
mod intermediate;
pub mod parser;
mod tests;

use ast::Message;
use std::env;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;

pub fn process_current_dir() -> io::Result<()> {
    process_dir(&env::current_dir()?)
}

fn convert_error<T>(r: Result<T, String>) -> Result<T, io::Error> {
    r.map_err(|s| io::Error::new(io::ErrorKind::Other, s))
}

fn get_out_dir() -> io::Result<PathBuf> {
    match env::var_os("OUT_DIR") {
        Some(var) => Ok(PathBuf::from(&var)),
        None => return convert_error(Err("missing OUT_DIR variable".to_string())),
    }
}

pub fn process_dir(path: &Path) -> io::Result<()> {
    let out_dir = get_out_dir()?;
    let name = path.components().last().expect("not a valid path");
    let out_file = out_dir.join(Path::new(&name)).with_extension("rs");

    process_dir_to(path, &out_file, None)
}

pub fn process_dir_to(
    path: &Path,
    out_file: &Path,
    post_process: Option<&dyn Fn(&str) -> String>,
) -> io::Result<()> {
    fs::create_dir_all(out_file.parent().expect("No parent for out file"))?;
    let messages = parse_dir_int(path)?;
    process(messages, &out_file, post_process)?;

    Ok(())
}

pub fn process_file(path: &Path, post_process: Option<&dyn Fn(&str) -> String>) -> io::Result<()> {
    let messages = parse_file(&path)?;

    let out_dir = get_out_dir()?;
    let name = path.components().last().expect("not a valid path");
    let out_file = out_dir.join(Path::new(&name)).with_extension("rs");

    process(messages, &out_file, post_process)
}

fn parse_dir_int(path: &Path) -> io::Result<Vec<Message>> {
    println!("process dir {}", path.to_string_lossy());

    let mut messages = vec![];

    for entry in fs::read_dir(path)? {
        let entry = entry?;

        if entry.file_type()?.is_dir() {
            messages.extend(parse_dir_int(&entry.path())?);
        } else if entry
            .path()
            .extension()
            .iter()
            .any(|ext| *ext == "protogen")
        {
            messages.extend(parse_file(&entry.path())?);
        }
    }

    Ok(messages)
}

fn parse_file(path: &Path) -> io::Result<Vec<Message>> {
    let data = fs::read(path)?;

    return match parser::source_file(&data) {
        Ok((rest, messages)) => {
            let rest = String::from_utf8_lossy(&rest);

            if rest.trim().is_empty() {
                Ok(messages)
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("Parse error at {}", rest),
                ))
            }
        }
        Err(e) => Err(io::Error::new(
            io::ErrorKind::Other,
            format!("Parse error {:?}", e),
        )),
    };
}

fn process(
    messages: Vec<Message>,
    out_path: &Path,
    post_process: Option<&dyn Fn(&str) -> String>,
) -> io::Result<()> {
    // let name = path.file_stem().expect("not a file");
    // let out_file = out_dir.join(Path::new(name)).with_extension("rs");
    let mut generated = convert_error(backend::Generator::from_messages(messages))?.to_string();
    if let Some(process_fn) = post_process {
        generated = process_fn(&generated);
    }

    fs::write(&out_path, generated)?;

    Ok(())
}
