#[macro_use]
extern crate nom;

use std::path::Path;
use std::env;
use std::fs;
use std::io;

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

    process_dir_int(path, out_dir)?;

    Ok(())
}

fn process_dir_int(path: &Path, out_dir: &Path) -> io::Result<()> {
    println!("process dir {}", path.to_string_lossy());

    for entry in fs::read_dir(path)? {
        let entry = entry?;

        if entry.file_type()?.is_dir() {
            process_dir_int(&entry.path(), out_dir)?;
        } else if entry.path().extension().iter().any(|ext| *ext == "protogen") {
            process_file(&entry.path(), &out_dir)?;
        }
    };

    Ok(())
}

fn process_file(path: &Path, out_dir: &Path) -> io::Result<()>  {
    println!("processing {}", path.to_string_lossy());

    let data = fs::read(path)?;

    match parser::source_file(&data) {
        Ok((rest, messages)) => {
            let rest = String::from_utf8_lossy(&rest);

            if rest.trim().is_empty() {
                let name = path.file_stem().expect("not a file");
                let out_file = out_dir.join(Path::new(name)).with_extension("rs");
                fs::write(&out_file, convert_error(
                    generator::Generator::from_messages(messages))?.to_string())?;
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

    Ok(())
}
