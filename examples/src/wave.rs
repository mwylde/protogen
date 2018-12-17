include!(concat!(env!("OUT_DIR"), "/wave.rs"));

#[cfg(test)]
mod tests {
    use wave::*;
    use std::path::Path;
    use std::fs::File;
    use std::io::Read;
    use std::str;

    #[test]
    fn parse() {
        let path = Path::new("/home/mwylde/Downloads/M1F1-AlawWE-AFsp.wav");

        let mut file = File::open(&path).unwrap();
        let mut buf: Vec<u8> = vec![];
        file.read_to_end(&mut buf).unwrap();

        let parsed = Wave::parse(&buf).unwrap().1;
        println!("Format: {}", str::from_utf8(parsed.get_format()).unwrap());

        for chunk in parsed.get_chunks() {
            println!("Chunk: {:?}", chunk);
        }
    }
}
