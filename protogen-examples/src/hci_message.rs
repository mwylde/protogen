include!(concat!(env!("OUT_DIR"), "/protogen-examples.rs"));

#[cfg(test)]
mod tests {
    use hex;
    use super::*;

    #[test]
    fn test_event_filter() {
        // let msg = [0x01u8, 0x05, 0x00, 0x01, 0x00];
        let msg = hex::decode("01050c0100").unwrap();

        println!("{:?}", HciMessage::parse(&msg));
    }

    #[test]
    fn test_local_name() {
        let msg = hex::decode("\
        011300f84368726f6d654c696e75785f37354632000000000000000000000000000000000000000000000000000\
        0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
        0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
        0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
        0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\
        0000000000000000000000000000000000000000000000000").unwrap();

        let parsed = HciMessage::parse(&msg);
        println!("{:?}", parsed);
    }

    #[test]
    fn test_unknown_command() {
        let msg = hex::decode("040e0601120c000000").unwrap();

        let parsed = HciMessage::parse(&msg);
        print!("{:?}", parsed);
    }
}

