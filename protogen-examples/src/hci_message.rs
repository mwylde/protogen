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
        println!("{:?}", parsed);
    }

    #[test]
    fn test_le_advertising_report() {
        println!("doing test");
        let data = "04 3e 28 02 01 02 01 32 9c f0 06 23 68 1c 03 03\
                         9f fe 17 16 9f fe 00 00 00 00 00 00 00 00 00 00\
                         00 00 00 00 00 00 00 00 00 00 b0";

        let msg = hex::decode(data.replace(" ", "")).unwrap();

        let parsed = HciMessage::parse(&msg).unwrap();
        println!("{:?}", parsed);

    }
}

