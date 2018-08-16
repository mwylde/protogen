include!(concat!(env!("OUT_DIR"), "/hci_message.rs"));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let msg = [0x01u8, 0x05, 0x0c, 0x01, 0x00];

        println!("{:?}", HciMessage::parse(&msg));
    }
}
