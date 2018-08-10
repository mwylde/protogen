use nom::{IResult, le_u8};

//
//
//impl HciMessage {
//    fn message_type(&self) -> &u8 {
//        &self.message_type
//    }
//
//    pub fn message(&self) -> &HciMessage_Message {
//        &self.message
//    }
//
//    pub fn parse(i: &[u8]) -> IResult<&[u8], HciMessage> {
//        let (i, message_type) = try_parse!(i, le_u8);
//        let (i, message) = try_parse!(i, alt!(
//            HciCommand::parse => {|v| HciMessage_Message::HciCommand(v)} |
//            HciData::parse => {|v| HciMessage_Message::HciData(v)}
//        ));
//        Ok((i, HciMessage {
//            message_type, message
//        }))
//    }
//}
