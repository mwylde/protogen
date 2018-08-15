use nom::{IResult, le_u8, le_u16};
use nom;

pub struct Reset {
}

pub struct AllDevices {
}

pub struct InquiryResult {
    condition: FilterCondition,
}

pub struct FilterCondition {
    condition_type: u8,
    value: FilterCondition_Value,
}

pub struct HciMessage {
    message_type: u8,
    message: HciMessage_Message,
}

pub struct MatchAddress {
    address: Vec<u8>,
}

pub struct ConnectionSetup {
    condition: FilterCondition,
    auto_accept: u8,
}

pub struct HciData {
}

pub struct MatchClass {
    class_of_device: Vec<u8>,
    class_of_device_mask: Vec<u8>,
}

pub struct SetEventFilter {
    filter_type: u8,
    filter: SetEventFilter_Filter,
}

pub struct ClearAllFilter {
}

pub struct HciCommand {
    ocf: u16,
    length: u8,
    data: Vec<u8>,
    command: HciCommand_Command,
}

impl MatchAddress {
    pub fn address(&self) -> &Vec<u8> {
        &self.address
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], MatchAddress> {
        let (_i1, address) = try_parse!(_i0, count!(le_u8, 6));
        Ok((_i1, MatchAddress { address }))
    }

}

impl ConnectionSetup {
    pub fn condition(&self) -> &FilterCondition {
        &self.condition
    }

    pub fn auto_accept(&self) -> &u8 {
        &self.auto_accept
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], ConnectionSetup> {
        let (_i1, condition) = try_parse!(_i0, FilterCondition::parse);
        let (_i2, auto_accept) = try_parse!(_i1, le_u8);
        Ok((_i2, ConnectionSetup { condition, auto_accept }))
    }

}

impl HciData {
    pub fn parse(_i0: &[u8]) -> IResult<&[u8], HciData> {
        Ok((_i0, HciData {  }))
    }

}

impl Reset {
    pub fn parse(_i0: &[u8]) -> IResult<&[u8], Reset> {
        Ok((_i0, Reset {  }))
    }

}

impl SetEventFilter {
    fn filter_type(&self) -> &u8 {
        &self.filter_type
    }

    pub fn filter(&self) -> &SetEventFilter_Filter {
        &self.filter
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], SetEventFilter> {
        let (_i1, filter_type) = try_parse!(_i0, le_u8);
        let (_i2, filter) = try_parse!(_i1, alt!(
            ClearAllFilter::parse => {|v| SetEventFilter_Filter::ClearAllFilter(v)} |
            InquiryResult::parse => {|v| SetEventFilter_Filter::InquiryResult(v)} |
            ConnectionSetup::parse => {|v| SetEventFilter_Filter::ConnectionSetup(v)}
    ));
        Ok((_i2, SetEventFilter { filter_type, filter }))
    }

}

impl FilterCondition {
    fn condition_type(&self) -> &u8 {
        &self.condition_type
    }

    pub fn value(&self) -> &FilterCondition_Value {
        &self.value
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], FilterCondition> {
        let (_i1, condition_type) = try_parse!(_i0, le_u8);
        let (_i2, value) = try_parse!(_i1, alt!(
            AllDevices::parse => {|v| FilterCondition_Value::AllDevices(v)} |
            MatchClass::parse => {|v| FilterCondition_Value::MatchClass(v)} |
            MatchAddress::parse => {|v| FilterCondition_Value::MatchAddress(v)}
    ));
        Ok((_i2, FilterCondition { condition_type, value }))
    }

}

impl ClearAllFilter {
    pub fn parse(_i0: &[u8]) -> IResult<&[u8], ClearAllFilter> {
        Ok((_i0, ClearAllFilter {  }))
    }

}

impl MatchClass {
    pub fn class_of_device(&self) -> &Vec<u8> {
        &self.class_of_device
    }

    pub fn class_of_device_mask(&self) -> &Vec<u8> {
        &self.class_of_device_mask
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], MatchClass> {
        let (_i1, class_of_device) = try_parse!(_i0, count!(le_u8, 3));
        let (_i2, class_of_device_mask) = try_parse!(_i1, count!(le_u8, 3));
        Ok((_i2, MatchClass { class_of_device, class_of_device_mask }))
    }

}

impl InquiryResult {
    pub fn condition(&self) -> &FilterCondition {
        &self.condition
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], InquiryResult> {
        let (_i1, condition) = try_parse!(_i0, FilterCondition::parse);
        Ok((_i1, InquiryResult { condition }))
    }

}

impl HciMessage {
    fn message_type(&self) -> &u8 {
        &self.message_type
    }

    pub fn message(&self) -> &HciMessage_Message {
        &self.message
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], HciMessage> {
        let (_i1, message_type) = try_parse!(_i0, le_u8);
        let (_i2, message) = try_parse!(_i1, alt!(
            HciCommand::parse => {|v| HciMessage_Message::HciCommand(v)} |
            HciData::parse => {|v| HciMessage_Message::HciData(v)}
    ));
        Ok((_i2, HciMessage { message_type, message }))
    }

}

impl AllDevices {
    pub fn parse(_i0: &[u8]) -> IResult<&[u8], AllDevices> {
        Ok((_i0, AllDevices {  }))
    }

}

impl HciCommand {
    fn ocf(&self) -> &u16 {
        &self.ocf
    }

    fn length(&self) -> &u8 {
        &self.length
    }

    fn data(&self) -> &Vec<u8> {
        &self.data
    }

    pub fn command(&self) -> &HciCommand_Command {
        &self.command
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], HciCommand> {
        let (_i1, ocf) = try_parse!(_i0, le_u16);
        let (_i2, length) = try_parse!(_i1, le_u8);
        let (_i3, data) = try_parse!(_i2, count!(le_u8, length as usize));
        let (_, command) = try_parse!(&_i2[..length as usize], alt!(
            Reset::parse => {|v| HciCommand_Command::Reset(v)} |
            SetEventFilter::parse => {|v| HciCommand_Command::SetEventFilter(v)}
    ));
        Ok((_i3, HciCommand { ocf, length, data, command }))
    }

}

pub enum HciCommand_Command {
    Reset(Reset),
    SetEventFilter(SetEventFilter),
}

pub enum FilterCondition_Value {
    AllDevices(AllDevices),
    MatchClass(MatchClass),
    MatchAddress(MatchAddress),
}

pub enum SetEventFilter_Filter {
    ClearAllFilter(ClearAllFilter),
    InquiryResult(InquiryResult),
    ConnectionSetup(ConnectionSetup),
}

pub enum HciMessage_Message {
    HciCommand(HciCommand),
    HciData(HciData),
}
