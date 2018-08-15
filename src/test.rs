use nom::{IResult, le_u8, le_u16};
use nom;

pub struct FilterCondition {
    _condition_type: u8,
    _value: FilterCondition_Value,
}

pub struct HciMessage {
    _message_type: u8,
    _message: HciMessage_Message,
}

pub struct AllDevices {
}

pub struct HciData {
}

pub struct SetEventFilter {
    _filter_type: u8,
    _filter: SetEventFilter_Filter,
}

pub struct Reset {
}

pub struct MatchClass {
    _class_of_device: Vec<u8>,
    _class_of_device_mask: Vec<u8>,
}

pub struct InquiryResult {
    _condition: FilterCondition,
}

pub struct HciCommand {
    _ocf: u16,
    _length: u8,
    _data: Vec<u8>,
    _command: HciCommand_Command,
}

pub struct ConnectionSetup {
    _condition: FilterCondition,
    _auto_accept: u8,
}

pub struct ClearAllFilter {
}

pub struct MatchAddress {
    _address: Vec<u8>,
}

impl MatchClass {
    pub fn get_class_of_device(&self) -> &Vec<u8> {
        &self._class_of_device
    }

    pub fn get_class_of_device_mask(&self) -> &Vec<u8> {
        &self._class_of_device_mask
    }

    pub fn parse(_i0: &[u8], _condition_type: u8) -> IResult<&[u8], MatchClass> {
        if _condition_type != 0x1 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        let (_i1, _class_of_device) = try_parse!(_i0, count!(le_u8, 3));
        let (_i2, _class_of_device_mask) = try_parse!(_i1, count!(le_u8, 3));
        Ok((_i2, MatchClass { _class_of_device, _class_of_device_mask }))
    }

}

impl FilterCondition {
    pub fn get_value(&self) -> &FilterCondition_Value {
        &self._value
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], FilterCondition> {
        let (_i1, _condition_type) = try_parse!(_i0, le_u8);
        let (_i2, _value) = try_parse!(_i1, alt!(
            call!(AllDevices::parse, _condition_type) => {|v| FilterCondition_Value::AllDevices(v)} |
            call!(MatchClass::parse, _condition_type) => {|v| FilterCondition_Value::MatchClass(v)} |
            call!(MatchAddress::parse, _condition_type) => {|v| FilterCondition_Value::MatchAddress(v)}
    ));
        Ok((_i2, FilterCondition { _condition_type, _value }))
    }

}

impl ConnectionSetup {
    pub fn get_condition(&self) -> &FilterCondition {
        &self._condition
    }

    pub fn get_auto_accept(&self) -> &u8 {
        &self._auto_accept
    }

    pub fn parse(_i0: &[u8], _filter_type: u8) -> IResult<&[u8], ConnectionSetup> {
        if _filter_type != 0x2 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        let (_i1, _condition) = try_parse!(_i0, FilterCondition::parse);
        let (_i2, _auto_accept) = try_parse!(_i1, le_u8);
        Ok((_i2, ConnectionSetup { _condition, _auto_accept }))
    }

}

impl SetEventFilter {
    pub fn get_filter(&self) -> &SetEventFilter_Filter {
        &self._filter
    }

    pub fn parse(_i0: &[u8], _ocf: u16) -> IResult<&[u8], SetEventFilter> {
        if _ocf != 0x5 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        let (_i1, _filter_type) = try_parse!(_i0, le_u8);
        let (_i2, _filter) = try_parse!(_i1, alt!(
            call!(ClearAllFilter::parse, _filter_type) => {|v| SetEventFilter_Filter::ClearAllFilter(v)} |
            call!(InquiryResult::parse, _filter_type) => {|v| SetEventFilter_Filter::InquiryResult(v)} |
            call!(ConnectionSetup::parse, _filter_type) => {|v| SetEventFilter_Filter::ConnectionSetup(v)}
    ));
        Ok((_i2, SetEventFilter { _filter_type, _filter }))
    }

}

impl HciMessage {
    pub fn get_message(&self) -> &HciMessage_Message {
        &self._message
    }

    pub fn parse(_i0: &[u8]) -> IResult<&[u8], HciMessage> {
        let (_i1, _message_type) = try_parse!(_i0, le_u8);
        let (_i2, _message) = try_parse!(_i1, alt!(
            call!(HciCommand::parse, _message_type) => {|v| HciMessage_Message::HciCommand(v)} |
            call!(HciData::parse, _message_type) => {|v| HciMessage_Message::HciData(v)}
    ));
        Ok((_i2, HciMessage { _message_type, _message }))
    }

}

impl InquiryResult {
    pub fn get_condition(&self) -> &FilterCondition {
        &self._condition
    }

    pub fn parse(_i0: &[u8], _filter_type: u8) -> IResult<&[u8], InquiryResult> {
        if _filter_type != 0x1 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        let (_i1, _condition) = try_parse!(_i0, FilterCondition::parse);
        Ok((_i1, InquiryResult { _condition }))
    }

}

impl ClearAllFilter {
    pub fn parse(_i0: &[u8], _filter_type: u8) -> IResult<&[u8], ClearAllFilter> {
        if _filter_type != 0x0 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        Ok((_i0, ClearAllFilter {  }))
    }

}

impl Reset {
    pub fn parse(_i0: &[u8], _ocf: u16) -> IResult<&[u8], Reset> {
        if _ocf != 0x3 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        Ok((_i0, Reset {  }))
    }

}

impl AllDevices {
    pub fn parse(_i0: &[u8], _condition_type: u8) -> IResult<&[u8], AllDevices> {
        if _condition_type != 0x0 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        Ok((_i0, AllDevices {  }))
    }

}

impl HciData {
    pub fn parse(_i0: &[u8], _type: u8) -> IResult<&[u8], HciData> {
        if _type != 0x0 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        Ok((_i0, HciData {  }))
    }

}

impl HciCommand {
    pub fn get_command(&self) -> &HciCommand_Command {
        &self._command
    }

    pub fn parse(_i0: &[u8], _type: u8) -> IResult<&[u8], HciCommand> {
        if _type != 0x1 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        let (_i1, _ocf) = try_parse!(_i0, le_u16);
        let (_i2, _length) = try_parse!(_i1, le_u8);
        let (_i3, _data) = try_parse!(_i2, count!(le_u8, _length as usize));
        let (_, _command) = try_parse!(&_i2[.._length as usize], alt!(
            call!(Reset::parse, _ocf) => {|v| HciCommand_Command::Reset(v)} |
            call!(SetEventFilter::parse, _ocf) => {|v| HciCommand_Command::SetEventFilter(v)}
    ));
        Ok((_i3, HciCommand { _ocf, _length, _data, _command }))
    }

}

impl MatchAddress {
    pub fn get_address(&self) -> &Vec<u8> {
        &self._address
    }

    pub fn parse(_i0: &[u8], _condition_type: u8) -> IResult<&[u8], MatchAddress> {
        if _condition_type != 0x2 {
            return Err(nom::Err::Error(nom::Context::Code(_i0, nom::ErrorKind::Tag)));
        }
        let (_i1, _address) = try_parse!(_i0, count!(le_u8, 6));
        Ok((_i1, MatchAddress { _address }))
    }

}

pub enum SetEventFilter_Filter {
    ClearAllFilter(ClearAllFilter),
    InquiryResult(InquiryResult),
    ConnectionSetup(ConnectionSetup),
}

pub enum FilterCondition_Value {
    AllDevices(AllDevices),
    MatchClass(MatchClass),
    MatchAddress(MatchAddress),
}

pub enum HciMessage_Message {
    HciCommand(HciCommand),
    HciData(HciData),
}

pub enum HciCommand_Command {
    Reset(Reset),
    SetEventFilter(SetEventFilter),
}