include!(concat!(env!("OUT_DIR"), "/protogen.rs"));

#[cfg(test)]
mod tests {
    use crate::*;
    use protogen::State;

    #[test]
    fn test_simple() {
        let simple = Simple {
            _f1_u1: true,
            _f2_u1: false,
            _f3_u2: 3,
            _f4_u4: 13,
            _f5_u32: 43,
            _f6_u8_5: b"hello".to_vec(),
        };

        let s = simple.to_vec();

        let (_, simple2) = Simple::parse(State::from_slice(&s)).unwrap();

        assert_eq!(simple, simple2);
    }

    #[test]
    fn test_submessage() {
        let msg = Simple2 {
            _f1: 1241,
            _sub: Submessage {
                _id: b"hello".to_vec(),
            },
        };

        let s = msg.to_vec();
        let (_, msg2) = Simple2::parse(State::from_slice(&s)).unwrap();

        assert_eq!(msg, msg2);
    }

    #[test]
    fn test_vars() {
        let msg = Vars { _f2: 13 };

        let s = msg.to_vec();
        let (_, msg2) = Vars::parse(State::from_slice(&s)).unwrap();

        assert_eq!(msg, msg2);
    }

    #[test]
    fn test_vars2() {
        let msg = Vars2 {
            _data: b"this is my data!".to_vec(),
        };

        let s = msg.to_vec();
        let (_, msg2) = Vars2::parse(State::from_slice(&s)).unwrap();

        assert_eq!(msg, msg2);
    }
}
