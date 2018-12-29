#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bits() {
        let mut buf = BitBuffer::new();
        buf.push_bit(true);
        buf.push_bit(false);
        buf.push_bit(false);
        buf.push_bit(true);
        buf.push_bit(true);
        buf.push_bit(false);
        buf.push_bit(true);
        buf.push_bit(false);

        buf.push_bit(true);

        let vec = buf.into_vec();
        assert_eq!(2, vec.len());
        assert_eq!(0b01011001, vec[0]);
        assert_eq!(1, vec[1]);
    }

    #[test]
    fn test_u8() {
        let mut buf = BitBuffer::new();
        buf.push_u8(15);

        let vec = buf.into_vec();
        assert_eq!(1, vec.len());
        assert_eq!(15, vec[0]);

        let mut buf = BitBuffer::new();
        buf.push_bit(false);
        buf.push_u8(0b11110001u8);

        let vec = buf.into_vec();
        assert_eq!(2, vec.len());
        assert_eq!(0b11100010, vec[0]);
        assert_eq!(1, vec[1]);
    }

    #[test]
    fn test_u16_le() {
        let mut buf = BitBuffer::new();
        buf.push_u16_le(3523);
        assert_eq!(buf.into_vec(), &[195, 13]);
    }

    #[test]
    fn test_u32_le() {
        let mut buf = BitBuffer::new();
        buf.push_u32_le(41284875);
        assert_eq!(buf.into_vec(), &[11, 245, 117, 2]);
    }

    #[test]
    fn test_u64_le() {
        let mut buf = BitBuffer::new();
        buf.push_u64_le(17923220538470316381);
        assert_eq!(buf.into_vec(), &[93, 57, 130, 82, 30, 18, 188, 248]);
    }

    #[test]
    fn test_i8() {
        let mut buf = BitBuffer::new();
        buf.push_i8(-30);
        assert_eq!(buf.into_vec(), &[226]);
    }

    #[test]
    fn test_i16_le() {
        let mut buf = BitBuffer::new();
        buf.push_i16_le(-32350);
        assert_eq!(buf.into_vec(), &[162, 129]);
    }
}

pub struct BitBuffer {
    buf: Vec<u8>,
    position: usize,
}

impl BitBuffer {
    pub fn new() -> BitBuffer {
        BitBuffer {
            buf: vec![],
            position: 0,
        }
    }

    pub fn push_bit(&mut self, bit: bool) {
        println!("({}, {})", self.position, bit);
        if self.buf.len() < (self.position / 8) + 1 {
            self.buf.push(0);
        }

        // the buffer is guaranteed not to be empty by the previous statement
        let v = self.buf.last_mut().unwrap();

        let offset = self.position % 8;
        *v = if bit {
            *v | (1 << offset)
        } else {
            *v & !(1 << offset)
        };

        self.position += 1;
    }

    pub fn push_u8(&mut self, v: u8) {
        if self.position % 8 == 0 {
            self.buf.push(v);
            self.position += 8;
        } else {
            for i in 0u8..8 {
                self.push_bit(v & (1 << i) > 0)
            }
        }
    }

    pub fn push_bytes(&mut self, bytes: &[u8]) {
        if self.position % 8 == 0 {
            self.buf.extend_from_slice(bytes);
        } else {
            for b in bytes {
                self.push_u8(*b);
            }
        }
    }

    pub fn push_u16_le(&mut self, v: u16) {
        self.push_u8((v & 0xFF) as u8);
        self.push_u8((v >> 8) as u8);
    }

    pub fn push_u32_le(&mut self, v: u32) {
        self.push_u8((v & 0xFF) as u8);
        self.push_u8(((v >> 8) & 0xFF) as u8);
        self.push_u8(((v >> 16) & 0xFF) as u8);
        self.push_u8(((v >> 24) & 0xFF) as u8);
    }

    pub fn push_u64_le(&mut self, v: u64) {
        self.push_u8((v & 0xFF) as u8);
        self.push_u8(((v >> 8) & 0xFF) as u8);
        self.push_u8(((v >> 16) & 0xFF) as u8);
        self.push_u8(((v >> 24) & 0xFF) as u8);
        self.push_u8(((v >> 32) & 0xFF) as u8);
        self.push_u8(((v >> 40) & 0xFF) as u8);
        self.push_u8(((v >> 48) & 0xFF) as u8);
        self.push_u8(((v >> 56) & 0xFF) as u8);
    }

    pub fn push_i8(&mut self, v: i8) {
        self.push_u8(v as u8);
    }

    pub fn push_i16_le(&mut self, v: i16) {
        self.push_u16_le(v as u16);
    }

    pub fn push_i32_le(&mut self, v: i32) {
        self.push_u32_le(v as u32);
    }

    pub fn push_i64_le(&mut self, v: i64) {
        self.push_u64_le(v as u64);
    }

    pub fn into_vec(self) -> Vec<u8> {
        self.buf
    }
}
