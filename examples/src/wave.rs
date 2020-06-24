use protogen::buffer::BitBuffer;
include!(concat!(env!("OUT_DIR"), "/wave.rs"));

#[cfg(test)]
mod tests {
    use crate::wave::{Subchunk, Subchunk_Subchunk, U16DataSubchunk, Wave};
    use protogen::buffer::BitBuffer;

    #[test]
    pub fn test() {
        let wave = Wave {
            _chunk_id: b"RIFF".to_vec(),
            _wave_id: b"WAVE".to_vec(),
            _fmt_id: b"fmt ".to_vec(),
            _audio_format: 1,
            _num_channels: 2,
            _sample_rate: 44100,
            _byte_rate: 0,
            _block_align: 0,
            _bits_per_sample: 16,
            _format_rest: vec![],
            _chunks: vec![Subchunk {
                //_bits_per_sample: 16,
                _subchunk: Subchunk_Subchunk::U16DataSubchunk(U16DataSubchunk {
                    _data: vec![1u16, 2, 3, 4],
                }),
            }],
        };

        // let buf = wave.to_vec();

        //println!("vec: {:?}", buf);
    }
}

pub fn subchunk_to_vec(subchunk: &Subchunk, buf: &mut BitBuffer) {}

// pub fn to_vec(wav: &Wave, buf: &mut BitBuffer) {
//     buf.push_bytes(&wav._chunk_id);
//     // @data_size = (@chunk_size - @fmt_size) - 12
//     // @data_size + 12 + @fmt_size = @chunk_size
//     // @data_size = len(@data)
//
//     // len(@data) + 12 + @fmt_size = @chunk_size
//     // @fmt_size = len(format_rest) + 16
//     // @chunk_size = len(@data) + 12 + len(format_rest) + 16
//
//     let mut _data_buf = BitBuffer::new();
//     for subchunk in &wav._chunks {
//         subchunk_to_vec(subchunk, &mut _data_buf);
//     }
//     let _data = _data_buf.into_vec();
//
//     // chunk_size
//     buf.push_u32_le((_data.len() + 12 + wav._format_rest.len() + 16) as u32);
//
//     buf.push_bytes(&wav._wave_id);
//     buf.push_bytes(&wav._fmt_id);
//     // fmt_size
//
//     // @fmt_size = len(format_rest) + 16
//     buf.push_u32_le((wav._format_rest.len() + 16) as u32);
//
//     buf.push_u16_le(wav._audio_format);
//     buf.push_u16_le(wav._num_channels);
//     // simple cases
//
//     buf.push_bytes(&_data);
// }

/*
chunk_id = b"RIFF"

 */
