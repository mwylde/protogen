use crate::wave::Subchunk_Subchunk;
use rodio::buffer::SamplesBuffer;
use rodio::Source;
use std::thread::sleep;
use std::time::Duration;

#[allow(dead_code)]
mod tcp;
#[allow(dead_code)]
mod wave;

pub fn main() {
    use protogen::State;
    use std::fs::File;
    use std::io::Read;
    use wave::Wave;

    let mut file = File::open("examples/resources/sine.wav").unwrap();
    let mut buf: Vec<u8> = vec![];
    file.read_to_end(&mut buf).unwrap();

    println!("len = {}", buf.len());

    let state = State::from_slice(&buf);
    println!("state size = {}", state.data.len());
    let (state, wav) = Wave::parse(state).unwrap();
    assert_eq!(state.offset, buf.len());
    println!("format: {}", wav.get_audio_format());
    println!("bits per sample: {}", wav.get_bits_per_sample());

    assert_eq!(wav.get_chunks().len(), 1);

    let device = rodio::default_output_device().unwrap();
    let buffer = match wav.get_chunks()[0].get_subchunk() {
        Subchunk_Subchunk::U16DataSubchunk(b) => {
            SamplesBuffer::new(wav.get_num_channels(), wav.get_sample_rate(), b.get_data())
        }
        _ => unimplemented!(),
    };

    rodio::play_raw(&device, buffer.convert_samples());

    sleep(Duration::from_secs(5));
}
