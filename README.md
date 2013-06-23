# dmix

dmix is a FPGA-based digital mixer.

## License

MIT License

## List of modules

- dac_drv.v
    - output I2S-like but 24bit MSB first format accepted by FA1242
- fa1242.v
    - reset FA1242 chip
- mixer.v
    - mix multiple 2ch audio streams w/ attenuators
- mpemu.v
    - multiplier IP core emulator
- posedge_latch.v
    - latch wb signal on posedge
    - used to interface modules w/ different clocks
- resampler.v
    - resample audio stream using polyphase FIR filters
- ringbuf.v
    - ringbuf for buffering audio data avoiding jitters
    - also serves as a delay line for FIR filters
- rom_firbank_441_480.v
    - polyphase FIR bank for 44.1kHz -> 48.0kHz resampling
- rom_firbank_half.v
    - polyphase FIR bank for halfband filter
- spdif_dai.v
    - decode S/PDIF signal
- synth.v
    - sawwave synth for test
