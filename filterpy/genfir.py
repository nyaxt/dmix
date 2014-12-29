import polyphase_resampler as pr

f = pr.PolyphaseResampler(32000.0, 48000.0, 32)
f.export_verilog_mem("../rom_firbank_32_48.v", "rom_firbank_32_48")

f = pr.PolyphaseResampler(44100.0, 48000.0, 32)
f.export_verilog_mem("../rom_firbank_441_480.v", "rom_firbank_441_480")

f = pr.PolyphaseResampler(48000.0, 96000.0, 32)
f.export_verilog_mem("../rom_firbank_48_96.v", "rom_firbank_48_96")

f = pr.PolyphaseResampler(96000.0, 192000.0, 16)
f.export_verilog_mem("../rom_firbank_96_192.v", "rom_firbank_96_192")
