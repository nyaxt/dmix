import polyphase_resampler as pr

from_rate = 44100.
to_rate = 48000.0

f = pr.PolyphaseResampler(from_rate, to_rate)
f.export_c_header("../dspsw/filter.h")
