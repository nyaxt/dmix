import polyphase_resampler as pr

from_rate = 44100.0
to_rate = 48000.0

f = pr.PolyphaseResampler(from_rate, to_rate)
f.test_sweep()
