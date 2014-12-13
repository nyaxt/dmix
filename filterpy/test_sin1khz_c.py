import struct
import subprocess
import matplotlib.pyplot as plot

import polyphase_resampler as pr

from_rate = 44100.0
to_rate = 48000.0

def gen_sin1khz_dat():
  a = pr.gen_sin(-0.1, 1000, from_rate, 1)
  a = [int(e * 0x7fffff) for e in a]

  bo = open("sin1khz.dat", 'w')
  bo.write(struct.pack('I', len(a)))
  for e in a:
    bo.write(struct.pack('i', e))
  bo.close()

def read_dat(filepath):
  bi = open(filepath, 'r')
  n = struct.unpack('I', bi.read(4))[0]
  ai = struct.unpack('i'*n, bi.read(4*n))
  return [float(e) / 0x7fffff for e in ai]

gen_sin1khz_dat()
subprocess.call(['../dspsw/resample', 'sin1khz.dat', 'sin1khz.out.dat'])
y = read_dat('sin1khz.out.dat')
pr.plot_periodogram(pr.findzc(y), to_rate, 'r')
plot.show()
