import fractions
import math
from pylab import *
import numpy
import scipy.signal as signal

from_rate = 44100.0
to_rate = 48000.0

rategcd = fractions.gcd(from_rate, to_rate)
samp_rate = from_rate * to_rate / rategcd
print("upsample rate: %f kHz\n" % (samp_rate/1000))
ups_ratio = int(samp_rate / from_rate)
dec_ratio = int(samp_rate / to_rate)
print("ups: %d, dec: %d\n" % (ups_ratio, dec_ratio))

nyq_rate = samp_rate / 2.0

audible_freq = 22000.0

# width = 5.0/nyq_rate
width = 1000.0/nyq_rate
ripple_db = 60.0

N, beta = signal.kaiserord(ripple_db, width)
print("suggested N: %d, beta: %d" % (N,beta))
depth = 16
beta = 5
N = depth * ups_ratio
print("N: %d, beta: %d" % (N,beta))
print("polyphase depth: %d\n" % depth) 

reqmem = N * 16 / 1024.0 / 2;
print("reqmem: %fKb\n" % reqmem)

taps = signal.firwin(N, cutoff = audible_freq / nyq_rate, window = ('kaiser', beta))
firbank = numpy.zeros(N, numpy.int64)
scale = 0x7fff * ups_ratio
for i in range(ups_ratio):
  for j in range(depth):
    firbank[i*depth + depth-1 - j] = taps[i + j*ups_ratio] * scale

f = open('filter.txt', 'w')
name = sys.argv[1]
buswidth = math.log(firbank.size, 2) - 1
header ="""
module rom_firbank_%s(
    input [%d:0] addr,
    output [15:0] data);

reg [15:0] data_reg;
assign data = data_reg;
always @(addr) begin
    case(addr)
""" % (name, buswidth)
footer = """
    endcase
end
endmodule
"""
f.write(header)
for i in range(firbank.size):
  e = firbank[i]
  flag = "-" if (e < 0) else ""
  f.write("        %d: data_reg = %s16'd%d;\n" % (i, flag, abs(e)))
f.write(footer)
f.close()

if False:
  w, h = signal.freqz(taps, worN=8000)
  h_dB = 20 * log10(abs(h))
  plot((w/pi)*nyq_rate, h_dB, linewidth=2)
  xlabel("freq")
  ylabel("gain dB")
  xlim(0, 40000)
  # ylim(-50, 50)
  grid(True)
  show()
  sys.exit()

import wave
import struct

wi = wave.open("test.wav", 'r')
n = wi.getnframes()
# n = 50000

print("%d frames" % n)

ai = frombuffer(wi.readframes(n), dtype='int16')
ao = []

outn = int(n * to_rate / from_rate)
outn = 200000
isrc = 0
pop_counter = 0
for i in range(outn):
  if i % 10000 == 0:
    print(i)

  firidx = i % ups_ratio
  fboff = firidx * depth
  
  pop_counter += dec_ratio
  if(pop_counter > ups_ratio):
    isrc += 1
    pop_counter -= ups_ratio

  out = 0.0
  for j in range(depth):
    idx = isrc - j
    if idx < 0 or idx >= n:
      s = 0
    else:
      s = ai[idx]

    out += firbank[fboff+j] * s

  out /= 0x7fff

  if(out > 0x7fff):
    out = 0x7fff
  if(out < -0x7fff):
    out = -0x7fff

  ao.append(out)

wo = wave.open("out.wav", 'w')
wo.setnchannels(1)
wo.setsampwidth(2)
wo.setframerate(to_rate)
wo.writeframes(struct.pack('h' * len(ao), *ao))
wo.close()
