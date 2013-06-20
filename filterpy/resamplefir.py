import fractions
from pylab import *
import numpy
import scipy.signal as signal

from_rate = 44100.0
to_rate = 48000.0 # 192000.0

rategcd = fractions.gcd(from_rate, to_rate)
samp_rate = from_rate * to_rate / rategcd
print("upsample rate: %f\n" % samp_rate)
ups_ratio = samp_rate / from_rate
dec_ratio = samp_rate / to_rate
print("ups: %f, dec: %f\n" % (ups_ratio, dec_ratio))

nyq_rate = samp_rate / 2.0

audible_freq = 22000.0

# width = 5.0/nyq_rate
width = 1000.0/nyq_rate
ripple_db = 60.0

N, beta = signal.kaiserord(ripple_db, width)
print("suggested N: %d, beta: %d" % (N,beta))
N = ups_ratio * 16
beta = 5
print("N: %d, beta: %d" % (N,beta))
print("polyphase depth: %f\n" % (N/ups_ratio))

reqmem = N * 16 / 1024.0 / 2;
print("reqmem: %fKb\n" % reqmem);

taps = signal.firwin(N, cutoff = audible_freq / nyq_rate, window = ('kaiser', beta))
tn = (taps * 0xffff).astype(numpy.int64)

f = open('filter.txt', 'w')
for t in tn:
	f.write("%d,\n" % t)
f.close()

# figure(1)
# plot(taps, 'bo-', linewidth=2)
# grid(True)
# 
# figure(2)
# clf()

if False:
	w, h = signal.freqz(taps, worN=8000)
	h_dB = 20 * log10(abs(h))
	plot((w/pi)*nyq_rate, h_dB, linewidth=2)
	xlabel("freq")
	ylabel("gain dB")
	xlim(0, 40000)
	ylim(-50, 1)
	grid(True)
	show()

from collections import deque

class FIR:
  def __init__(self, coeff):
    self.coeff = coeff
    self.depth = len(coeff)
    self.data = deque([0] * self.depth, self.depth)
    self.output = 0

  def push(self, s):
    self.data.append(s)
    self.output = 0
    for x, c in zip(self.data, self.coeff):
      self.output += x * c
    self.output = int(self.output / 1024)

  def appl(self, xs):
    out = 0
    for x, c in zip(xs, self.coeff):
      out += x * c
    return int(out / 1024)

class PolyphaseFilter:
  def __init__(self, coeff, m, d):
    self.coeff = coeff
    self.m = int(m)
    self.depth = int(len(self.coeff) / self.m)
    self.d = int(d)
    self.insel = 0
    self.outsel = 0
    self.firs = []
    for i in range(self.m):
      subcoeff = [self.coeff[i + j*self.m] for j in range(self.depth)]
      fir = FIR(subcoeff)
      self.firs.append(fir)

  def push(self, s):
    self.firs[self.insel].push(s)
    self.insel = (self.insel + 1) % self.m
  
  def output(self):
    fir = self.firs[self.outsel]
    out = fir.output
    self.outsel = (self.outsel + self.d) % self.m

    return out

pf = PolyphaseFilter(tn, ups_ratio, dec_ratio)

import wave
import struct

wi = wave.open("test.wav", 'r')
n = wi.getnframes()
# n = 50000

print("%d frames" % n)

ai = frombuffer(wi.readframes(n), dtype='int16')
ao = []
# for i in range(n):
#   if i % 10000 == 0:
#     print i
#   pf.push(ai[i])
#   if(len(ao) < (i-10)*(to_rate / from_rate)):
#     ao.append(ai[i])
#     ao.append(pf.output())

outn = int(n * to_rate / from_rate)
# outn = 100000
for i in range(outn):
  if i % 10000 == 0:
    print(i)

  fir = pf.firs[i % len(pf.firs)]
  
  ia = i * pf.d / pf.m

  firin = []
  for j in range(pf.depth):
    idx = ia-j
    if idx < 0 or idx >= n:
      s = 0
    else:
      s = ai[idx]
    firin.append(s)

  out = fir.appl(firin)
  ao.append(out)

wo = wave.open("out.wav", 'w')
wo.setnchannels(1)
wo.setsampwidth(2)
wo.setframerate(48000)
wo.writeframes(struct.pack('h' * len(ao), *ao))
wo.close()
