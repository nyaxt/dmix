import fractions
import math
import numpy
import scipy.signal as signal
import scipy.io.wavfile as wavfile
import matplotlib.pyplot as plot
import sys
import wave

# port of libresample to python for algorithm evaluation purpose
# https://github.com/minorninth/libresample
# 
# Original Copyright:
"""
Copyright (c) 2003, Dominic Mazzoni
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

from_rate = 44100.0
to_rate = 48000.0

numzc = 4096

def ratio_to_db(ratio):
  return 10 * math.log10(ratio)

def db_to_ratio(db):
  return math.pow(10, db / 10)

def gen_sin(db, freq, sampling_rate, t):
  amplitude = db_to_ratio(db)
  n = int(t * sampling_rate)
  ret = numpy.zeros(n)
  for i in xrange(n):
    theta = float(i) * freq * 2 * math.pi / sampling_rate
    ret[i] = math.sin(theta) * amplitude
  return ret

sin1khz = gen_sin(-0.1, 1000, from_rate, 0.1)
# w = wave.open("sin48k.wav", 'r')
# n = w.getnframes()
# sin1khz_48k = numpy.frombuffer(w.readframes(n), dtype='int16').astype(numpy.float32) / 0x7fff

def mk_filter(n, co, beta):
  c = numpy.zeros(n+1)
  c[0] = 2 * co
  for i in xrange(1, n):
    t = float(i) / numzc
    a = math.pi * t
    c[i] = math.sin(a) / a
    c[i] = math.sin(a * 2*co) / a # ?????
  k = numpy.kaiser(n*2+1, beta)
  for i in xrange(n):
    c[i] *= k[n+i]

  c[n] = 0
  return c

def innerp(c, nc, x, xi, phase, inc):
  phase = phase * numzc

  v = 0.0
  hpi = phase
  end = nc
  a = phase - math.floor(phase)
  
  if inc == 1:
    end -= 1
    if phase == 0.0:
      hpi += numzc

  while hpi < end:
    xs = x[xi]
    m = (1.0-a)*c[hpi] + a*c[hpi+1]
    v += m * xs
    hpi += numzc
    xi += inc

  return v

def resample(x, ratio):
  nmult = 11 # or 35
  n = numzc*(nmult-1)/2

  co = 0.9 /2
  beta = 6

  c = mk_filter(n, co, beta)
  if False:
    plot.plot(c)
    plot.show()

  xoff = float(nmult+1) / 2*max(1.0, 1.0/ratio) + 10;
  print("xoff: %d" % xoff)

  currTime = float(xoff)
  y = []

  # nx = len(x) - xoff
  # endTime = currTime + nx
  endTime = len(x) - currTime
  while currTime < endTime:
    leftPhase = currTime - math.floor(currTime)
    rightPhase = 1.0 - leftPhase

    xi = int(math.floor(currTime))
    print("%f / %f. xi: %d / %d phase: %f" % (currTime, endTime, xi, endTime, leftPhase))

    v = (innerp(c, n, x, xi, leftPhase, -1) + innerp(c, n, x, xi+1, rightPhase, 1))
    y.append(v)

    currTime += 1.0 / ratio

  return y

def plot_waveform(d, sampling_freq):
  t_xaxis = numpy.arange(len(d)) / sampling_freq
  plot.plot(t_xaxis, d, marker='o')

def plot_periodogram(d, sampling_freq, col='b'):
  f, p = signal.periodogram(d, sampling_freq, scaling='spectrum')
  db = numpy.log10(p) * 10
  plot.ylim([-180, 0])
  plot.xlim([0, 22000])
  # plot.xscale('log')
  plot.plot(f, db, col)

from scipy import fft, arange

def plotSpectrum(y,Fs):
  n = len(y) # lungime semnal
  k = arange(n)
  T = n/Fs
  frq = k/T # two sides frequency range
  frq = frq[range(n/2)] # one side frequency range

  Y = fft(y)/n # fft computing and normalization
  Y = Y[range(n/2)]
  db = numpy.log10(abs(Y)) * 10

  plot.plot(frq,db,'r') # plotting the spectrum
  plot.xlabel('Freq (Hz)')
  plot.ylabel('dB')

ratio = to_rate / from_rate 
y = resample(sin1khz, ratio)

# plot.subplot(211)
# plot_waveform(y, to_rate)

# plot.subplot(212)
# plot_periodogram(y, to_rate)
# plot_periodogram(sin1khz[1000:], from_rate, 'r')
# plot.show()

# plotSpectrum(sin1khz[1000:], from_rate)
plotSpectrum(y, from_rate)
plot.show()
