import fractions
import math
import numpy
import scipy.signal as signal
import scipy.io.wavfile as wavfile
import matplotlib.pyplot as plot
import sys
import wave

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
width = 100.0/nyq_rate
ripple_db = 30.0

N, beta = signal.kaiserord(ripple_db, width)
print("suggested N: %d, beta: %d" % (N,beta))
depth = 20
beta = 6
N = depth * ups_ratio
print("N: %d, beta: %d" % (N,beta))
print("polyphase depth: %d\n" % (N/ups_ratio)) 

reqmem = N * 16 / 1024.0 / 2;
print("reqmem: %fKb\n" % reqmem)

w = ('kaiser', beta)
w = 'blackmanharris'
taps = signal.firwin(N, cutoff = audible_freq, window = w, nyq = nyq_rate)

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

def apply_filter(src, taps, ups, dec):
  ups = int(ups)
  dec = int(dec)
  depth = len(taps) / ups
  taps2 = numpy.append(taps, [0])
  dst = []
  firidx = 0
  si = 0
  while True:
    d = 0.0
    for j in xrange(depth):
      s = src[si + j]
      c = taps2[ups-1-firidx + j*ups]
      d += s * c
    d *= ups
    dst.append(d)
    firidx += dec
    if firidx >= ups:
      firidx -= ups
      si += 1
      if si > len(src) - depth:
        break

  return dst

def plot_filterfreqresp(taps, nyq_rate):
  w, h = signal.freqz(taps, worN=8000)
  h_dB = 20 * numpy.log10(abs(h))
  plot.plot((w/math.pi)*nyq_rate, h_dB)
  plot.xlabel("freq")
  plot.ylabel("gain dB")
  plot.xlim(0, 40000)
  plot.grid(True)

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

def findzc(d):
  s = 0
  for i in xrange(1, len(d)):
    if d[i-1] < 0 and d[i] > 0:
      s = i
      break

  e = 0
  for j in xrange(1, len(d)):
    i = len(d)-j
    if d[i-1] < 0 and d[i] > 0:
      e = i
      break

  return d[s:e]

sin1khz = gen_sin(-0.1, 1000, from_rate, 1)
res = apply_filter(sin1khz, taps, ups_ratio, dec_ratio)
print("done res");

plot.subplot(411)
plot.plot(taps)

plot.subplot(412)
plot_filterfreqresp(taps, nyq_rate)

plot.subplot(413)
# plot_waveform(sin1khz, from_rate)
plot_waveform(res[0:300], to_rate)

plot.subplot(414)
plot_periodogram(findzc(sin1khz[1000:len(sin1khz)-1000]), from_rate, 'r')
plot_periodogram(findzc(res), to_rate)

plot.show()
