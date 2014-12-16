import fractions
import math
import numpy
import scipy
import scipy.signal as signal
import matplotlib.pyplot as plot
import sys
import wave
import struct

def calc_samp_rate(from_rate, to_rate):
  rategcd = fractions.gcd(from_rate, to_rate)
  return from_rate * to_rate / rategcd

def calc_nyq_rate(samp_rate):
  return samp_rate / 2.0

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
  dst = []
  firidx = 0
  si = 0
  while True:
    d = 0.0
    for j in xrange(depth):
      s = src[si + j]
      c = taps[ups-1-firidx + j*ups]
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

def half_filter(taps):
  n = len(taps)
  return taps[n/2:n]

def reorder_half_filter(htaps, ups, dec):
  ups = int(ups)
  dec = int(dec)
  hn = len(htaps)
  hd = hn / ups
  depth = hd*2
  r = numpy.zeros(len(htaps))
  ri = 0
  for i in xrange(ups):
    for j in xrange(hd):
      r[ri] = htaps[ups-1-i + j*ups]
      ri += 1
  return r

def muladd(mcand, mplier, cross):
  if len(mcand) != len(mplier):
    raise ValueError('mcand and mplier len do not match!')
  n = len(mcand)

  ret = 0.0

  if cross:
    for i in xrange(n):
      ret += mcand[i] * mplier[n-1-i]
  else:
    for i in xrange(n):
      ret += mcand[i] * mplier[i]

  return ret

def apply_filter_half_reordered(src, rhtaps, ups, dec):
  ups = int(ups)
  dec = int(dec)
  hn = len(rhtaps)
  hd = hn / ups
  depth = hd * 2
  dst = []
  firidx = 0
  si = 0
  while True:
    d = 0.0

    # L
    o = hn-(firidx+1)*hd
    d += muladd(src[si:si+hd], rhtaps[o:o+hd], True)

    # R
    o = firidx*hd
    d += muladd(src[si+hd:si+hd*2], rhtaps[o:o+hd], False)

    d *= ups
    dst.append(d)
    firidx += dec
    if firidx >= ups:
      firidx -= ups
      si += 1
      if si > len(src) - depth:
        break

  return dst

def float_to_fixed(l, bits):
  scale = (1 << (bits-1)) - 1
  return [int(x * scale) for x in l]

def muladde(mcand, mplier, cross):
  if len(mcand) != len(mplier):
    raise ValueError('mcand and mplier len do not match!')
  n = len(mcand)
  
  ret = 0

  if cross:
    for i in xrange(n):
      ret += mcand[i] * mplier[n-1-i]
  else:
    for i in xrange(n):
      ret += mcand[i] * mplier[i]

  return ret

def apply_filter_half_reordered_emu(srci, rhetaps, ups, dec):
  ups = int(ups)
  dec = int(dec)
  hn = len(rhetaps)
  hd = hn / ups
  depth = hd * 2
  dst = []
  firidx = 0
  si = 0
  while True:
    if (si & (1024-1) == 0):
      print("%d/%d" % (si, len(srci)))

    d = 0

    # L
    o = hn-(firidx+1)*hd
    d += muladde(srci[si:si+hd], rhetaps[o:o+hd], True)

    # R
    o = firidx*hd
    d += muladde(srci[si+hd:si+hd*2], rhetaps[o:o+hd], False)

    dst.append(d)
    firidx += dec
    if firidx >= ups:
      firidx -= ups
      si += 1
      if si > len(srci) - depth:
        break

  return dst

def plot_filterfreqresp(taps, nyq_rate):
  w, h = signal.freqz(taps, worN=8000)
  h_dB = 20 * numpy.log10(abs(h))
  plot.plot((w/math.pi)*nyq_rate, h_dB)
  plot.xlabel("freq")
  plot.ylabel("gain dB")
  plot.xlim(0, 40000)
  plot.ylim(-100, 5)
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

def readwav(wav_filepath):
  w = wave.open(wav_filepath, 'r')
  n = w.getnframes()
  return numpy.frombuffer(w.readframes(n), dtype='int16')

class PolyphaseResampler:
  def __init__(self, from_rate, to_rate):
    self.from_rate = from_rate
    self.to_rate = to_rate

    samp_rate = calc_samp_rate(self.from_rate, self.to_rate)
    nyq_rate = calc_nyq_rate(samp_rate)
    print("upsample rate: %f kHz\n" % (samp_rate/1000))
    self.ups_ratio = int(samp_rate / self.from_rate)
    self.dec_ratio = int(samp_rate / self.to_rate)
    print("ups: %d, dec: %d\n" % (self.ups_ratio, self.dec_ratio))

    # width = 5.0/nyq_rate
    width = 100.0/nyq_rate
    ripple_db = 30.0
    N, beta = signal.kaiserord(ripple_db, width)
    print("suggested N: %d, beta: %d" % (N,beta))

    depth = 32
    beta = 6
    N = depth * self.ups_ratio
    print("N: %d, beta: %d" % (N,beta))
    print("polyphase depth: %d\n" % (N/self.ups_ratio))

    # reqmem = N * 16 / 1024.0 / 2;
    # print("reqmem: %fKb\n" % reqmem)

    audible_freq = 22000.0
    w = 'blackmanharris'
    # w = ('kaiser', beta)
    self.taps = signal.firwin(N, cutoff = audible_freq, window = w, nyq = nyq_rate)

    self.htaps = half_filter(self.taps)
    self.rhtaps = reorder_half_filter(self.htaps, self.ups_ratio, self.dec_ratio)

    self.tapsbits = 24
    self.rhetaps = float_to_fixed(self.rhtaps * self.ups_ratio, self.tapsbits)

    self.srcbits = 24

  def resample(self, x):
    xi = float_to_fixed(x, self.srcbits)
    resi = apply_filter_half_reordered_emu(xi, self.rhetaps, self.ups_ratio, self.dec_ratio)

    scale = 1.0 / ((1 << (self.srcbits-1 + self.tapsbits-1)) - 1)
    res = [float(x) * scale for x in resi]
    return res

  def convwav(self, wav_filepath):
    x = readwav(wav_filepath)

    xi = [i * (1<<8) for i in x]
    xi = x

    self.srcbits = 24
    res = apply_filter_half_reordered_emu(xi, self.rhetaps, self.ups_ratio, self.dec_ratio)

    scale = 1.0 / ((1 << (self.srcbits-1 + self.tapsbits-1)) - 1)
    y = [i >> (8+self.tapsbits-1) for i in res]

    wo = wave.open("out.wav", 'w')
    wo.setnchannels(1)
    wo.setsampwidth(2)
    wo.setframerate(48000)
    wo.writeframes(struct.pack('h' * len(y), *y))
    wo.close()

  def export_c_header(self, header_filepath):
    f = open(header_filepath, 'w')
    f.write("int rhtaps_441_48[] = {\n")
    for c in self.rhetaps:
      f.write("  %d,\n" % c)
    f.write("""};

polyphase_filter_t filter_441_48 = {rhtaps_441_48, %d, %d, %d, %d};
""" % (len(self.rhetaps), self.ups_ratio, self.dec_ratio, len(self.rhetaps) / self.ups_ratio))
    f.close()

  def export_verilog_mem(self, header_filepath):
    f = open(header_filepath, 'w')
    addrbus_width = int(math.ceil(math.log(len(self.rhetaps), 2)))
    f.write("""
module rom_firbank_441_480(
    input clk,
    input [{0}:0] addr,
    output [{1}:0] data);
reg [{1}:0] data_ff;
assign data = data_ff;
always @(posedge clk) begin
    case(addr)
""".format(addrbus_width-1, self.tapsbits-1))
    i = 0
    for c in self.rhetaps:
      flag = "" if c > 0 else "-"
      f.write("        {0}: data_ff <= {1}{2}'d{3};\n".format(i, flag, self.tapsbits, abs(c)))
      i += 1
    f.write("""
        default: data_ff <= 0;
    endcase
end
endmodule
""")
    f.close()

  def test_sin1khz(self):
    sin1khz = gen_sin(-0.1, 1000, self.from_rate, 10)
    res = self.resample(sin1khz)
    print("done res")

    plot.subplot(411)
    plot.plot(self.taps)

    plot.subplot(412)
    plot_filterfreqresp(self.taps, calc_nyq_rate(calc_samp_rate(self.from_rate, self.to_rate)))

    plot.subplot(413)
    # plot_waveform(sin1khz, self.from_rate)
    plot_waveform(res[0:300], self.to_rate)

    plot.subplot(414)
    plot_periodogram(findzc(sin1khz[1000:len(sin1khz)-1000]), self.from_rate, 'r')
    plot_periodogram(findzc(res), self.to_rate)

    plot.show()

  def test_sweep(self):
    amp = db_to_ratio(-6)
    te = 8
    t = numpy.linspace(0, 8, self.from_rate*8)
    x = signal.chirp(t, f0=0, f1=self.from_rate, t1=te, method='quadratic') * amp
    print("done chirp")

    y = self.resample(x)
    print("done filtering")

    nfft = 64
    win = scipy.hamming(nfft)

    plot.subplot(211)
    plot.specgram(x, NFFT=nfft, Fs=self.from_rate, noverlap=nfft/2, window=win)

    plot.subplot(212)
    plot.specgram(y, NFFT=nfft, Fs=self.to_rate, noverlap=nfft/2, window=win)

    plot.show()
