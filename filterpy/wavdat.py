import sys
import wave
import struct

infile = sys.argv[1]
outfile = sys.argv[2]

if infile.endswith("wav"):
  wi = wave.open(infile, 'r')
  n = wi.getnframes()
  ai = struct.unpack('h'*n, wi.readframes(n))

  bo = open(outfile, 'w')
  bo.write(struct.pack('I', n))
  for e in ai:
    bo.write(struct.pack('i', e))
  bo.close()
else:
  bi = open(infile, 'r')
  n = struct.unpack('I', bi.read(4))[0]
  ai = struct.unpack('i'*n, bi.read(4*n))

  ao = ai
  minx = min(*ao)
  maxx = max(*ao)
  print("min: %d, max: %d" % (minx, maxx))

  wo = wave.open(outfile, 'w')
  wo.setnchannels(1)
  wo.setsampwidth(2)
  wo.setframerate(48000)
  wo.writeframes(struct.pack('h' * len(ao), *ao))
  wo.close()
