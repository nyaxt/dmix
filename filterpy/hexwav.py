import sys
import wave
import struct

a = []

bi = open(sys.argv[1], 'r')
for l in bi:
  if(l[0] == 'x'):
    continue

  d = int(l, 16)
  d >>= 8
  a.append(d)
bi.close()

wi = wave.open(sys.argv[2], 'w')
wi.setnchannels(1)
wi.setsampwidth(2)
wi.setframerate(96000)
wi.writeframes(struct.pack('H' * len(a), *a))
wi.close()
