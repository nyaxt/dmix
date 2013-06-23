import sys
import wave
import struct

wi = wave.open(sys.argv[1], 'r')
# n = wi.getnframes()
n = 100000

ai = struct.unpack('H'*n, wi.readframes(n))

bo = open(sys.argv[2], 'w')
for e in ai:
  bo.write("%x\n" % e)
bo.close()
