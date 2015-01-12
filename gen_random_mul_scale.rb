$pmg = Random.new(12345)

def gen_24bit_signed_int
  $pmg.rand(0xffffff) - 0x7fffff
end

def gen_32bit_unsigned_int
  $pmg.rand(0xffffffff)
end

def write_hex(filename, array)
  open(filename, 'w') do |f|
    f.write array.map {|x| sprintf("%08x", x & 0xffffffff) }.join("\n")
    f.write "\n"
  end
end

as = [0x123456]
bs = [0x01000000]
ps = [0x123456]

31.times do
  a = gen_24bit_signed_int
  b = gen_32bit_unsigned_int
  p = a * b / 0x1000000

  as << a
  bs << b
  ps << p
end

write_hex("testdata/gen/mp_scale_a.hex", as)
write_hex("testdata/gen/mp_scale_b.hex", bs)
write_hex("testdata/gen/mp_scale_p.hex", ps)
