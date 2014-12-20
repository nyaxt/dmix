$pmg = Random.new(12345)

def gen_24bit_signed_int
  $pmg.rand(0xffffff) - 0x7fffff
end

def write_hex(filename, array)
  open(filename, 'w') do |f|
    f.write array.map {|x| sprintf("%06x", x & 0xffffff) }.join("\n")
    f.write "\n"
  end
end

as = []
bs = []
ps = []

32.times do
  a = gen_24bit_signed_int
  b = gen_24bit_signed_int
  p = a * b / 0x800000

  as << a
  bs << b
  ps << p
end

write_hex("testdata/gen/mp_a.hex", as)
write_hex("testdata/gen/mp_b.hex", bs)
write_hex("testdata/gen/mp_p.hex", ps)
