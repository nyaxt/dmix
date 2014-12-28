ARGF.gets # skip header
ARGF.each_line do |l|
  c = l.split(/,/)
  c.shift
  puts c.join('')
end
