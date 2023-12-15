$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.read.chomp.split(',')
end

def hash(s)
  v = 0
  s.each_char do |c|
    v += c.ord
    v *= 17
    v = v % 256
  end
  v
end

p input.map { |s| hash(s) }.sum
