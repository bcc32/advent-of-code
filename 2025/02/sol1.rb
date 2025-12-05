$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.read.split(/,/).map { |x| x.split('-').map(&:to_i) }
end

def is_invalid?(x)
  s = x.to_s
  z = s[0...s.size/2]
  s == z + z
end

p (input.map do |x, y|
  (x..y).select do |x|
    is_invalid?(x)
  end.sum
end.sum)
