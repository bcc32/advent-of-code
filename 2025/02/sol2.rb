$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.read.split(/,/).map { |x| x.split('-').map(&:to_i) }
end

def is_invalid?(x)
  s = x.to_s
  (1..s.size/2).any? do |len|
    next unless s.size % len == 0
    z = s[0...len]
    s == z * (s.size / len)
  end
end

p (input.map do |x, y|
  (x..y).select do |x|
    is_invalid?(x)
  end.sum
end.sum)
