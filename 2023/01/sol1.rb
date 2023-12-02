$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp)
end

p (input.map do |s|
  a = s.scan(/\d/)
  (a.first + a.last).to_i
end.sum)
