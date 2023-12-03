require 'set'

$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp).map(&:chars)
end

symbols = Set.new
input.size.times do |i|
  input[i].size.times do |j|
    if input[i][j] =~ /[^.\d]/
      symbols << [i, j]
    end
  end
end

sum = 0

input.size.times do |i|
  line = input[i].join
  pos = 0
  while m = /\d+/.match(line, pos)
    if [i-1,i,i+1].any? { |ii| ((m.begin(0)-1)..(m.end(0))).any? { |jj| symbols.include?([ii,jj]) }}
      sum += m.to_s.to_i
    end
    pos = m.end(0)
  end
end

p sum
