require 'set'

$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp).map(&:chars)
end

gears = Set.new
input.size.times do |i|
  input[i].size.times do |j|
    if input[i][j] == '*'
      gears << [i, j]
    end
  end
end

sum = 0

gears_used = Hash.new { |h, k| h[k] = [] }

input.size.times do |i|
  line = input[i].join
  pos = 0
  while m = /\d+/.match(line, pos)
    [i-1,i,i+1].each do |ii|
      ((m.begin(0)-1)..(m.end(0))).each do |jj|

        if gears.include?([ii,jj]) && gears_used[[ii,jj]].size < 2
          gears_used[[ii,jj]] << m.to_s.to_i
        elsif gears.include?([ii,jj])
          gears.delete([ii,jj])
        end
      end
    end
    pos = m.end(0)
  end
end

p (gears_used.values.select { |v| v.size == 2 }.map { |v| v[0]*v[1] }.sum)
