$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map { |l| l.split(',').map(&:to_i) }
end

def area(a, b)
  ((a[0] - b[0]).abs + 1) * ((a[1] - b[1]).abs + 1)
end

ans = input.each_with_index.flat_map do |a, i|
  (i+1...input.size).map do |j|
    b = input[j]
    area(a, b)
  end
end.max

p ans
