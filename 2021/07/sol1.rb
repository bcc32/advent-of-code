input = File.open('input') do |f|
  f.read.split(',').map(&:to_i)
end

a, b = input.minmax
p (a..b).map { |x|
  input.map { |y| (x - y).abs }.sum
}.min
