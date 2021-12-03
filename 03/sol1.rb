input = File.open('input') do |f|
  f.map { |s| s.chomp.chars.map(&:to_i) }
end

def mode(arr)
  h = Hash.new(0)
  arr.each { |x| h[x] += 1 }
  h.keys.max_by(&h)
end

gamma = input[0].size.times.map { |i| mode(input.map { |r| r[i] }) }.join.to_i(2)
epsilon = input[0].size.times.map { |i| 1 - mode(input.map { |r| r[i] }) }.join.to_i(2)

puts gamma * epsilon
