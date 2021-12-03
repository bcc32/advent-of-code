input = File.open('input') do |f|
  f.map { |s| s.chomp.chars.map(&:to_i) }
end

def mode(arr)
  arr.group_by { |x| x }.max_by { |_, y| y.size }.first
end

gamma = input[0].size.times.map { |i| mode(input.map { |r| r[i] }) }.join.to_i(2)
epsilon = input[0].size.times.map { |i| 1 - mode(input.map { |r| r[i] }) }.join.to_i(2)

puts gamma * epsilon
