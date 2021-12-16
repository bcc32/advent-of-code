input = File.open('input') do |f|
  f.map { |s| s.chomp.chars.map(&:to_i) }
end

def mode(arr)
  arr.group_by { |x| x }.max_by { |x, y| [y.size, x] }.first
end

oxygen = input.dup
bit = 0
while oxygen.size > 1
  m = mode(oxygen.map { |x| x[bit] })
  oxygen.select! { |x| x[bit] == m }
  bit += 1
end

co2 = input.dup
bit = 0
while co2.size > 1
  m = 1 - mode(co2.map { |x| x[bit] })
  co2.select! { |x| x[bit] == m }
  bit += 1
end

p oxygen[0].join.to_i(2) * co2[0].join.to_i(2)
