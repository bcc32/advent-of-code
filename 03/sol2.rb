input = File.open('input') do |f|
  f.map { |s| s.chomp.chars.map(&:to_i) }
end

def mode(arr)
  h = Hash.new(0)
  arr.each { |x| h[x] += 1 }
  if h[0] == h[1]
    return 1
  end
  h.keys.max_by(&h)
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
