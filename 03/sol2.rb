input = File.open('input') do |f|
  f.map { |s| s.chomp.chars.map(&:to_i) }
end

oxygen = input.dup
bit = 0
while oxygen.size > 1
  zeros = oxygen.map { |x| x[bit] }.count(0)
  ones = oxygen.map { |x| x[bit] }.count(1)
  most_common = if zeros > ones
    0
  else
    1
  end

  oxygen.select! { |x| x[bit] == most_common }
  bit += 1
end

oxygen = oxygen[0]


co2 = input.dup
bit = 0
while co2.size > 1
  zeros = co2.map { |x| x[bit] }.count(0)
  ones = co2.map { |x| x[bit] }.count(1)
  least_common = if zeros <= ones
                  0
                else
                  1
                end

  co2.select! { |x| x[bit] == least_common }
  bit += 1
end

co2 = co2[0]

p oxygen.join.to_i(2) * co2.join.to_i(2)
