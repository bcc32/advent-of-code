input = File.open('input') do |f|
  f.map { |s| s.chomp.chars.map(&:to_i) }
end
gamma = (((0...input[0].size).map { |i|
                     [0, 1].min_by { |b|
                       input.map { |r| r[i] }.count(b)
                     }
                   }).join).to_i(2)
epsilon = (((0...input[0].size).map { |i|
  [0, 1].max_by { |b|
    input.map { |r| r[i] }.count(b)
  }
}).join).to_i(2)

p gamma
p epsilon

puts gamma * epsilon
