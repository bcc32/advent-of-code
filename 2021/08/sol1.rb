input = File.open('input') do |f|
  f.lines.map do |line|
    xs = line.split
    before = xs.take(xs.size - 5)
    after = xs.drop(xs.size - 4)
    [before, after]
  end
end

p input.map { |x, y|
  y.count { |z|
    [2, 3, 4, 7].include?(z.size)
    }
  }.sum
