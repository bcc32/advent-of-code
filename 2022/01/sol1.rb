input = File.open('aoc.in') do |f|
  f.read.split("\n\n").map(&:split).map do |r|
    r.map(&:to_i)
  end
end

p input.max_by(&:sum).sum
