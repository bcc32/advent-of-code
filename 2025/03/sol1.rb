$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line.chomp.chars.map(&:to_i)
  end
end

p (input.map do |bank|
  pairs = Set.new
  seen = Set.new
  bank.each do |n|
    seen.each do |m|
      pairs << 10 * m + n
    end
    seen << n
  end

  pairs.max
end.sum)
