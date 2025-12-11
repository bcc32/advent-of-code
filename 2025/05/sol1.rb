$stdout = File.open('aoc.out', 'w')
ranges, items = File.open('aoc.in') do |f|
  ranges, items = f.read.split(/\n\n/)
  ranges = ranges.lines.map { |line| line.split('-').map(&:to_i) }
  items = items.lines.map(&:to_i)
  [ranges, items]
end

p (items.count do |item|
     ranges.any? { |x, y| (x..y) === item }
   end)
