dots, folds = File.open('input') do |f|
  b, a = f.read.split(/\n\n/)
  b = b.lines.map  { |line| line.split(/,/).map(&:to_i) }
  a = a.lines.map { |line|
    line =~ /fold along (.)=(\d+)/
    [$1, $2.to_i]
  }
  [b, a]
end

maxx = dots.map(&:first).max
maxy = dots.map(&:last).max

grid = (0..maxy).map { |y|
  (0..maxx).map { |x|
    '.'
  }
}

dots.each do |x, y|
  grid[y][x] = '#'
end

# puts 'before'
# grid.map(&:join).each { |x| puts x }

folds.each do |axis, value|
  case axis
  when 'x'
    width = [ value, grid[0].size - value - 1 ].max
    grid.map! do |row|
      x1 = value - 1
      x2 = value + 1
      width.times.map {
        ans = row[x1] == '#' || row[x2] == '#' ? '#' : '.'
        x1 -= 1
        x2 += 1
        ans
      }.reverse
    end
  when 'y'
    width = [ value, grid.size - value - 1 ].max
    grid = grid.transpose.map do |row|
      y1 = value - 1
      y2 = value + 1
      width.times.map {
        ans = row[y1] == '#' || row[y2] == '#' ? '#' : '.'
        y1 -= 1
        y2 += 1
        ans
      }.reverse
    end.transpose
  end
  # puts 'after'

  # grid.map(&:join).each { |x| puts x }

  break
end

# puts 'after'

# grid.map(&:join).each { |x| puts x }

p grid.flatten.count('#')
