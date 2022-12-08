$stdout = File.open('aoc.out', 'w')
grid = IO.read('aoc.in').lines.map(&:chomp).map(&:chars)

c = 0

(0...grid.size).each do |i|
  grid[0].size.times do |j|
    visible = true
    (0...i).each do |ii|
      visible = false if grid[ii][j] >= grid[i][j]
    end
    if visible
      c += 1
      next
    end

    visible = true
    (i+1...grid.size).each do |ii|
      visible = false if grid[ii][j] >= grid[i][j]
    end
    if visible
      c += 1
      next
    end

    visible = true
    (0...j).each do |jj|
      visible = false if grid[i][jj] >= grid[i][j]
    end
    if visible
      c += 1
      next
    end

    visible = true
    (j+1...grid[0].size).each do |jj|
      visible = false if grid[i][jj] >= grid[i][j]
    end
    if visible
      c += 1
      next
    end
  end

end
p c
