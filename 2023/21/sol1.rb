require 'set'

$stdout = File.open('aoc.out', 'w')
grid = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line.chomp.chars
  end
end

def find_start(grid)
  grid.size.times.each do |i|
    grid[i].size.times.each do |j|
      if grid[i][j] == 'S'
        return [i, j]
      end
    end
  end
end

start = find_start(grid)

def bfs(grid, start)
  reachable = Set.new
  reachable << start
  64.times do
    new_reachable = Set.new
    reachable.each do |x, y|
      [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dx, dy|
        next if grid[x + dx][y + dy] == '#'
        next unless (0...grid.size).cover?(x + dx) && (0...grid[0].size).cover?(y + dy)
        new_reachable << [x + dx, y + dy]
      end
    end
    reachable = new_reachable
  end

  reachable.size
end

p bfs(grid, start)
