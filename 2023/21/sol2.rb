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
N = 26501365

# for repeats of the map that are not in the same (inter-map) row or column as
# the starting point, it is always sufficient to move to the nearest corner from
# the starting map, then move around in the sidewalks, then move from the corner
# to the ending position

def bfs(grid, start)
  this = Set.new
  this << start
  last = Set.new                # to keep track of parity
  frontier = Set.new
  frontier << start

  5000.times do
    new_reachable = Set.new
    reachable.each do |x, y|
      [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dx, dy|
        next if grid[(x + dx) % grid.size][(y + dy) % grid[0].size] == '#'
        next if reached_last_time.include?([x + dx, y + dy])
        new_reachable << [x + dx, y + dy]
      end
    end

    # p new_reachable.size - reachable.size
    reached_last_time = reachable
    reachable = new_reachable
    # p reachable.size
  end

  reachable.size
end

p bfs(grid, start)
