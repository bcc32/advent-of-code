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

def bfs(grid, start, n, extra_grids)
  q = [start]
  d = { start => 0 }

  until q.empty?
    x, y = q.shift

    if d[[x, y]] > n
      break
    end

    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dx, dy|
      # if x + dx < -(extra_grids*grid.size) || x + dx >= (extra_grids + 1) * grid.size ||
      #    y + dy < -(extra_grids*grid[0].size) || y + dy >= (extra_grids + 1) * grid[0].size
      #   next
      # end

      next if grid[(x + dx) % grid.size][(y + dy) % grid[0].size] == '#'
      next if d[[x + dx, y + dy]]

      d[[x + dx, y + dy]] = d[[x, y]] + 1
      q << [x + dx, y + dy]
    end
  end

  d.each_value.count { |v| v % 2 == n % 2 }
end

# apparently this pattern is quadratic:

5.times do |i|
  # p bfs(grid, start, 65 + 131 * i, Float::INFINITY)
end

# for my input, this is 14_773 x^2 - 14_683 x + 3640
x = (N - 65) / 131 + 1
p (14_773 * x * x - 14_683 * x + 3640)
