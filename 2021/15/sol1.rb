grid = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp).map(&:chars).map { |line| line.map(&:to_i) }
end

def dijk(grid, start, end_)
  dist = {}
  dist[start] = 0
  queue = [start]
  until queue.empty?
    x, y = queue.min_by(&dist)
    d = dist[[x, y]]
    return d if end_ == [x, y]

    queue.delete([x, y])
    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dx, dy|
      next unless (0...grid.size).include?(x + dx) && (0...grid[0].size).include?(y + dy)

      next if dist.key?([x + dx, y + dy])

      cost = grid[x + dx] && grid[x + dx][y + dy]
      dist[[x + dx, y + dy]] = d + cost
      queue << [x + dx, y + dy]
    end
  end
end

p dijk(grid, [0, 0], [grid.size - 1, grid[0].size - 1])
