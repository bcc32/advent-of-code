require '../util/min_heap'

grid = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp).map(&:chars).map { |line| line.map(&:to_i) }
end

def cost(grid, x, y)
  xx = x / grid.size
  xm = x % grid.size
  yy = y / grid[0].size
  ym = y % grid[0].size
  ans = grid[xm][ym] + xx + yy
  ans = ans % 9
  ans = 9 if ans == 0
  ans
end

def dijk(grid, xmax, ymax, start, end_)
  dist = {}
  dist[start] = 0
  pqueue = MinHeap.new([start]) { |x, y| dist[x] <=> dist[y] }
  until pqueue.empty?
    x, y = pqueue.shift
    d = dist[[x, y]]
    return d if [x, y] == end_
    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dx, dy|
      next unless (0..xmax) === x + dx && (0..ymax) === y + dy
      next if dist.has_key?([x + dx, y + dy])
      cost = cost(grid, x + dx, y + dy)
      dist[[x + dx, y + dy]] = d + cost
      pqueue << [x + dx, y + dy]
    end
  end
end

p dijk(grid, (grid.size * 5 - 1), (grid[0].size * 5 - 1), [0, 0], [grid.size * 5 - 1, grid[0].size * 5 - 1])
