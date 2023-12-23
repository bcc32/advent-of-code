require 'set'

$stdout = File.open('aoc.out', 'w')
grid = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line.chomp.chars
  end
end

start_x = 0
start_y = grid[0].index { |c| c == '.' }

end_x = grid.size - 1
end_y = grid.last.index('.')

$interesting_points = Set.new
$interesting_points << [start_x, start_y]
$interesting_points << [end_x, end_y]

# other interesting points are those with more than 2 neighbors

grid.size.times do |x|
  grid[0].size.times do |y|
    dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]]
    next if grid[x][y] == '#'
    if dirs.count { |dx, dy| (0...grid.size) === (x + dx) && (0...grid[0].size) === (y + dy) && grid[x + dx][y + dy] != '#' } > 2
      $interesting_points << [x, y]
    end
  end
end

# p ['interesting', $interesting_points]

$cache = {}
def shortest_distance(grid, x1, y1, x2, y2)
  $cache[[x1, y1]] ||=
    begin
      q = [[x1, y1]]
      d = { q[0] => 0 }
      until q.empty?
        # p q
        x, y = q.shift
        if [x, y] != [x1, y1] && $interesting_points.include?([x, y])
          next
        end
        dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]]
        dirs.each do |dx, dy|
          next unless (0...grid.size) === x+dx && (0...grid[0].size) === y+dy
          next if grid[x+dx][y+dy] == '#'
          next if d[[x + dx, y + dy]]
          d[[x+dx,y+dy]] = d[[x,y]] + 1
          q << [x+dx, y+dy]
        end
      end
      d
    end

  $cache[[x1, y1]][[x2, y2]]
end

$adj = Hash.new { |h, k| h[k] = [] }

$points_flat = $interesting_points.to_a

$points_flat.each_with_index do |u, i|
  $points_flat.each_with_index do |v, j|
    if shortest_distance(grid, *u, *v)
      $adj[i] << j
    end
  end
end

# p shortest_distance(grid, 0, 1, 5, 3)
# p shortest_distance(grid, 5, 3, 3, 11)

$max = 0

# this works but is too slow
def dfs_longest_path(grid, start, stop, visited, current_total)
  if visited[start]
    return 0
  end

  if start == stop
    $max = [$max, current_total].max
    p $max
    return current_total
  end

  visited[start] = true

  ans = $adj[start].map do |next_point|
    d = shortest_distance(grid, *$points_flat[start], *$points_flat[next_point])
    dfs_longest_path(grid, next_point, stop, visited, current_total + d)
  end.compact.max

  visited[start] = false

  ans || -Float::INFINITY
end

# Takes about 3 minutes to run on my fastest machine
p dfs_longest_path(grid,
                   $points_flat.index([start_x, start_y]),
                   $points_flat.index([end_x, end_y]),
                   Array.new($points_flat.size, false),
                   0)
