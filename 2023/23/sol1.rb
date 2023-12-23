require 'set'

$stdout = File.open('aoc.out', 'w')
grid = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line.chomp.chars
  end
end

start_x = 0
start_y = grid[0].index { |c| c == '.' }

# def bfs(grid, start_x, start_y)
#   q = [[start_x, start_y]]
#   max_dist = { q[0] => 0 }

#   until q.empty?
#     x, y = q.shift
#     d = max_dist[[x, y]]

#     dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]]
#     case grid[x][y]
#     when '<'
#       dirs = [[0, -1]]
#     when '>'
#       dirs = [[0, 1]]
#     when '^'
#       dirs = [[-1, 0]]
#     when 'v'
#       dirs = [[1, 0]]
#     end
#     dirs.each do |dx, dy|
#       next unless (0...grid.size) === (x + dx) && (0...grid[0].size) === (y + dy)
#       next if grid[x + dx][y + dy] == '#'

#       cand = d + 1
#       if !max_dist[[x + dx, y + dy]] || cand > max_dist[[x + dx, y + dy]]
#         max_dist[[x + dx, y + dy]] = cand
#       end
#     end
#   end
# end

def dfs_max_length(grid, start_x, start_y, visited)
  if visited.include?([start_x, start_y])
    return 0
  end

  if start_x == grid.size - 1
    return visited.size
  end

  visited << [start_x, start_y]

  dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]]
  case grid[start_x][start_y]
  when '<'
    dirs = [[0, -1]]
  when '>'
    dirs = [[0, 1]]
  when '^'
    dirs = [[-1, 0]]
  when 'v'
    dirs = [[1, 0]]
  end

  ans = dirs.map do |dx, dy|
    next unless (0...grid.size) === (start_x + dx) && (0...grid[0].size) === (start_y + dy)
    next if grid[start_x + dx][start_y + dy] == '#'

    dfs_max_length(grid, start_x + dx, start_y + dy, visited)
  end.compact.max

  visited.delete [start_x, start_y]

  ans || -Float::INFINITY
end

p dfs_max_length(grid, start_x, start_y, Set.new)
