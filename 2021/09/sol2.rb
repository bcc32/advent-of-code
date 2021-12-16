require 'set'

input = File.open('input') do |f|
  f.lines.map do |line|
    line.chomp.chars.map(&:to_i)
  end
end

def dfs(grid, visited, i, j)
  return 0 if visited.include?([i, j])

  count = 1
  visited << [i, j]

  [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |di, dj|
    if (0...grid.size).include?(i + di) \
       && (0...grid[0].size).include?(j + dj) \
       && grid[i + di][j + dj] != 9
      count += dfs(grid, visited, i + di, j + dj)
    end
  end

  count
end

basins_size = []

visited = Set.new
input.each_with_index do |row, i|
  row.each_with_index do |cell, j|
    if cell != 9
      basins_size << dfs(input, visited, i, j)
    end
  end
end

p basins_size.max(3).reduce(&:*)
