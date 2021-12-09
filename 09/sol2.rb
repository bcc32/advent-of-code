require 'set'

input = File.open('input') do |f|
  f.lines.map(&:chomp).map(&:chars).map { |row| row.map(&:to_i) }
end

def dfs(input, visited, i, j)
  return if visited.include?([i, j])

  visited << [i, j]

  [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |di, dj|
    if (0...input.size).include?(i + di) \
       && (0...input[0].size).include?(j + dj) \
       && input[i + di][j + dj] != 9
      dfs(input, visited, i + di, j + dj)
    end
  end
end

basins_size = []

visited = Set.new
input.each_with_index do |row, i|
  row.each_with_index do |cell, j|
    if cell != 9
      before = visited.size
      dfs(input, visited, i, j)
      if visited.size > before
        basins_size << visited.size - before
      end
    end
  end
end

basins_size.sort!
p basins_size[-3, 3].reduce(&:*)
