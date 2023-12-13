$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.read.split(/\n\n/).map do |para|
    para.split
  end
end

def is_sym_col(grid, col)
  (col...grid[0].size).all? do |c|
    other_c = col - 1 - (c - col)
    other_c < 0 || (grid.all? { |row| row[c] == row[other_c] })
  end
end

def is_sym_row(grid, row)
  (row...grid.size).all? do |r|
    other_r = row - 1 - (r - row)
    other_r < 0 || grid[r] == grid[other_r]
  end
end

def pattern(grid)
  c = (1...grid[0].size).find { |c| is_sym_col(grid, c) }
  return c if c

  r = (1...grid.size).find { |r| is_sym_row(grid, r) }
  100 * r
end

p input.map { |g| pattern(g) }.sum
