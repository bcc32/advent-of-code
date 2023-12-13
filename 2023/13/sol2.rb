$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.read.split(/\n\n/).map do |para|
    para.split
  end
end

def is_sym_col(grid, col)
  errors = (col...grid[0].size).map do |c|
    other_c = col - 1 - (c - col)
    other_c < 0 ? 0 : (grid.count { |row| row[c] != row[other_c] })
  end.sum

  errors == 1
end

def is_sym_row(grid, row)
  errors = (row...grid.size).map do |r|
    other_r = row - 1 - (r - row)
    other_r < 0 ? 0 : (grid[r].chars.zip(grid[other_r].chars).count { |a, b| a != b })
  end.sum

  errors == 1
end

def pattern(grid)
  c = (1...grid[0].size).find { |c| is_sym_col(grid, c) }
  return c if c

  r = (1...grid.size).find { |r| is_sym_row(grid, r) }
  100 * r
end

p input.map { |g| pattern(g) }.sum
