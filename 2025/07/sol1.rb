require 'set'

$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines
end

$splits = 0
cols = [input[0].index('S')].to_set
1.upto(input.size-1) do |row|
  new_cols = Set.new
  cols.each do |c|
    if input[row][c] == '^'
      new_cols << c - 1
      new_cols << c + 1
      $splits += 1
    else
      new_cols << c
    end
  end
  cols = new_cols
end

p $splits
