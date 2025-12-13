require 'set'

$stdout = File.open('aoc.out', 'w')
$input = File.open('aoc.in') do |f|
  f.readlines
end

$memo = {}
def timelines(col, row)
  return 1 if row == $input.size

  $memo[[col, row]] ||=
    begin
      if $input[row][col] == '^'
        timelines(col - 1, row) + timelines(col + 1, row)
      else
        timelines(col, row + 1)
      end
    end
end
starting_col = $input[0].index('S')
ans = timelines(starting_col, 1)

p ans
