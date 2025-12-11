$stdout = File.open('aoc.out', 'w')
$ans = 0
File.open('aoc.in') do |f|
  lines = f.readlines
  ops = lines.pop

  args = []
  (lines[0].size - 1).downto(-1) do |col|
    if col == -1 || lines.all? { |l| l[col] == ' ' }
      op = ops[col+1] == '*' ? :* : :+
      $ans += args.reduce(op)
      args.clear
    else
      args << lines.map { |l| l[col] }.join.to_i
    end
  end
end
p $ans
