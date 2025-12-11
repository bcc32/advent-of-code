$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map(&:split)
end

ops = input.pop
input.each { |line| line.map!(&:to_i) }

$ans = 0
input[0].each_index do |col|
  op = ops[col] == '*' ? :* : :+
  col_ans = input.map { |r| r[col] }.reduce(op)
  $ans += col_ans
end
p $ans
