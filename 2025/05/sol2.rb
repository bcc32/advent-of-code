$stdout = File.open('aoc.out', 'w')
ranges = File.open('aoc.in') do |f|
  ranges, items = f.read.split(/\n\n/)
  ranges.lines.map { |line| line.split('-').map(&:to_i) }
end

ranges.sort_by!(&:first)

$ans = 0
ranges.each_with_index do |r, i|
  di = 1
  while true
    if i + di < ranges.size && r[1] >= ranges[i + di][0]
      ranges[i+di][0] = r[1] + 1
    else
      break
    end
    di += 1
  end

  if r[0] <= r[1]
    $ans += r[1] - r[0] + 1
  end
end
p $ans
