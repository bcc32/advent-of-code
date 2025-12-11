$stdout = File.open('aoc.out', 'w')
$input = File.open('aoc.in') do |f|
  f.readlines.map { |l| l.chomp.chars }
end

def is_occupied(x, y)
  (0...$input.size) === x &&
    (0...$input[0].size) === y &&
    $input[x][y] == '@'
end

$ans = 0
$input.size.times do |x|
  $input[x].size.times do |y|
    next unless $input[x][y] == '@'
    # plus one for self
    if (-1..1).flat_map { |dx| (-1..1).map { |dy| is_occupied(x + dx, y + dy) } }.count(true) < 5
      $ans += 1
    end
  end
end

p $ans
