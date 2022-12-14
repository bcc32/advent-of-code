$stdout = File.open('aoc.out', 'w')

$state = {}
$max_y = 0
input = File.open('aoc.in') do |f|
  f.readlines.each do |line|
    line.split(/ -> /).map { |coord| coord.split(/,/) }.each_cons(2) do |(x1, y1), (x2, y2)|
      x1, x2, y1, y2 = [x1, x2, y1, y2].map(&:to_i)
      x1, x2 = [x1, x2].sort
      y1, y2 = [y1, y2].sort
      (x1..x2).each do |x|
        (y1..y2).each do |y|
          $state[[x, y]] = '#'
        end
      end

      $max_y = [$max_y, y2].max
    end
  end
end

def drop_sand!
  x, y = [500, 0]
  if $state[[x, y]] == 'o'
    return false
  end

  loop do
    if y == $max_y + 1
      $state[[x, y]] = 'o'
      return true
    elsif !$state[[x, y + 1]]
      y += 1
    elsif !$state[[x - 1, y + 1]]
      x -= 1
      y += 1
    elsif !$state[[x + 1, y + 1]]
      x += 1
      y += 1
    else
      $state[[x, y]] = 'o'
      return true
    end
  end
end

1 while drop_sand!

p $state.count { |k, v| v == 'o' }
