require 'set'

$stdout = File.open('aoc.out', 'w')

tail_visited = Set.new

input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    dir, count = line.split
    count = count.to_i
    [dir, count]
  end
end

$nodes = 10.times.map { [0, 0] }
tail_visited << $nodes.last.dup

def sign(x)
  return 0 if x.zero?
  x / x.abs
end

def update_tail!
  $nodes.each_cons(2) do |head, tail|
    dx = head[0] - tail[0]
    dy = head[1] - tail[1]

    if dx.abs >= 2 || dy.abs >= 2
      tail[0] += sign(dx)
      tail[1] += sign(dy)
    end
  end
end

input.each do |dir, count|
  count.times do
    head = $nodes[0]
    case dir
    when 'L'
      head[0] -= 1
    when 'R'
      head[0] += 1
    when 'U'
      head[1] += 1
    when 'D'
      head[1] -= 1
    else
      fail
    end

    update_tail!

    tail_visited << $nodes.last.dup
  end
end

p tail_visited.size
