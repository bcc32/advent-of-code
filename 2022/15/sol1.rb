require 'set'

$stdout = File.open('aoc.out', 'w')

def dist(x1, y1, x2, y2)
  (x1 - x2).abs + (y1 - y2).abs
end

input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line =~ /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/ or fail
    x1, y1, x2, y2 = [$1.to_i, $2.to_i, $3.to_i, $4.to_i]
    d = dist(x1, y1, x2, y2)
    [x1, y1, x2, y2, d]
  end
end

max_dist = input.map { |_, _, _, _, d| d }.max

min_x, max_x = input.map { |x, _, _, _, _| x }.minmax
min_y, max_y = input.map { |_, y, _, _, _| y }.minmax

y = 2000000

bad = Set.new

input.each do |x1, y1, x2, y2, d|
  dist_on_line = d - (y1 - y).abs
  if dist_on_line >= 0
    (x1 - dist_on_line..x1 + dist_on_line).each do |x|
      bad << x
    end
  end
end

input.each do |_, _, x2, y2, _|
  if y2 == y
    bad.delete(x2)
  end
end

p bad.size
