require 'set'

$stdout = File.open('aoc.out', 'w')

# both ends inclusive
class IntervalSet
  def initialize
    @intervals = []
  end

  def find_intersecting(start1, end1)
    @intervals.find do |start2, end2|
      (start1..end1) === start2 ||
        (start1..end1) === end2
    end
  end

  def <<(i)
    start, end_ = i
    fail unless start <= end_

    int = find_intersecting(start, end_)
    if int
      int[0] = [int[0], start].min
      int[1] = [int[1], end_].max
    else
      @intervals << [start, end_]
    end

    sort_and_merge!
  end

  def sort_and_merge!
    @intervals.sort_by! { |start, _| start }

    new_intervals = []
    @intervals.each do |start, end_|
      if new_intervals[-1] && new_intervals[-1][1] >= start - 1
        new_intervals[-1][1] = [new_intervals[-1][1], end_].max
      else
        new_intervals << [start, end_]
      end
    end

    @intervals = new_intervals
  end

  def find_first_gap(start1, end1)
    i = @intervals.find { |start2, end2| start1 >= start2 }
    i && i[1] < end1 && i[1] + 1
  end

  def each
    @intervals.each do |start, end_|
      (start..end_).each do |x|
        yield x
      end
    end
  end

  include Enumerable
end

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

K = 4000000

ans = (0..K).lazy.filter_map do |y|
  bad = IntervalSet.new
  input.each do |x1, y1, x2, y2, d|
    dist_on_line = d - (y1 - y).abs
    if dist_on_line >= 0
      bad << [x1 - dist_on_line, x1 + dist_on_line]
    end
    bad << [x2, x2] if y2 == y
  end

  x = bad.find_first_gap(0, K)
  x && [x, y]
end.first

x, y = ans
p (4000000 * x + y)
