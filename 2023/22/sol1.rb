require 'set'

$stdout = File.open('aoc.out', 'w')
blocks = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line =~ /^(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)$/
    x1, y1, z1, x2, y2, z2 = $1.to_i, $2.to_i, $3.to_i, $4.to_i, $5.to_i, $6.to_i
    [x1, y1, z1, x2, y2, z2]
  end
end

blocks.sort_by! { |_, _, z1, _, _, _| z1 }

def ranges_intersect(a, b)
  # not quite right but good enough for this problem
  a.cover?(b.min) || a.cover?(b.max) || b.cover?(a.min) || b.cover?(a.max)
end

def intersection_in_xy_plane(b1, b2)
  ranges_intersect(b1[0]..b1[3], b2[0]..b2[3]) &&
    ranges_intersect(b1[1]..b1[4], b2[1]..b2[4])
  # ((b1[0]..b1[3]) === b2[0] || (b1[0]..b1[3]) === b2[3]) &&
  #   ((b1[1]..b1[4]) === b2[1] || (b1[1]..b1[4]) === b2[4])
end

$resting_on = Hash.new { |h, k| h[k] = Set.new }

def may_fall?(blocks, i)
  x1, y1, z1, x2, y2, z2 = blocks[i]
  return false if z1 == 1

  blocks[...i].each_with_index.map { |b, j|
    next true if j == i

    if ((b[2]..b[5]) === (z1 - 1) || (b[2]..b[5]) === (z2 - 1)) && intersection_in_xy_plane(b, blocks[i])
      $resting_on[i] << j
      false
    else
      true
    end
  }.uniq == [true]
end

def fall!(coords)
  coords[2] -= 1
  coords[5] -= 1
end

def fall_all!(blocks)
  blocks.each_index do |i|
    p i
    while may_fall?(blocks, i)
      fall! blocks[i]
    end
  end
end

fall_all! blocks
# p $resting_on
p 'done falling'

# def may_disintegrate(blocks, i)
#   b = blocks.dup
#   b.delete_at(i)
#   ! (i...b.size).any? { |j| may_fall?(b, j) }
# end

def may_disintegrate(blocks, i)
  ! $resting_on.each_value.any? { |v| v.to_a == [i] }
end

p blocks.each_index.count { |i| may_disintegrate(blocks, i) }
