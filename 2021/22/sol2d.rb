# Heavily adapted from
# https://www.reddit.com/r/adventofcode/comments/rlxhmg/2021_day_22_solutions/hpk0e3o/?context=3
# because I gave up on the interval map thing.

input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line =~ /(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/
    [($1 == 'on'), $2.to_i, $3.to_i, $4.to_i, $5.to_i, $6.to_i, $7.to_i]
  end
end

def region_intersect(r1, r2)
  x1 = [r1[0], r2[0]].max
  x2 = [r1[1], r2[1]].min

  y1 = [r1[2], r2[2]].max
  y2 = [r1[3], r2[3]].min

  z1 = [r1[4], r2[4]].max
  z2 = [r1[5], r2[5]].min

  [x1, x2, y1, y2, z1, z2] if x1 <= x2 && y1 <= y2 && z1 <= z2
end

def region_area(r)
  x1, x2, y1, y2, z1, z2 = r
  (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)
end

regions_plus = []
regions_minus = []

input.each do |onoff, x1, x2, y1, y2, z1, z2|
  r = [x1, x2, y1, y2, z1, z2]

  rmn = regions_plus.map do |plus|
    region_intersect(r, plus)
  end.compact

  rpn = regions_minus.map do |minus|
    region_intersect(r, minus)
  end.compact

  regions_plus.concat(rpn)
  regions_minus.concat(rmn)

  if onoff
    regions_plus << r
  end
end

p (regions_plus.map { |r| region_area(r) }.sum - regions_minus.map { |r| region_area(r) }.sum)
