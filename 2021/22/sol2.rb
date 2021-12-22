input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line =~ /(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/
    [($1 == 'on'), $2.to_i, $3.to_i, $4.to_i, $5.to_i, $6.to_i, $7.to_i]
  end
end

def coords_intersect(min1, max1, min2, max2)
  max1 >= min2 && max2 >= min1
end

def coord_intersection(min1, max1, min2, max2)
  a, b, c, d = [min1, max1, min2, max2].sort
end

# returns a list of regions that represent the space in r1 minus the space in r2
def region_diff(r1, r2)
  minx1, maxx1, miny1, maxy1, minz1, maxz1 = r1
  minx2, maxx2, miny2, maxy2, minz2, maxz2 = r2
  if coords_intersect(minx1, maxx1, minx2, maxx2) &&
     coords_intersect(miny1, maxy1, miny2, maxy2) &&
     coords_intersect(minz1, maxz1, minz2, maxz2)
  else
    [r1]
  end
end

def region_area(r)
  x1, x2, y1, y2, z1, z2 = r
  (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)
end

regions_on = []

input.each do |onoff, x1, x2, y1, y2, z1, z2|
  region1s = [[x1, x2, y1, y2, z1, z2]]

  if onoff
    regions_on.each do |region2|
      region1s = region1s.flat_map { |region1| region_diff(region1, region2) }
    end
  else
    regions_on = regions_on.flat_map do |region2|
      fail unless region1s.size == 1
      region_diff(region2, region1s[0])
    end
  end
end

p regions_on.map { |region| region_area(region) }.sum
