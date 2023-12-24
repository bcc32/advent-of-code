$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    px, py, pz, vx, vy, vz = line.scan(/-?\d+/)
    [px, py, pz, vx, vy, vz].map(&:to_f)
  end
end

# test_area_x = (7..27)
# test_area_y = (7..27)

test_area_x = (200000000000000..400000000000000)
test_area_y = (200000000000000..400000000000000)

def intersect(point1, point2)
  # y=m1x+b1
  # y=m2x+b2
  slope1 = point1[4] / point1[3]
  slope2 = point2[4] / point2[3]

  b1 = point1[1] - slope1 * point1[0]
  b2 = point2[1] - slope2 * point2[0]

  if slope1 == slope2
    if b1 == b2
      return :everywhere
    else
      return :nowhere
    end
  end

  # m1x+b1 = m2x+b2
  # x = (b2 - b1) / (m1 - m2)
  x = (b2 - b1) / (slope1 - slope2)
  y = slope1 * x + b1

  [x, y]
end

def is_future?(point, x)
  (point[3] > 0) == ((x - point[0]) > 0)
end

count = 0
input.each_with_index do |p1, i|
  input[i+1..].each do |p2|
    intersection = intersect(p1, p2)
    case intersection
    when :everywhere
      count += 1
    when :nowhere
    else
      x, y = intersection
      # p intersection
      if is_future?(p1, x) && is_future?(p2, x) && test_area_x === x && test_area_y === y
        count +=1
      end
    end
  end
end

p count
