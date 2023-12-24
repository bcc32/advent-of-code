$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    px, py, pz, vx, vy, vz = line.scan(/-?\d+/)
    [px, py, pz, vx, vy, vz].map(&:to_i)
  end
end

def intersect_at_t(point1, point2, t)
  x1, y1, z1 = point1[0] + t * point1[3] +
               point1[1] + t * point1[4] +
               point1[2] + t * point1[5]
  x2, y2, z2 = point2[0] + t * point2[3] +
               point2[1] + t * point2[4] +
               point2[2] + t * point2[5]

  [x1, y1, z1] == [x2, y2, z2]
end

def maybe_intersection_time(point1, point2)
  dx = point2[0] - point1[0]
  dvx = point1[3] - point2[3]
  dvx != 0 && dx / dvx
end

# starts at point1 at t=0
# def find_traj_between_points(point1, point2)
#   dx = point2[0] - point1[0]
#   dy = point2[1] - point1[1]
#   dz = point2[2] - point1[2]

#   g = [dx, dy, dz].reduce(&:gcd)
#   factors = (1..g).select { |f| g % f == 0 }
#   factors.map do |t|
#     vx, vy, vz = [dx / t, dy / t, dz / t]
#     [point1[0], point1[1], point1[2], t, vx, vy, vz]
#   end
# end

# p find_traj_between_points(input.last, input[1])

# where x0, vx0 is the rock's position and velocity, and where t1 is the
# collision time with hailstone 1, etc.:
#
# x0 + vx0 * t1 = x1 + vx1 * t1
# x0 + vx0 * t2 = x2 + vx2 * t2
# x0 + vx0 * t3 = x3 + vx3 * t3

# (t2 - t1) vx0 = (t2 * vx2 + x2 - t1 * vx1 - x1)
# vx0 = (t2 * vx2 + x2 - t1 * vx1 - x1) / (t2 - t1)

x1, y1, z1, vx1, vy1, vz1 = input[0]
x2, y2, z2, vx2, vy2, vz2 = input[1]

def try(input, p0)
  all_intersect = input.all? do |p|
    t = maybe_intersection_time(p0, p)
    t && t >= 0 && intersect_at_t(p0, p, t)
  end

  if all_intersect
    p p0
    p p0[0..2].sum
    exit
  end
end

def try_v(input, t1, vx0, vy0, vz0)
  x1, y1, z1, vx1, vy1, vz1 = input[0]

  x0 = x1 + vx1 * t1 - t1 * vx0
  y0 = y1 + vy1 * t1 - t1 * vy0
  z0 = z1 + vz1 * t1 - t1 * vz0

  p0 = [x0, y0, z0, vx0, vy0, vz0]
  # p [t1, t2, p0]
  try(input, p0)
end

(1..Float::INFINITY).each do |max_t|
  p max_t
  (0..max_t).each do |t2|
    (0..max_t).each do |t1|
      next if t1 == t2

      qx = t2 * vx2 + x2 - t1 * vx1 - x1
      qy = t2 * vy2 + y2 - t1 * vy1 - y1
      qz = t2 * vz2 + z2 - t1 * vz1 - z1

      if qx % (t2 - t1) == 0 && qy % (t2 - t1) == 0 && qz % (t2 - t1) == 0
        vx0 = qx / (t2 - t1)
        vy0 = qy / (t2 - t1)
        vz0 = qz / (t2 - t1)

        try_v(input, t1, vx0, vy0, vz0)
      end
    end
  end
end
