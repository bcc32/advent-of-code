require_relative 'common'

xmin, xmax, ymin, ymax = File.open('aoc.in') do |f|
  f.read =~ /x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)/
  xmin, xmax, ymin, ymax = $1.to_i, $2.to_i, $3.to_i, $4.to_i
end

fail unless xmin > 0
fail unless ymax < 0

# All trajectories where init_vy > 0 return to exactly y = 0 because of
# symmetry.  If, in only one timestep, the probe passes ymin, then init_vy is
# clearly too large.
max_vy = ymin.abs

max_vx = xmax
min_vx = (0..Float::INFINITY).find do |vx|
  vx * (vx + 1) / 2 >= xmin
end

count = 0
max_vy.downto(ymin).each do |vy|
  (min_vx..max_vx).each do |vx|
    best = [-Float::INFINITY]
    if simulate(vx, vy, xmin, xmax, ymin, ymax, best)
      count += 1
    end
  end
end

p count
