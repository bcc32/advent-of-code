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

max_y_for_success = -Float::INFINITY
max_vy.downto(ymin).find do |vy|
  (min_vx..max_vx).any? do |vx|
    best = [-Float::INFINITY]
    if simulate(vx, vy, xmin, xmax, ymin, ymax, best)
      max_y_for_success = [max_y_for_success, best[0]].max
    end
  end
end

p max_y_for_success
