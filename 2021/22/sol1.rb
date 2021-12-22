MIN_COORD = -50
MAX_COORD = 50

input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line =~ /(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/
    [($1 == 'on'), $2.to_i, $3.to_i, $4.to_i, $5.to_i, $6.to_i, $7.to_i]
  end
end

h = {}

input.each do |onoff, x1, x2, y1, y2, z1, z2|
  [x1, MIN_COORD].max.upto([x2, MAX_COORD].min).each do |x|
    [y1, MIN_COORD].max.upto([y2, MAX_COORD].min).each do |y|
      [z1, MIN_COORD].max.upto([z2, MAX_COORD].min).each do |z|
        if onoff
          h[[x, y, z]] = true
        else
          h.delete([x, y, z])
        end
      end
    end
  end
end

p h.size
