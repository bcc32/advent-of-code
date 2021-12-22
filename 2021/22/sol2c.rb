input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    next if line =~ /^#/#
    line =~ /(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/
    [($1 == 'on'), $2.to_i, $3.to_i, $4.to_i, $5.to_i, $6.to_i, $7.to_i]
  end.compact
end

class IntervalMap
  # non-overlapping
  def initialize
    @intervals = [[-Float::INFINITY, nil]]
  end

  def set_or_yield(min, max, value)
    all_starting_points = @intervals.map(&:first) + [min, (max + 1)]
    all_starting_points.sort!

    all_starting_points.each_cons(2) do |start1, start2|
      start1, data1 = i1
      start2, data2 = i2

      if (min..max) === start1
        if data1.nil?
          yield data1
        else
          value
        end
      end
    end

    all_intervals = @intervals.flat_map { |range, data|
      rmin, rmax = range
      if (rmin..rmax) === min && (rmin..rmax) === max
        [[rmin, min - 1], [min, max], [max + 1, rmax]].map do |range|
          if range[1] >= range[0]
            [range, data]
          end
        end.compact
      elsif (rmin..rmax) === min
        [[rmin, min - 1], [min, rmax]].map do |range|
          if range[1] >= range[0]
            [range, data]
          end
        end.compact
      elsif (rmin..rmax) === max
        [[rmin, max - 1], [max, rmax]].map do |range|
          if range[1] >= range[0]
            [range, data]
          end
        end.compact
      else
        [range, data]
      end
    }

    @intervals.each_cons(2) do |(range1, data1), (range2, data2)|
      if range[1] + 1 >= min
      end
    end

    p all_intervals

    all_intervals.each do |range, data|
      if (min..max) === range[0]
        yield data
      end
    end

    invariant
  end

  def invariant
    @intervals.each do |a|
      unless a[0][0] <= a[0][1]
        p [min, max, @intervals]
        fail
      end
    end

    @intervals.each_cons(2).all? { |a, b|
      unless a[0][1] < b[0][0]
        p [min, max, @intervals]
        fail
      end
    }
  end

  def map_regions
    @intervals.map do |range, data|
      min, max = range
      yield min, max, data
    end
  end
end

# x regions, containing y regions, containing z regions
regions_on = IntervalMap.new

input.each do |onoff, x1, x2, y1, y2, z1, z2|
  zrange = IntervalMap.new
  zrange.set_or_yield(z1, z2, [onoff]) {}

  yrange = IntervalMap.new
  yrange.set_or_yield(y1, y2, zrange) {}
  regions_on.set_or_yield(x1, x2, yrange) do |yrange|
    yrange.set_or_yield(y1, y2, zrange) do |zrange|
      zrange.set_or_yield(z1, z2, [onoff]) do |a|
        a[0] = onoff
      end
    end
  end
end

p regions_on

p (regions_on.map_regions do |xmin, xmax, yrange|
     (xmax - xmin + 1) * (yrange.map_regions do |ymin, ymax, zrange|
                            (ymax - ymin + 1) * zrange.map_regions do |zmin, zmax, onoff|
                              (zmax - zmin + 1) * (onoff[0] ? 1 : 0)
                            end.sum
                          end.sum)
   end.sum)
