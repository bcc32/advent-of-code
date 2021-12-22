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
    @intervals = []
  end

  def set(min, max, value)
    idx1 = @intervals.bsearch_index { |range, data|
      rmin, rmax = range
      rmax >= min
    }

    unless idx1
      @intervals << [[min, max], value]
      return
    end

    range, data = @intervals[idx1]

    if max >= range[0] && min <= range[1]
      idx2 = @intervals.bsearch_index { |range, data|
        rmin, rmax = range
        rmin > max
      } || idx1

      idx2range, idx2data = @intervals[idx2]

      idx2.downto(idx1).each { |i| @intervals.delete_at(i) }

      # intersect

      new_intervals = []

      if range[0] < min
        # left remaining
        new_intervals << [[range[0], min - 1], data.dup]
      end

      new_intervals << [[min, max], value]

      if idx2range[1] > max
        # right remaining
        new_intervals << [[max + 1, idx2range[1]], idx2data.dup]
      end

      @intervals.insert(idx1, *new_intervals)

      # @intervals.sort_by!(&:first)
    end

    @intervals.each_cons(2).all? { |a, b|
      unless a[0][1] < b[0][0]
        p [min, max, range, @intervals]
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
  zrange.set(z1, z2, onoff)
  yrange = IntervalMap.new
  yrange.set(y1, y2, zrange)
  regions_on.set(x1, x2, yrange)
end

p regions_on

p (regions_on.map_regions do |xmin, xmax, yrange|
     (xmax - xmin + 1) * (yrange.map_regions do |ymin, ymax, zrange|
                            (ymax - ymin + 1) * zrange.map_regions do |zmin, zmax, onoff|
                              (zmax - zmin + 1) * (onoff ? 1 : 0)
                            end.sum
                          end.sum)
   end.sum)
