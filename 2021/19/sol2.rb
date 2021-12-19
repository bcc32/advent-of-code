class Scanner
  def initialize(coords)
    @coords = coords
  end

  attr_reader :coords

  def orientations
    fx, fy, fz = coords.first

    [ # y up
      coords.map { |x, y, z| [x, y, z] },
      coords.map { |x, y, z| [-z, y, x] },
      coords.map { |x, y, z| [-x, y, -z] },
      coords.map { |x, y, z| [z, y, -x] },
      # x up
      coords.map { |x, y, z| [z, x, y] },
      coords.map { |x, y, z| [-y, x, z] },
      coords.map { |x, y, z| [-z, x, -y] },
      coords.map { |x, y, z| [y, x, -z] },
      # z up,
      coords.map { |x, y, z| [y, z, x] },
      coords.map { |x, y, z| [-x, z, y] },
      coords.map { |x, y, z| [-y, z, -x] },
      coords.map { |x, y, z| [x, z, -y] },
    ].map { |coords| [ self.class.new(coords) , self.class.new(coords.map { |coord| coord.map { |x| -x }.reverse })] }.flatten(1)

    # UGH, I spent about 30 minutes debugging why my solution wasn't working,
    # and I didn't know that I needed [reverse] above.  Should have just looked
    # at my diagram for like 30 seconds.
  end
end

scanners = File.open('aoc.in') do |f|
  groups = f.read.split(/\n\n/)
  groups.map do |g|
    lines = g.lines.drop(1)
    lines.map do |line|
      line.split(/,/).map(&:to_i)
    end
  end.map { |coords| Scanner.new(coords) }
end

def diff(coord1, coord2)
  coord1.zip(coord2).map { |x, y| x - y }
end

# each group's scanners have coordinates relative to the coordinate system of
# the first scanner in the group

def reconcile(scanner1, scanner2)
  scanner2.orientations.each do |orient|
    translations = []
    orient.coords.each do |u|
      scanner1.coords.each do |v|
        translations << diff(u, v)
      end
    end

    translations.group_by(&:itself).values.each do |a|
      if a.size >= 12
        v = a.first
        return [Scanner.new(orient.coords.map { |c| diff(c, v) }), v]
      end
    end
  end

  nil
end

scanners_grouped = [scanners.shift]
positions = [[0,0,0]]
until scanners.empty?
  scanners.each_with_index do |scanner, scanner_index|
    found = false
    r = nil
    scanners_grouped.each_with_index do |scanner2, scanner2_index|
      r = reconcile(scanner2, scanner)
      if r
        r, v = r
        positions << v
        scanners_grouped << r
        scanners.delete_at(scanner_index)
        found = true
        break
      end
    end

    if found
      break
    end
  end
end

p positions.flat_map { |p1|
  positions.map { |p2|
    diff(p1, p2).map(&:abs).sum
  }
}.max
