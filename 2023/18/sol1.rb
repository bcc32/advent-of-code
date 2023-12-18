require 'set'
$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line =~ /([LURD]) (\d+) \(#([[:xdigit:]]{6})\)/
    dir = $1
    n = $2.to_i
    color = $3
    [dir, n, color]
  end
end

x, y = [0, 0]

edges = {}
dug = Set.new
dug << [x, y]

first_edge = nil

input.each do |dir, n, color|
  n.times do
    x2, y2 = case dir
             when 'L'
               [x - 1, y]
             when 'R'
               [x+1, y]
             when 'U'
               [x, y+1]
             when 'D'
               [x, y-1]
             end

    edges[[[x, y], [x2, y2]]] = color

    if edges.size == 1
      first_edge = edges.keys[0]
    end

    x, y = [x2, y2]
    dug << [x, y]
  end
end

(x, y), (x2, y2) = first_edge

ymax = dug.map(&:last).max
ymin = dug.map(&:last).min
xmax = dug.map(&:first).max
xmin = dug.map(&:first).min

ymax.downto(ymin).each do |y|
  s = ''
  (xmin..xmax).each do |x|
    s << (dug.include?([x, y]) ? '#' : '.')
  end
  puts s
end

# p [xmax, xmin, ymax, ymin]

# flood fill

bad = Set.new

(xmin..xmax).each do |start_x|
  (ymin..ymax).each do |start_y|
    next if bad.include?([start_x, start_y])
    next if dug.include?([start_x, start_y])

    went_outside = false

    q = [[start_x, start_y]]
    visited = Set.new

    until q.empty?
      x, y = q.pop
      # p [start_x, start_y, x, y]

      if dug.include?([x, y])
        next
      end

      unless (xmin..xmax) === x && (ymin..ymax) === y
        went_outside = true
        break
      end

      if visited.include?([x, y])
        next
      end
      visited << [x, y]

      [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dx, dy|
        q << [x + dx, y + dy]
      end
    end

    if went_outside
      bad.merge(visited)
    else
      # p [start_x, start_y, visited]
      dug.merge(visited)
    end
  end
end

ymax.downto(ymin).each do |y|
  s = ''
  (xmin..xmax).each do |x|
    s << (dug.include?([x, y]) ? '#' : '.')
  end
  puts s
end

p dug.size
