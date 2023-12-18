require 'set'
$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line =~ /([LURD]) (\d+) \(#([[:xdigit:]]{6})\)/
    # dir = $1
    # n = $2.to_i
    # color = $3
    n = $3[0...5].to_i(16)
    dir = case $3.chars.last
          when '0'
            'R'
          when '1'
            'D'
          when '2'
            'L'
          when '3'
            'U'
          end
    [dir, n]
  end
end

x, y = [0, 0]

key_points = Set.new
horiz_edges_by_y = Hash.new { |h, k| h[k] = [] }

input.each do |dir, n|
  x2, y2 = case dir
           when 'L'
             [x - n, y]
           when 'R'
             [x+n, y]
           when 'U'
             [x, y+n]
           when 'D'
             [x, y-n]
           end

  if y == y2
    horiz_edges_by_y[y] << [x, x2].sort
  end

  x, y = [x2, y2]
  key_points << [x, y]
end

key_x_points = key_points.map(&:first).uniq.sort
key_y_points = key_points.map(&:last).uniq.sort

dug_blocks = []

key_x_points.each_cons(2).with_index do |(x1, x2), i|
  am_inside = false

  key_y_points.each_cons(2).with_index do |(y1, y2), j|
    if horiz_edges_by_y[y1].any? { |x1e, x2e| (x1e..x2e) === x1 && (x1e..x2e) === x2 }
      am_inside = !am_inside
    end

    if am_inside
      dug_blocks << [x1, y1, x2, y2, i, j]
    end
  end
end

dug_block_coords = dug_blocks.map { |_, _, _, _, i, j| [i, j] }.to_set

dug_count = 0

corner_counts = Hash.new(0)

dug_blocks.each do |x1, y1, x2, y2, i, j|
  left_overlap = dug_block_coords.include?([i - 1, j])
  bottom_overlap = dug_block_coords.include?([i, j - 1])

  dug_count += (x2 - x1 + 1) * (y2 - y1 + 1)

  if left_overlap
    dug_count -= (y2 - y1 - 1)
  end

  if bottom_overlap
    dug_count -= (x2 - x1 - 1)
  end

  corner_counts[[x1, y1]] += 1
  corner_counts[[x1, y2]] += 1
  corner_counts[[x2, y1]] += 1
  corner_counts[[x2, y2]] += 1
end

dug_count -= corner_counts.values.map { |c| (c <= 1 ? 0 : c - 1) }.sum

p dug_count
