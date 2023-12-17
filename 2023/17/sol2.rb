require 'set'

$stdout = File.open('aoc.out', 'w')
grid = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp).map do |line|
    line.chars.map(&:to_i)
  end
end

def dijkstra(grid)
  heat_lost = {}
  q = [[0, 0, 'X']]
  heat_lost[q[0]] = 0
  until q.empty?
    i = q.min_by { |x| heat_lost[x] }
    orig_x, orig_y, dir = q.delete(i)
    if heat_lost.size % 1024 == 0
      # p heat_lost.size
    end
    hl = heat_lost[[orig_x, orig_y, dir]]

    [[-1, 0, 'U'], [1, 0, 'D'], [0, -1, 'L'], [0, 1, 'R']].each do |dx, dy, d|
      x = orig_x
      y = orig_y

      next if dir == d
      # can't reverse
      next if [d, dir].sort == ['D', 'U'] || [d, dir].sort == ['L', 'R']

      hl = heat_lost[[x, y, dir]]

      (1..10).each do |steps|
        x += dx
        y += dy
        break unless (0...grid.size) === x && (0...grid[0].size) === y
        hl += grid[x][y]
        next if steps < 4

        if !heat_lost[[x, y, d]] || hl < heat_lost[[x, y, d]]
          heat_lost[[x, y, d]] = hl
          q << [x, y, d]
          # p [x, y, d]
        end
      end
    end
  end

  # p heat_lost.keys

  heat_lost.to_a.
    select { |(x, y, _), _| x == grid.size - 1 && y == grid[0].size - 1 }.
    map(&:last).
    min
end

p dijkstra(grid)
