$stdout = File.open('aoc.out', 'w')
$grid = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp).map(&:chars)
end

starts = []
end_ = nil
$grid.each_with_index do |row, i|
  row.each_with_index do |c, j|
    if c == 'S' || c == 'a'
      starts << [i, j]
    elsif c == 'E'
      end_ = [i, j]
    end
  end
end

def can_get_to?(x, y)
  if x == 'E'
    x = 'z'
  end
  if y == 'S'
    y = 'a'
  end

  x.ord - y.ord <= 1
end

def bfs(start)
  q = [start]
  cost = {}
  cost[q[0]] = 0

  until q.empty?
    i, j = q.shift
    my_cost = cost[[i, j]]

    if $grid[i][j] == 'E'
      return my_cost
    end

    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |di, dj|
      begin
        if i + di >= 0 && j + dj >= 0 && can_get_to?($grid[di + i][dj + j], $grid[i][j]) && !cost.include?([di + i, dj + j])
          q << [i + di, j + dj]
          cost[[di + i, dj + j]] = my_cost + 1
        end
      rescue
      end
    end
  end
end

p starts.map { |start| bfs(start) }.compact.min
