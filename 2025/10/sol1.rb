$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    parts = line.split(' ')
    goal = parts.shift[1...-1]
    joltage = parts.pop
    buttons = parts.map { |p| p[1...-1].split(',').map(&:to_i) }
    [goal, buttons, joltage]
  end
end

# return number of steps
def bfs(start, goal, buttons)
  queue = []
  queue << start
  dist = {}
  dist[start] = 0

  until queue.empty?
    x = queue.shift
    return dist[x] if x == goal

    buttons.each do |b|
      y = x ^ b
      unless dist[y]
        dist[y] = dist[x] + 1
        queue << y
      end
    end
  end

  raise "no solution"
end

ans = []
input.each do |goal, buttons, joltage|
  goal_bitmap = goal.chars.map.with_index do |c, i|
    if c == '#'
      1 << i
    else
      0
    end
  end.inject(&:|)

  buttons_as_bitmaps = buttons.map do |b|
    b.map { |i| 1 << i }.inject(&:|)
  end

  ans << bfs(0, goal_bitmap, buttons_as_bitmaps)
end

p ans.sum
