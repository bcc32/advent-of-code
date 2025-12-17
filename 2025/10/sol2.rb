$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    parts = line.split(' ')
    goal = parts.shift[1...-1]
    joltage = parts.pop[1...-1].split(',').map(&:to_i)
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

  best_button = buttons.map(&:sum).max
  # FIXME: way too slow, we're spending all our time hashing.
  heur = Hash.new do |h, k|
    h[k] = k.zip(goal).map { |k, g| (k - g).abs }.sum.to_f / best_button
  end

  until queue.empty?
    x = queue.shift
    return dist[x] if x == goal

    buttons.each do |b|
      y = x.zip(b).map { |x, b| x+b }
      unless y.zip(goal).any? { |y, g| y > g }
        unless dist[y]
          dist[y] = dist[x] + 1
          queue << y
        end
      end
    end

    if rand(100) == 0
      # approximate A*
      queue.sort_by! do |x|
        dist[x] + heur[x]
      end
    end
  end

  raise "no solution"
end

ans = []
input.each do |goal, buttons, joltage|
  buttons_as_diffs = buttons.map do |b|
    x = [0] * joltage.size
    b.each { |i| x[i] = 1 }
    x
  end

  ans << bfs([0] * joltage.size, joltage, buttons_as_diffs)
  p ans
end

p ans.sum
