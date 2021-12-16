require 'set'

edges = File.open('input') do |f|
  f.lines.map { |line|
    line.chomp.split(/-/)
  }
end

$can_go = Hash.new { |h, k| h[k] = [] }
edges.each do |x, y|
  $can_go[x] << y
  $can_go[y] << x
end

def is_small(cave)
  cave.chars.all? { |c| c.downcase == c }
end

def dfs(start, goal, small_visited, twice)
  return 1 if start == goal

  # p small_visited

  paths = 0
  $can_go[start].each do |neighbor|
    next if neighbor == 'start'
    next if small_visited[neighbor] >= 1 && twice

    if is_small(neighbor)
      small_visited[neighbor] += 1

      if small_visited[neighbor] >= 2
        this_twice = true
        twice = true
      end
    end

    paths += dfs(neighbor, goal, small_visited, twice)

    if is_small(neighbor)
      small_visited[neighbor] -= 1
      if this_twice
        twice = false
      end
    end
  end

  paths
end

visited = Hash.new(0)
visited['start'] = 1
p dfs('start', 'end', visited, false)
