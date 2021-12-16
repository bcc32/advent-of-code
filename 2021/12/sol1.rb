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

def dfs(start, goal, small_visited)
  return 1 if start == goal

  # p small_visited

  paths = 0
  $can_go[start].each do |neighbor|
    next if small_visited.include?(neighbor)
    if is_small(neighbor)
      small_visited << neighbor
    end
    paths += dfs(neighbor, goal, small_visited)
    if is_small(neighbor)
      small_visited.delete(neighbor)
    end
  end

  paths
end

visited = Set.new
visited << 'start'
p dfs('start', 'end', visited)
