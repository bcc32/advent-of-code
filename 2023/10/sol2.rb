$stdout = File.open('aoc.out', 'w')
$grid = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line.chomp.chars
  end
end

$adj = Hash.new { |h, k| h[k] = [] }

start = nil

$grid.size.times do |i|
  $grid[0].size.times do |j|
    case $grid[i][j]
    when '|'
      $adj[[i,j]] << [i-1,j]
      $adj[[i,j]] << [i+1,j]
      # $adj[[i-1,j]] << [i,j]
      # $adj[[i+1,j]] << [i,j]
    when '-'
      $adj[[i,j]] << [i,j-1]
      $adj[[i,j]] << [i,j+1]
      # $adj[[i,j-1]] << [i,j]
      # $adj[[i,j+1]] << [i,j]
    when 'L'
      $adj[[i,j]] << [i-1,j]
      $adj[[i,j]] << [i,j+1]
      # $adj[[i-1,j]] << [i,j]
      # $adj[[i,j+1]] << [i,j]
    when 'J'
      $adj[[i,j]] << [i-1,j]
      $adj[[i,j]] << [i,j-1]
      # $adj[[i-1,j]] << [i,j]
      # $adj[[i,j-1]] << [i,j]
    when '7'
      $adj[[i,j]] << [i+1,j]
      $adj[[i,j]] << [i,j-1]
      # $adj[[i+1,j]] << [i,j]
      # $adj[[i,j-1]] << [i,j]
    when 'F'
      $adj[[i,j]] << [i+1,j]
      $adj[[i,j]] << [i,j+1]
      # $adj[[i+1,j]] << [i,j]
      # $adj[[i,j+1]] << [i,j]
    when 'S'
      start = [i,j]
      $adj[[i,j]] << [i-1,j]
      $adj[[i,j]] << [i+1,j]
      $adj[[i,j]] << [i,j-1]
      $adj[[i,j]] << [i,j+1]
    end
  end
end

raise unless start

queue = [start]
$dist = { start => 0 }
until queue.empty?
  i, j = queue.shift
  $adj[[i,j]].uniq.each do |ii,jj|
    next if $dist[[ii,jj]]
    next if ii < 0 || ii >= $grid.size || jj < 0 || jj >= $grid[0].size
    next unless $adj[[ii,jj]].include?([i,j])
    $dist[[ii,jj]] = $dist[[i,j]]+1
    queue << [ii,jj]
  end
end

enclosed = 0

def count_crossings(i, j)
  if j == $grid[0].size - 1
    return 0
  end

  crossings = 0
  i -= 1
  while i >= 0
    left = [i, j]
    right = [i, j + 1]
    if $adj[left].include?(right) && $adj[right].include?(left) && $dist[left] && $dist[right]
      crossings += 1
    end
    i -= 1
  end
  crossings
end

$grid.size.times do |i|
  $grid[0].size.times do |j|
    if !$dist[[i,j]] || $grid[i][j] == '.'
      # count how many times we cross the main loop
      if count_crossings(i, j).odd?
        p [i, j]
        enclosed += 1
      end
    end
  end
end

p enclosed
