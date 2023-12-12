$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    grid, counts = line.chomp.split
    counts = counts.split(/,/).map(&:to_i)
    [grid, counts]
  end
end

def try(grid, current_streak, counts)
  # p [grid, current_streak, counts]

  if grid.empty?
    if counts[0] && counts[0] == current_streak
      counts = counts[1..]
      current_streak = 0
    end
    if counts.empty? && current_streak == 0
      # p 'success'
      return 1
    else
      return 0
    end
  end

  if counts[0] && current_streak > counts[0]
    return 0
  end

  case grid[0]
  when '#'
    try(grid[1..], current_streak + 1, counts)
  when '.'
    if current_streak > 0
      if counts[0] == current_streak
        try(grid[1..], 0, counts[1..])
      else
        0
      end
    else
      try(grid[1..], 0, counts)
    end
  when '?'
    try('#' + grid[1..], current_streak, counts) + \
    try('.' + grid[1..], current_streak, counts)
  end
end

# p try('?###????????', 0, [3,2,1])
# p try('???.###', 0, [1,1,3])
# p try('?###????????', 0, [3,2,1])

p (input.map do |grid, counts|
     try(grid, 0, counts)
end.sum)
