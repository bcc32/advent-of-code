$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    grid, counts = line.chomp.split
    counts = counts.split(/,/).map(&:to_i)
    [([grid] * 5).join('?'), counts * 5]
  end
end

$memo = {}
def try(grid, current_streak, counts)
  # p [grid, current_streak, counts]

  mem = $memo[[grid, current_streak, counts]]
  return mem if mem

  result =
    if grid.empty?
      if counts[0] && counts[0] == current_streak
        counts = counts[1..]
        current_streak = 0
      end
      if counts.empty? && current_streak == 0
        # p 'success'
        1
      else
        0
      end
    elsif counts[0] && current_streak > counts[0]
      0
    else
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

  $memo[[grid, current_streak, counts]] = result
end

# p try('?###????????', 0, [3,2,1])
# p try('???.###', 0, [1,1,3])
# p try('?###????????', 0, [3,2,1])

p (input.map do |grid, counts|
     try(grid, 0, counts)
   end.sum)
