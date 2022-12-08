$stdout = File.open('aoc.out', 'w')
grid = IO.read('aoc.in').lines.map(&:chomp).map(&:chars)

scores = []

(0...grid.size).each do |i|
  grid[0].size.times do |j|
    score = 1

    c = 0
    last = '0'
    (0...i).to_a.reverse.each do |ii|
      c +=1
      if grid[ii][j] >= grid[i][j]
        break
      end
    end
    score *= c

    c = 0
    last = '0'
    (i+1...grid.size).each do |ii|
      c +=1
      if grid[ii][j] >= grid[i][j]
        break
      end
    end
    score *= c

    c = 0
    last = '0'
    (0...j).to_a.reverse.each do |jj|
      c +=1
      if grid[i][jj] >= grid[i][j]
        break
      end
    end
    score *= c

    c = 0
    last = '0'
    (j+1...grid[0].size).each do |jj|
      c +=1
      if grid[i][jj] >= grid[i][j]
        break
      end
    end
    score *= c

    scores << score
  end

end

p scores.max
