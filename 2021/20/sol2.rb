alg, grid = File.open('aoc.in') do |f|
  alg = f.readline.chomp
  f.readline
  grid = f.readlines.map(&:chomp)
  [alg, grid]
end

def tick(alg, grid, flip)
  (-1..grid.size).map do |x|
    (-1..grid[0].size).map do |y|
      bits = []
      (-1..1).each do |dx|
        (-1..1).each do |dy|
          if (0...grid.size) === x + dx && (0...grid[0].size) === y + dy
            bits << (grid[x + dx][y + dy] == '#' ? 1 : 0)
          else
            bits << (flip ? 1 : 0)
          end
        end
      end

      index = bits.join.to_i(2)
      alg[index]
    end
  end
end

# if flip, then out-of-bounds pixels are lit, else dark
flip = false
50.times do
  grid = tick(alg, grid, flip)
  if alg[0] == '#'
    flip = !flip
  end
end

p grid.flatten.count('#')
