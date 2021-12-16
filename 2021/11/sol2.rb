require 'set'

grid = File.open('input') do |f|
  f.lines.map(&:chomp).map { |line| line.chars.map(&:to_i) }
end

$flashes = 0

def tick(grid)
  grid.each { |row| row.map!(&:succ) }
  desired_size = grid.map(&:size).sum
  flashes_this_tick = Set.new
  loop do
    new_flashes = false
    grid.each_with_index do |row, i|
      row.each_with_index do |cell, j|
        if cell > 9 && !flashes_this_tick.include?([i, j])
          $flashes += 1
          flashes_this_tick << [i, j]
          new_flashes = true
          (-1..1).each do |di|
            (-1..1).each do |dj|
              next if di == 0 && dj == 0
              next unless (0...grid.size) === i + di &&
                          (0...grid[0].size) === j + dj
              grid[i + di][j + dj] += 1
            end
          end
        end
      end
    end
    break unless new_flashes
  end

  grid.each { |row| row.map! { |x| x > 9 ? 0 : x } }

  flashes_this_tick.size == desired_size
end

p ((1..Float::INFINITY).find do |i|
  tick grid
end)
