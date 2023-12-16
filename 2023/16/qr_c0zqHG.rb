require 'set'

$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp)
end

def advance(r, c, dir)
  case dir
  when 'L'
    c -= 1
  when 'R'
    c += 1
  when 'D'
    r += 1
  when 'U'
    r -= 1
  end

  [r, c]
end

def follow(grid, r, c, dir)
  energized = Set.new
  queue = []
  queue << [r, c, dir]
  visited = Set.new

  until queue.empty?
    r, c, dir = queue.shift

    if r < 0 || r >= grid.size || c < 0 || c >= grid[0].size
      next
    end
    next if visited.include?([r, c, dir])
    visited << [r, c, dir]

    energized << [r, c]

    case grid[r][c]
    when '.'
      r, c = advance(r, c, dir)
      queue << [r, c, dir]
    when '/'
      case dir
      when 'L'
        queue << [r + 1, c, 'D']
      when 'R'
        queue << [r - 1, c, 'U']
      when 'U'
        queue << [r, c + 1, 'R']
      when 'D'
        queue << [r, c - 1, 'L']
      end

    when '\\'
      case dir
      when 'L'
        queue << [r - 1, c, 'U']
      when 'R'
        queue << [r + 1, c, 'D']
      when 'U'
        queue << [r, c - 1, 'L']
      when 'D'
        queue << [r, c + 1, 'R']
      end

    when '|'
      case dir
      when /[UD]/
        r, c = advance(r, c, dir)
        queue << [r, c, dir]

      when /[LR]/
        queue << [r - 1, c, 'U']
        queue << [r + 1, c, 'D']
      end

    when '-'
      case dir
      when /[LR]/
        r, c = advance(r, c, dir)
        queue << [r, c, dir]

      when /[UD]/
        queue << [r, c - 1, 'L']
        queue << [r, c + 1, 'R']
      end
    end
  end
  energized
end

starts =
  input.size.times.map { |r| [r, 0, 'R'] } +
  input.size.times.map { |r| [r, input[0].size - 1, 'L'] } +
  input[0].size.times.map { |c| [0, c, 'D'] } +
  input[0].size.times.map { |c| [input.size-1, c, 'U'] }


p starts.map { |r, c, dir| follow(input, r, c, dir).size }.max
