$stdout = File.open('aoc.out', 'w')
instructions, $map = File.open('aoc.in') do |f|
  instructions = f.readline.chomp
  f.readline
  $map = f.readlines.map do |line|
    line =~ /^(...) = \((...), (...)\)/ or raise
    from = $1
    to_left = $2
    to_right = $3
    [from, [to_left, to_right]]
  end
  [instructions, $map.to_h]
end

points = $map.keys.select { |k| k.end_with?('A') }

$orig_instructions = instructions

def how_many_steps(start)
  steps = 0
  instructions = $orig_instructions.chars

  until start.end_with?('Z')
    if instructions.empty?
      instructions = $orig_instructions.chars
    end

    case instructions.shift
    when 'L'
      start = $map[start][0]
    when 'R'
      start = $map[start][1]
    end

    steps += 1
  end

  steps
end

p (points.map { |p| how_many_steps(p) }.reduce(&:lcm))
