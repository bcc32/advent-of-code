$stdout = File.open('aoc.out', 'w')
instructions, map = File.open('aoc.in') do |f|
  instructions = f.readline.chomp
  f.readline
  map = f.readlines.map do |line|
    line =~ /^(...) = \((...), (...)\)/ or raise
    from = $1
    to_left = $2
    to_right = $3
    [from, [to_left, to_right]]
  end
  [instructions, map.to_h]
end

point = 'AAA'

orig_instructions = instructions
instructions = orig_instructions.chars
steps = 0

while point != 'ZZZ'
  if instructions.empty?
    instructions = orig_instructions.chars
  end

  case instructions.shift
  when 'L'
    point = map[point][0]
  when 'R'
    point = map[point][1]
  end

  steps += 1
end

p steps
