$stdout = File.open('aoc.out', 'w')

c = 0

IO.read('aoc.in').lines.each do |line|
  a, b = line.split(',')
  a1, a2 = a.split('-').map(&:to_i)
  b1, b2 = b.split('-').map(&:to_i)
  c += 1 if (b1..b2) === a1 && (b1..b2) === a2 ||
            (a1..a2) === b1 && (a1..a2) === b2
end

p c
