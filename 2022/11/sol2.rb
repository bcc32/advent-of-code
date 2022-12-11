require './common'

$stdout = File.open('aoc.out', 'w')

monkeys = File.open('aoc.in') do |f|
  f.read.split(/\n\n/).map do |group|
    Monkey.new(group.lines)
  end
end

modulus = monkeys.map(&:divisibility_test).reduce(&:lcm)
midpoint = lambda { |item| item = item % modulus }

10_000.times do
  monkeys.each_with_index do |monkey, index|
    monkey.act monkeys, midpoint
  end
end

p monkeys.map(&:inspection_count).max(2).reduce(&:*)
