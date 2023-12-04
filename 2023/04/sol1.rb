$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    winning, have = line.split('|')
    winning = winning.scan(/\d+/)[1..].map(&:to_i)
    have = have.scan(/\d+/).map(&:to_i)
    [winning, have]
  end
end

points = 0
input.each do |card|
  winning, have = card
  have_good = have.count { |x| winning.include?(x) }
  if have_good > 0
    score = 2**(have_good - 1)
  else
    score = 0
  end

  points += score
end

p points
