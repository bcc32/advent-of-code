$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line.split.map(&:to_i)
  end
end

def analyze(history)
  return [0]+history if history.all? { |h| h == 0 }

  n = history.each_cons(2).map { |x, y| y - x }
  n = analyze(n)
  [history.first - n.first]+history
end

sum = 0
input.each do |history|
  sum += analyze(history).first
end

p sum
