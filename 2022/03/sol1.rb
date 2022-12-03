def common(s1, s2)
  (s1.chars & s2.chars).first
end

def score(l)
  case l
  when 'a'..'z'
    l.ord - 'a'.ord + 1
  when 'A'..'Z'
    l.ord - 'A'.ord + 27
  end
end

sum = 0

IO.read('aoc.in').lines.each do |line|
  a = line[0, line.size / 2]
  b = line[line.size / 2..-1]
  sum += score(common(a, b))
end

p sum
