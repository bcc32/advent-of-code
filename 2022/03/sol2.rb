def score(l)
  case l
  when 'a'..'z'
    l.ord - 'a'.ord + 1
  when 'A'..'Z'
    l.ord - 'A'.ord + 27
  end
end

sum = 0

IO.read('aoc.in').lines.each_slice(3) do |lines|
  common = (lines[0].chars & lines[1].chars & lines[2].chars)[0]
  sum += score(common)
end

p sum
