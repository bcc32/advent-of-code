$stdout = File.open('aoc.out', 'w')
s = IO.read('aoc.in')
c = 14
s.chars.each_cons(14) do |slice|
  if slice.uniq.size == 14
    p c
    break
  else
    c += 1
  end
end
