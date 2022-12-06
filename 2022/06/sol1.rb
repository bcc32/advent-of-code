$stdout = File.open('aoc.out', 'w')
s = IO.read('aoc.in')
c = 4
s.chars.each_cons(4) do |slice|
  if slice.uniq.size == 4
    p c
    break
  else
    c += 1
  end
end
