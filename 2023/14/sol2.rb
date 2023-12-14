$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp).map(&:chars)
end

cols = input.transpose.map(&:join)
rows = cols[0].size

def go_north!(cols)
  cols.map! do |c|
    c.gsub!(/[O.]+/) { |m| m.chars.sort.reverse.join }
  end
end

def rotate_cw(cols)
  cols.map(&:chars).transpose.reverse.map(&:join)
end

def do_cycle!(cols)
  4.times do
    go_north! cols
    cols = rotate_cw cols
  end
  cols
end

def floyd(cols)
  tortoise = cols.map(&:dup)
  tortoise = do_cycle!(tortoise)

  hare = cols.map(&:dup)
  hare = do_cycle!(hare)
  hare = do_cycle!(hare)

  while tortoise != hare
    tortoise = do_cycle!(tortoise)
    hare = do_cycle!(hare)
    hare = do_cycle!(hare)
  end

  mu = 0
  tortoise = cols.map(&:dup)

  while tortoise != hare
    tortoise = do_cycle!(tortoise)
    hare = do_cycle!(hare)
    mu += 1
  end

  lam = 1
  hare = do_cycle!(tortoise.map(&:dup))
  while tortoise != hare
    hare = do_cycle!(hare)
    lam += 1
  end

  # p [lam, mu]
  offset = (1_000_000_000 - mu) % lam
  x = cols.map(&:dup)
  (mu + offset).times do
    x = do_cycle!(x)
  end

  x
end

cols = floyd(cols)

# cols.map(&:chars).transpose.each { |s| puts(s.join) }

p (cols.map do |c|
   c.chars.each_with_index.map do |x, i|
     x == 'O' ? (rows - i) : 0
   end.sum
 end.sum)

# 3.times do
# cols = do_cycle! cols
# puts
# cols.map(&:chars).transpose.each { |s| puts(s.join) }
# end
