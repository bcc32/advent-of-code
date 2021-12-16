input = File.open('input') do |f|
  f.map do |line|
    l, r = line.split(/ -> /)
    l = l.split(',').map(&:to_i)
    r = r.split(',').map(&:to_i)
    [l, r]
  end
end

h = Hash.new(0)
input.each do |a, b|
  x1, y1 = a
  x2, y2 = b
  if x1 == x2
    y1, y2 = [y1, y2].sort
    (y1..y2).each do |y|
      h[[x1, y]] += 1
    end
  elsif y1 == y2
    x1, x2 = [x1, x2].sort
    (x1..x2).each do |x|
      h[[x, y1]] += 1
    end
  else
    xs = x1 < x2 ? (x1..x2) : (x2..x1).reverse_each
    ys = y1 < y2 ? (y1..y2) : (y2..y1).reverse_each
    xs.zip(ys).each do |x, y|
      h[[x, y]] += 1
    end
  end
end

p h.values.count { |x| x > 1 }
