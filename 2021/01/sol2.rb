xs = File.open('input') do |f|
  f.readlines.map(&:to_i)
end

p xs.each_cons(3).map(&:sum).each_cons(2).count { |x, y| x < y }
