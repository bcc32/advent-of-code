$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp).map(&:chars)
end

cols = input.transpose
rows = cols[0].size

p (cols.map do |c|
     c = c.join
     c.gsub!(/[O.]+/) { |m| m.chars.sort.reverse.join}
     c = c.chars
     c.each_with_index.map do |x, i|
       x == 'O' ? (rows - i) : 0
     end.sum
   end.sum)
