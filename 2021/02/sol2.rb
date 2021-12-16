input = File.open('input') do |f|
  f.map do |line|
    dir, n = line.split
    n = n.to_i
    [dir, n]
  end
end

depth = 0
aim = 0
x = 0
input.each do |dir, n|
  case dir
  when 'down'
    aim += n
  when 'up'
    aim -= n
  when 'forward'
    x += n
    depth += aim * n
  end
end

p x * depth
