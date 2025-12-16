$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line.split(',').map(&:to_i)
  end
end

def dist2(a, b)
  a.zip(b).map { |x, y| (x-y).to_f ** 2 }.sum
end

$group = Hash.new { |h, k| h[k] = k }
$pairs = [] # array of [dist2, i, j] sorted ascending by dist2, where i < j

input.each_index do |i|
  input.each_index do |j|
    next unless i < j
    d = dist2(input[i], input[j])
    $pairs << [d, i, j]
  end
end
$pairs.sort_by!(&:first)

def group(i)
  return i if $group[i] == i
  $group[i] = group($group[i])
end
def join!(i, j)
  $group[group(i)] = j
end

# weird interpretation, but sure
last_joined = nil
joins = 0
while input.size - joins > 1
  d, i, j = $pairs.shift
  next if group(i) == group(j)

  join!(i, j)
  joins += 1
  last_joined = [i, j]
  # p 'joining', i, j
  # p (input.each_index.group_by { |i| group(i) })
end

ans = input[last_joined[0]][0] * input[last_joined[1]][0]

p ans
