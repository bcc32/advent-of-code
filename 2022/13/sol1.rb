$stdout = File.open('aoc.out', 'w')

def parse(line)
  eval(line)
end

input = File.open('aoc.in') do |f|
  f.read.split(/\n\n/).map do |group|
    group.lines.map { |line| parse(line) }
  end
end

def compare(a, b)
  if a.is_a?(Fixnum) && b.is_a?(Fixnum)
    return a <=> b
  end

  a = [a] if a.is_a?(Fixnum)
  b = [b] if b.is_a?(Fixnum)

  a = a.dup
  b = b.dup

  until a.empty? || b.empty?
    x = a.shift
    y = b.shift
    c = compare(x, y)
    return c if c != 0
  end

  # My bug was not checking this condition first
  if a.empty? && b.empty?
    0
  elsif a.empty?
    -1
  elsif b.empty?
    1
  end
end

s = 0
input.each_with_index do |(a, b), i|
  if compare(a, b) < 0
    s += i + 1
  end
end
p s
