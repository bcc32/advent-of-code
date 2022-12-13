$stdout = File.open('aoc.out', 'w')

def parse(line)
  eval(line)
end

dividers = [[[2]], [[6]]]
packets = dividers.dup
input = File.open('aoc.in') do |f|
  f.read.split(/\n\n/).map do |group|
    group.lines.each do |line|
      packets << parse(line)
    end
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

  if a.empty? && b.empty?
    0
  elsif a.empty?
    -1
  elsif b.empty?
    1
  end
end

packets.sort! { |a, b| compare(a, b) }
p (packets.index(dividers[0]) + 1) * (packets.index(dividers[1]) + 1)
