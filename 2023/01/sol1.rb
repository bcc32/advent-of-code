$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map(&:chomp)
end

pat = /\d|one|two|three|four|five|six|seven|eight|nine/

def dig(s)
  case s
  when 'one'
    '1'
  when 'two'
    '2'
  when 'three'
    '3'
  when 'four'
    '4'
  when 'five'
    '5'
  when 'six'
    '6'
  when 'seven'
    '7'
  when 'eight'
    '8'
  when 'nine'
    '9'
  else
    s
  end
end

x = input.map do |s|
  matches = s.size.times.map do |i|
    s.match(pat, i)
  end.compact

  (dig(matches.first.to_s) + dig(matches.last.to_s)).to_i
end.sum

p x
