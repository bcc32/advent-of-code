$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  insns = f.readlines.map do |line|
    case line
    when /addx (-?\d+)/
      [:add, $1.to_i]
    when /noop/
      [:noop, 0]
    else
      fail
    end
  end
end

x = 1
sum = 0

pixels = '.' * 240

240.times do |cycle|
  if (x-1..x+1) === cycle % 40
    pixels[cycle] = '#'
  end

  insn, count = input.shift
  case insn
  when nil
  when :add
    input.insert(0, [:add1, count])
  when :add1
    x += count
  when :noop
  else
    fail
  end
end

pixels.chars.each_slice(40).each do |line|
  puts line.join
end
