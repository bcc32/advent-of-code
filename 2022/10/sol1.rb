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

220.times do |cycle|
  if [20,60,100,140,180,220].include?(cycle+1)
    sum += (cycle + 1) * x
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

p sum
