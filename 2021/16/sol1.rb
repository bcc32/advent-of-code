require_relative 'packet'

input = File.open('aoc.in') do |f|
  f.readline.chars.map { |c| '%04b' % c.hex }.join.chars
end

p Packet.consume_bits(input).version_sum
