input = File.open('aoc.in') do |f|
  f.readline.chars.map { |c| '%04b' % c.hex }.join.chars
end

class Packet
  def initialize(version, type, children)
    @version = version
    @type = type
    @children = children
  end

  attr_reader :version, :type, :children

  def version_sum
    sum = version
    children.each { |c| sum += c.version_sum }
    sum
  end
end

def read_packet(bits)
  return if bits.empty?

  version = bits.shift(3).join.to_i(2)
  type = bits.shift(3).join.to_i(2)
  children = []
  case type
  when 4
    literal_bits = []
    loop do
      group = bits.shift(5)
      literal_bits.concat group[1, 4]
      if group[0].to_i(2) == 0
        break
      end
    end
  else
    length_type_id = bits.shift
    case length_type_id.to_i(2)
    when 0
      bit_length_of_children = bits.shift(15).join.to_i(2)
      children_bits = bits.shift(bit_length_of_children)
      while child = read_packet(children_bits)
        children << child
      end
    when 1
      num_children = bits.shift(11).join.to_i(2)
      num_children.times { children << read_packet(bits) }
    else
      fail
    end
  end

  Packet.new(version, type, children)
end

p read_packet(input).version_sum
