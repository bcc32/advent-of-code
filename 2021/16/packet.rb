class Packet
  def initialize(version, type, literal, children)
    @version = version
    @type = type
    @literal = literal
    @children = children
  end

  attr_reader :version, :type, :literal, :children

  def version_sum
    sum = version
    children.each { |c| sum += c.version_sum }
    sum
  end

  def evaluate
    case type
    when 0
      children.map(&:evaluate).sum
    when 1
      children.map(&:evaluate).reduce(&:*)
    when 2
      children.map(&:evaluate).min
    when 3
      children.map(&:evaluate).max
    when 4
      literal
    when 5
      a, b = children.map(&:evaluate)
      a > b ? 1 : 0
    when 6
      a, b = children.map(&:evaluate)
      a < b ? 1 : 0
    when 7
      a, b = children.map(&:evaluate)
      a == b ? 1 : 0
    end
  end

  class << self
    def consume_bits(bits)
      return if bits.empty?

      version = bits.shift(3).join.to_i(2)
      type = bits.shift(3).join.to_i(2)
      children = []
      literal_bits = []
      case type
      when 4
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
          while child = consume_bits(children_bits)
            children << child
          end
        when 1
          num_children = bits.shift(11).join.to_i(2)
          num_children.times { children << consume_bits(bits) }
        else
          fail
        end
      end

      literal = literal_bits.join.to_i(2)

      self.new(version, type, literal, children)
    end
  end
end
