class Monkey
  def initialize(lines)
    @inspection_count = 0

    lines.each do |line|
      case line
      when /Monkey \d/
      when /Starting items: ((\d+)(, \d+)*)/
        @items = $1.split(/, /).map(&:to_i)
      when /Operation: new = old ([*+]) (\d+|old)/
        op = $1
        operand = $2 == 'old' ? :old : $2.to_i
        @operation = lambda do |item|
          operand_value = operand == :old ? item : operand
          case op
          when '*'
            item * operand_value
          when '+'
            item + operand_value
          else
            fail
          end
        end
      when /Test: divisible by (\d+)/
        @divisibility_test = $1.to_i
      when /If true: throw to monkey (\d+)/
        @true_dst = $1.to_i
      when /If false: throw to monkey (\d+)/
        @false_dst = $1.to_i
      else
        fail line
      end
    end
  end

  attr_reader :items, :inspection_count, :divisibility_test

  def act(monkeys, midpoint)
    until @items.empty?
      item = @items.shift
      @inspection_count += 1
      item = @operation[item]
      item = midpoint[item]
      if item % @divisibility_test == 0
        monkeys[@true_dst].items << item
      else
        monkeys[@false_dst].items << item
      end
    end
  end
end
