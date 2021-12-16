class CompressedString
  def initialize(str)
    @pairs = ('^' + str + '$').chars.each_cons(2).group_by(&:itself)
               .transform_keys(&:join)
               .transform_values(&:size)
  end

  def iterate(rules)
    new_pairs = Hash.new(0)
    @pairs.each do |k, v|
      l, r = k.chars
      if rhs = rules[k]
        new_pairs[l + rhs] += v
        new_pairs[rhs + r] += v
      else
        new_pairs[l + r] += v
      end
    end
    @pairs = new_pairs
  end

  def char_count
    char_count = Hash.new(0)
    @pairs.each do |k, v|
      k.each_char do |c|
        char_count[c] += v
      end
    end
    char_count.transform_values! { |v| v / 2 }
    char_count.delete('^')
    char_count.delete('$')
    char_count
  end
end
