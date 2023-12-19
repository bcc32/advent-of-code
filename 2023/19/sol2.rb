$stdout = File.open('aoc.out', 'w')
rules = File.open('aoc.in') do |f|
  rules, parts = f.read.split("\n\n")
  rules = rules.lines.map do |line|
    line =~ /^([a-z]+)\{(.*)\}/
    name = $1
    steps = $2.split(',').map do |step|
      if step =~ /:/
        target = $'
        $` =~ /[<>]/
        cmp_var = $`
        cmp_op = $&
        cmp_val = $'
        cmp_val = cmp_val.to_i
        [cmp_var, cmp_op, cmp_val, target]
      else
        step
      end
    end
    [name, steps]
  end

  rules = rules.to_h

  # parts = parts.lines.map do |line|
  #   line =~ /\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}/
  #   [$1.to_i, $2.to_i, $3.to_i, $4.to_i]
  # end

  rules
end

# accept, reject
def split_range(r, cmp_op, cmp_val)
  if cmp_op == '<'
    if r.cover?(cmp_val)
      [r.min..(cmp_val-1), cmp_val..r.max].map { |i| i.size == 0 ? nil : i }
    elsif cmp_val < r.min
      [nil, r]
    else
      raise unless cmp_val > r.max
      [r, nil]
    end
  else
    if r.cover?(cmp_val)
      [(cmp_val+1)..r.max, r.min..cmp_val].map { |i| i.size == 0 ? nil : i }
    elsif cmp_val < r.min
      [r, nil]
    else
      raise unless cmp_val > r.max
      [nil, r]
    end
  end
end

class MyRange
  def initialize(x,m,a,s)
    @x = x
    @m = m
    @a = a
    @s = s
  end

  def size
    @x.size * @m.size * @a.size * @s.size
  end

  attr_reader :x, :m, :a, :s

  # returns list of [range, next_step] pairs
  def apply(rule)
    results = []

    cur_range = self
    rule.each do |step|
      break unless cur_range

      if step.is_a?(Array)
        cmp_var, cmp_op, cmp_val, ptarget = step
        case cmp_var
        when 'x'
          acc, rej = split_range(cur_range.x, cmp_op, cmp_val)
          results << [MyRange.new(acc, cur_range.m, cur_range.a, cur_range.s), ptarget] if acc
          cur_range = rej && MyRange.new(rej, cur_range.m, cur_range.a, cur_range.s)
        when 'm'
          acc, rej = split_range(cur_range.m, cmp_op, cmp_val)
          results << [MyRange.new(cur_range.x, acc, cur_range.a, cur_range.s), ptarget] if acc
          cur_range = rej && MyRange.new(cur_range.x, rej, cur_range.a, cur_range.s)
        when 'a'
          acc, rej = split_range(cur_range.a, cmp_op, cmp_val)
          results << [MyRange.new(cur_range.x, cur_range.m, acc, cur_range.s), ptarget] if acc
          cur_range = rej && MyRange.new(cur_range.x, cur_range.m, rej, cur_range.s)
        when 's'
          acc, rej = split_range(cur_range.s, cmp_op, cmp_val)
          results << [MyRange.new(cur_range.x, cur_range.m, cur_range.a, acc), ptarget] if acc
          cur_range = rej && MyRange.new(cur_range.x, cur_range.m, cur_range.a, rej)
        end

      else
        results << [cur_range, step]
        break
      end
    end

    results
  end
end

accepted_ranges = []
rejected_ranges = []
ranges = [[MyRange.new(1..4000,1..4000,1..4000,1..4000), 'in']]
# ranges = [[MyRange.new(787..788,2655..2655,1222..1222,2876..2876), 'in']]

until ranges.empty?
  new_ranges = []
  ranges.each do |range, which_rule|
    ranges_and_targets = range.apply(rules[which_rule])
    ranges_and_targets.each do |r, next_rule|
      if next_rule == 'A'
        accepted_ranges << r
      elsif next_rule == 'R'
        rejected_ranges << r
      else
        new_ranges << [r, next_rule]
      end
    end
  end
  # p [ranges, accepted_ranges.size, rejected_ranges.size]
  ranges = new_ranges
end

# p accepted_ranges
p accepted_ranges.map(&:size).sum
