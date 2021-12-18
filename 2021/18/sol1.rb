require 'json'

input = File.open('aoc.in') do |f|
  f.readlines.map { |line |
    arr = JSON.load(line)
  }

end

def add_on_left(expr, val)
  if expr.is_a?(Numeric)
    expr + val
  else
    left, right = expr
    [add_on_left(left, val), right]
  end
end

def add_on_right(expr, val)
  if expr.is_a?(Numeric)
    expr + val
  else
    left, right = expr
    [left, add_on_right(right, val)]
  end
end

def reduce_action(expr, depth)
  # p ['reduce_action called: ', expr]

  if expr.is_a?(Numeric)
    if expr >= 10
      return [:split, [expr / 2, (expr + 1) / 2]]
    end

    return nil
  end

  left, right = expr
  if depth >= 4
    # explode
    fail unless left.is_a?(Numeric)
    fail unless right.is_a?(Numeric)
    [:explode, left, 0, right]
  else
    left_action = reduce_action(left, depth + 1)
    right_action = reduce_action(right, depth + 1)

    case
    when left_action && left_action[0] == :explode
      l, mid, r = left_action[1,3]
      [:explode, l, [mid, add_on_left(right, r)], 0]
    when right_action && right_action[0] == :explode
      l, mid, r = right_action[1, 3]
      [:explode, 0, [add_on_right(left, l), mid], r]
    when left_action && left_action[0] == :split
      [:split, [left_action[1], right]]
    when right_action && right_action[0] == :split
      [:split, [left, right_action[1]]]
    end
  end
end

def reduce(expr)
  r = reduce_action(expr, 0)
  case
  when r.nil?
    expr
  when r[0] == :explode
    l, mid, r = r[1,3]
    reduce(mid)
  when r[0] == :split
    reduce(r[1])
  end
end

def add(expr1, expr2)
  reduce([expr1, expr2])
end

def magnitude(expr)
  if expr.is_a?(Numeric)
    expr
  else
    left, right = expr
    3 * magnitude(left) + 2 * magnitude(right)
  end
end

sum = input.reduce { |x, y| add(x, y) }
p magnitude(sum)
