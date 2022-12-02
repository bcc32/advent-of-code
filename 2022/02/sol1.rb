z = 0

def shape_score(mine)
  case mine
  when 'X'
    1
  when 'Y'
    2
  when 'Z'
    3
  else
    raise
  end
end

def outcome(other, mine)
  case other+mine
  when 'AX', 'BY', 'CZ'
    :draw
  when 'AY', 'BZ', 'CX'
    :win
  else
    :lose
  end
end

def outcome_score(outcome)
  case outcome
  when :win
    6
  when :draw
    3
  when :lose
    0
  else
    raise
  end
end

IO.read('aoc.in').lines.each do |line|
  other, mine = line.split()
  z += shape_score(mine) + outcome_score(outcome(other, mine))
end

p z
