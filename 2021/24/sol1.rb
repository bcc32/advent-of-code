def calc(input)
  vars = [0, 0, 0, 0]

  IO.read('aoc.in').lines.each do |line|
    op, a, b = line.split

    a = a.ord - 'w'.ord

    if op != 'inp'
      if b =~ /\d/
        val_b = b.to_i
      else
        val_b = vars[b.ord - 'w'.ord]
      end
    end

    case op
    when 'inp'
      vars[a] = input.shift.to_i
    when 'add'
      vars[a] += val_b
    when 'mul'
      vars[a] *= val_b
    when 'div'
      vars[a] /= val_b
    when 'mod'
      vars[a] = vars[a] % val_b
    when 'eql'
      vars[a] = (vars[a] == val_b) ? 1 : 0
    end
  end

  vars.last == 0
end

(99_999_999_999_999).downto(1) do |n|
  p n
  next if n.to_s =~ /0/
  if calc(n.to_s.chars)
    puts n
    break
  end
end
