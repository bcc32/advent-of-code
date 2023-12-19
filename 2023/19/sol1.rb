$stdout = File.open('aoc.out', 'w')
rules, parts = File.open('aoc.in') do |f|
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

  parts = parts.lines.map do |line|
    line =~ /\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}/
    [$1.to_i, $2.to_i, $3.to_i, $4.to_i]
  end

  [rules, parts]
end

accepted_parts = []
rejected_parts = []

parts.each do |part|
  cur_rule_name = 'in'
  loop do
    # p cur_rule_name
    rule = rules[cur_rule_name]
    target = nil
    rule.each do |step|
      if step.is_a?(Array)
        cmp_var, cmp_op, cmp_val, ptarget = step
        x, m, a, s = part
        raise unless cmp_var =~ /[xmas]/
        if eval("#{cmp_var}#{cmp_op}#{cmp_val}")
          target = ptarget
          break
        end
      else
        target = step
        break
      end
    end

    raise unless target

    if target == 'A'
      accepted_parts << part
      break
    elsif target == 'R'
      rejected_parts << part
      break
    else
      cur_rule_name = target
    end
  end
end

p (accepted_parts.map { |x,m,a,s| x+m+a+s }.sum)
