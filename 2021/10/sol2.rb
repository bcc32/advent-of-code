input = File.open('input') do |f|
  # parse input
  f.lines.map(&:chomp)
end

line_scores = []
input.each do |line|
  invalid = false
  stack = []
  line_score = 0
  line.each_char do |char|
    if '(<{['.include?(char)
      stack << char
    elsif char == ')'
      if stack.last == '('
        stack.pop
      else
        invalid = true
      end
    elsif char == ']'
      if stack.last == '['
        stack.pop
      else
        invalid = true
      end
    elsif char == '}'
      if stack.last == '{'
        stack.pop
      else
        invalid = true
      end
    elsif char == '>'
      if stack.last == '<'
        stack.pop
      else
        invalid = true
      end
    end
  end
  next if invalid

  until stack.empty?
    case stack.last
    when '('
      line_score = 5 * line_score + 1
    when '['
      line_score = 5 * line_score + 2
    when '{'
      line_score = 5 * line_score + 3
    when '<'
      line_score = 5 * line_score + 4
      end
    stack.pop
  end

  line_scores << line_score
end

line_scores.sort!

p line_scores[line_scores.size / 2]
