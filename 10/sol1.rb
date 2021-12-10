input = File.open('input') do |f|
  # parse input
  f.lines.map(&:chomp)
end

score = 0
input.each do |line|
  stack = []
  line.each_char do |char|
    if '(<{['.include?(char)
      stack << char
    elsif char == ')'
      if stack.last == '('
        stack.pop
      else
        score += 3
        break
      end
    elsif char == ']'
      if stack.last == '['
        stack.pop
      else
        score += 57
        break
      end
    elsif char == '}'
      if stack.last == '{'
        stack.pop
      else
        score += 1197
        break
      end
    elsif char == '>'
      if stack.last == '<'
        stack.pop
      else
        score += 25137
        break
      end
    end
  end
end

p score
