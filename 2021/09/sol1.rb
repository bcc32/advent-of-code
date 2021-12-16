input = File.open('input') do |f|
  f.lines.map(&:chomp).map(&:chars)
end

risk = 0
input.each_with_index do |row, i|
  row.each_with_index do |cell, j|
    low = true
    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |di, dj|
      if (0...input.size).include?(i + di) \
        && (0...row.size).include?(j + dj) \
        && !(input[i + di][j + dj] > cell)
        low = false
      end
    end

    if low
      risk += cell.to_i + 1
    end
  end
end

p risk
