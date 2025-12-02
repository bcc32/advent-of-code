$stdout = File.open('aoc.out', 'w')
input = File.open('aoc.in') do |f|
  f.readlines.map do |line|
    if line =~ /^R(\d+)/
      $1.to_i
    elsif line =~ /^L(\d+)/
      - $1.to_i
    end
  end
end

dial = 50
count_zero = 0
input.each do |dx|
  dial += dx
  dial = dial % 100
  if dial == 0
    count_zero += 1
  end
end

p count_zero
