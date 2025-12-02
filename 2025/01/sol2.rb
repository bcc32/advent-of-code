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
  while dx >= 100
    dx -= 100
    count_zero += 1
  end
  while dx <= -100
    dx += 100
    count_zero += 1
  end

  # definitely at most one crossing now

  if dx > 0
    dx.times do
      dial += 1
      dial = 0 if dial == 100
      count_zero += 1 if dial == 0
    end
  else
    (-dx).times do
      dial -= 1
      dial += 100 if dial < 0
      count_zero += 1 if dial == 0
    end
  end
end

p count_zero
