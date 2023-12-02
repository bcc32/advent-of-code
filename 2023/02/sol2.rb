$stdout = File.open('aoc.out', 'w')

sum = 0

File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line =~ /Game (\d+): (.*)/
    id = $1.to_i
    rounds = $2
    min_blue = min_green = min_red = 0

    rounds.split(/; /).map do |round|
      round.split(/, /).map do |color|
        color =~ /(\d+) (\w+)/
        c = $2
        count = $1.to_i
        min_blue = [min_blue, count].max if c == 'blue'
        min_green = [min_green, count].max if c == 'green'
        min_red = [min_red, count].max if c == 'red'
      end
    end

    power = min_blue * min_green * min_red
    sum += power
  end
end

p sum
