$stdout = File.open('aoc.out', 'w')

red = 12
green = 13
blue = 14

sum = 0
File.open('aoc.in') do |f|
  f.readlines.map do |line|
    line =~ /Game (\d+): (.*)/
    id = $1.to_i
    rounds = $2
    bad = false
    rounds.split(/; /).map do |round|
      round.split(/, /).map do |color|
        color =~ /(\d+) (\w+)/
        c = $2
        count = $1.to_i
        bad = true if c == 'blue' && count > blue
        bad = true if c == 'green' && count > green
        bad = true if c == 'red' && count > red
      end
    end

    sum += id if not bad
  end
end

p sum
